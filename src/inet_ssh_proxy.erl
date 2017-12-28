-module(inet_ssh_proxy).
-behaviour(gen_server).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/net_address.hrl").

-export([setup/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %gen_server
-record(state, { waiting = [], connecting, host, port, opts, ssh, sock, cr, rc, uc, ur, dist }).

-define(PACKET_SIZE, 2147483648). % rfc4254 says this is the biggest block we can receive,
-define(WINDOW_SIZE, 2147483648). % and erlang doesn't support bigger than 2G 

opts(K) -> case application:get_env(kernel, K) of {ok, ConnectOpts} -> ConnectOpts; _ -> [] end.

setup(Type, Address, Port, Opts, Timeout) ->
	T = binary_to_atom(iolist_to_binary(io_lib:format("inet_ssh_proxy: ~p", [Address])),utf8),
	Pid = case gen_server:start({local, T}, ?MODULE, [Address], [{timeout,Timeout}]) of
		{error,{already_started,P}} -> P;
		{ok, P} -> P
	end,
	{ok, LS} = gen_tcp:listen(0, [{ip,loopback},{active,false},{backlog,1}]),
	{ok, InetAddress = {{127,0,0,1}, LP}} = inet:sockname(LS),
	MyRef = make_ref(),
	Pid ! {setup, self(), MyRef, Type, LP, Port, Opts},
	{ok, Sock} = gen_tcp:accept(LS, Timeout),
	ok = gen_tcp:close(LS),
	receive
		{Pid, MyRef, Ref, Conn} ->
			ok = gen_server:call(Pid, {'link-finish', Sock, Ref, Conn}),
			ok = inet:setopts(Sock, Opts)
	end,
	{ok, Sock, InetAddress, Pid}.

callback(PeerName, Fingerprint) ->
	io:fwrite(standard_error, "unknown host ~p fp = ~p~n", [PeerName, Fingerprint]),
	false.


init([Address]) ->
	undefined == application:get_application(ssh) andalso ssh:start(),
	BaseOpts = [{user_interaction,false},{silently_accept_hosts,fun callback/2}],
	{HostPart, Opts} = case string:split(Address, "@") of [U,H] -> {H, [{user, U}|BaseOpts] }; [H] -> {H, BaseOpts} end,
	{Host, Port} = case string:split(HostPart, ":") of [A,P] -> {A, list_to_integer(P)}; [A] -> {A, 22} end,
	self() ! connect,
	{ok, #state{ host = Host, port = Port, opts = Opts,
			dist = gb_trees:empty(),
			ur = gb_trees:empty(), uc = gb_trees:empty(),
			cr = gb_trees:empty(), rc = gb_trees:empty() }}.

handle_call({'link-finish', Sock, Ref, Conn}, _From, State = #state{ ur = UR, uc = UC }) ->
	inet:setopts(Conn, [binary,{active,true}]),
	{reply, ok, State#state{ ur = gb_trees:insert(Sock, Ref, UR), uc = gb_trees:insert(Sock, Conn, UC) }};
handle_call({packet_mode, Sock, Size}, _From, State = #state{ dist = Dist, ur = UR, uc = UC }) ->
	Ref = gb_trees:get(Sock, UR), Conn = gb_trees:get(Sock, UC),
	inet:setopts(Conn, [binary,{packet,Size}]), inet:setopts(Sock, [{packet,Size}]),
	UseBuffer = case gb_trees:lookup(Ref, Dist) of
		{value, {_, OldBuffer = <<>>}} -> OldBuffer;
		{value, {_OldSize, OldBuffer}} -> OldBuffer; % unclear...
		_ -> <<>>
	end,
	{reply, ok, State#state{ dist = gb_trees:enter(Ref,{Size, UseBuffer},Dist) }};
handle_call(_Message, _From, State) -> {reply, nyi, State}.
handle_cast(_Message, State) -> {noreply, State}.

handle_info(connect, State = #state{ host = Host, port = Port, opts = Opts }) ->
	Pid = self(),
	spawn_link(fun() ->
		{ok,#hostent{ h_addr_list = HL }} = inet:gethostbyname(Host),
		UseOpts = opts(inet_dist_connect_options) ++ opts(inet_ssh_dist_connect_options) ++ Opts ++ [
				%{ssh_msg_debug_fun, fun(_,_,Msg,_)-> io:format("DEBUG: ~p~n", [Msg]) end}
		],
		{ok, SSH} = lists:foldr(fun(_IP, S = {ok, _SSH}) -> S; (IP, _) -> ssh:connect(IP, Port, UseOpts) end, {error, enoent}, HL),
		[{socket,Sock}] = ssh:connection_info(SSH,[socket]),
		Pid ! {connect_finish, SSH, Sock}
	end),
	{noreply, State#state{ connecting = true, host = Host, port = Port, opts = Opts }};
handle_info({connect_finish, SSH, Sock}, State = #state{ connecting = true, waiting = Waiting }) ->
	Pid = self(), lists:foreach(fun (W) -> Pid ! W end, Waiting),
	{noreply, State#state{ connecting = false, ssh = SSH, sock = Sock, waiting = [] } };
handle_info(X, State = #state{ connecting = true, waiting = Waiting }) ->
	{noreply, State#state{ waiting = [X|Waiting] }};
handle_info({setup, Pid, MyRef, Type, LocalPort, RemotePort, Opts}, State = #state{ ssh = SSH }) ->
	%io:fwrite(standard_error, "~p setup from erlang to ssh ~p~n",[SSH,{LocalPort,RemotePort}]),
	H = <<"localhost">>, L=byte_size(H), P = 10000 + rand:uniform(55000),
	M = <<L:32/big, H/binary, RemotePort:32/big, L:32/big, H/binary, P:32/big>>,
	case ssh_connection_handler:open_channel(SSH, Type, M, ?WINDOW_SIZE, ?PACKET_SIZE, infinity) of
		{open, Ref} ->
			self() ! {setup_finish, Pid, MyRef, Ref, LocalPort, SSH};
		{error,closed} ->
			self() ! connect,
			self() ! {setup, Pid, MyRef, LocalPort, RemotePort, Opts}
	end,
	{noreply, State};
handle_info({setup_finish, Pid, MyRef, Ref, LocalPort, SSH}, State = #state{ cr = CR, rc = RC, ssh = SSH }) ->
	{ok, Conn} = gen_tcp:connect({127,0,0,1},LocalPort,[{active,false}]),
	Pid ! {self(), MyRef, Ref, Conn},
	%io:fwrite(standard_error, "~p setup finished ~p~n",[SSH,{Ref,Conn}]),
	{noreply, State#state{ cr = gb_trees:insert(Conn, Ref, CR), rc = gb_trees:insert(Ref, Conn, RC) }};
handle_info({tcp, Conn, Data}, State = #state{ cr = CR, ssh = SSH, dist = Dist }) when is_binary(Data) ->
	%io:fwrite(standard_error, "~p tcp from erlang to ssh ~p~n",[SSH,Data]),
	Ref = gb_trees:get(Conn, CR),
	Data2 = case gb_trees:lookup(Ref, Dist) of
		{value, {4, _}} -> L = byte_size(Data), <<L:32/big, Data/binary>>;
		{value, {2, _}} -> L = byte_size(Data), <<L:16/big, Data/binary>>;
		{value, _} -> exit(oops);
		_ -> Data
	end,
	%io:fwrite(standard_error, "~p tcp ~p data from erlang: ~p is conn ~p~n",[SSH,byte_size(Data),Ref,Conn]),
	ssh_connection:send(SSH, Ref, Data2),
	{noreply, State};
handle_info({tcp_closed, Conn}, State = #state{ cr = CR, ssh = SSH }) ->
	Ref = gb_trees:get(Conn, CR),
	%io:fwrite(standard_error, "~p tcp closed by erlang ref was ~p~n",[SSH,Ref]),
	ssh_connection:send_eof(SSH, Ref),
	{noreply, State};
handle_info({tcp_error, Conn, _Reason}, State = #state{ cr = CR, ssh = SSH }) ->
	%io:fwrite(standard_error, "~p tcp error from erlang: ~p~n",[SSH,Reason]),
	Ref = gb_trees:get(Conn, CR),
	ssh_connection:close(SSH, Ref),
	{noreply, State};
handle_info({ssh_cm, SSH, {data, Ref, _, Data}}, State = #state{ dist = Dist, ssh = SSH, rc = RC }) ->
	%io:fwrite(standard_error, "~p ssh data: ~p~n",[SSH,Data]),
	Conn = gb_trees:get(Ref, RC),
	%io:fwrite(standard_error, "~p ssh ~p data to erlang: ~p is conn ~p~n",[SSH,byte_size(Data),Ref,Conn]),
	Dist2 = case gb_trees:lookup(Ref, Dist) of
		{value, {Size, OldData}} ->
			Data2 = <<OldData/binary, Data/binary>>,
			gb_trees:enter(Ref, handle_decode(Size, Conn, Data2), Dist);
		_ ->
			gen_tcp:send(Conn, Data),
			Dist
	end,
	{noreply, State#state{ dist = Dist2 }};
handle_info({ssh_cm, SSH, {eof, Ref}}, State = #state{ ssh = SSH, rc = RC }) ->
	Conn = gb_trees:get(Ref, RC),
	%io:fwrite(standard_error, "~p ssh eof ~p~n",[SSH,{Ref,Conn}]),
	(catch gen_tcp:shutdown(Conn,read)),
	{noreply, State};
handle_info({ssh_cm, SSH, {closed, Ref}}, State = #state{ ssh = SSH, cr = CR, rc = RC }) ->
	Conn = gb_trees:get(Ref, RC),
	%io:fwrite(standard_error, "~p ssh CLOSED ~p~n",[SSH,{Ref,Conn}]),
	(catch gen_tcp:close(Conn)),
	{noreply, State#state{ cr = gb_trees:delete(Conn, CR), rc = gb_trees:delete(Ref, RC) }};
handle_info(Else, State) ->
	io:fwrite(standard_error,"inet_ssh_proxy got ~p~n", [Else]),
	{noreply, State}.

code_change(_Old, State, _Extra) -> {ok, State}.
terminate(_Reason, #state{ ssh = SSH, cr = CR, rc = RC }) -> 
	lists:foreach(fun (Conn) -> gen_tcp:close(Conn) end, gb_trees:values(RC)),
	lists:foreach(fun (Ref) -> ssh_connection:close(SSH, Ref) end, gb_trees:values(CR)),
	normal.

handle_decode(Size, _Conn, Data = <<>>) -> {Size, Data};
handle_decode(Size, Conn, Data) ->
	case erlang:decode_packet(Size, Data, []) of
		{ok, Packet, Rest} ->
			gen_tcp:send(Conn, Packet),
			handle_decode(Size, Conn, Rest);
		{more, _More} ->
			%io:fwrite(standard_error, "(need ~p bytes; ~p more)~n", [More+byte_size(Data), More]),
			{Size, Data}
	end.
