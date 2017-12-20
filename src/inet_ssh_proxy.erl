-module(inet_ssh_proxy).
-behaviour(gen_server).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/net_address.hrl").

-export([setup/3, setup/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %gen_server
-record(state, { ssh, sock, cr = #{}, rc = #{} }).

-define(PACKET_SIZE, 32768).
-define(WINDOW_SIZE, 4*?PACKET_SIZE).

setup(Address, Port, Opts) -> setup(Address, Port, Opts, infinity).
setup(Address, Port, Opts, Timeout) ->
	T = binary_to_atom(iolist_to_binary(io_lib:format("~p", [{?MODULE, Address}])),utf8),
	Pid = case gen_server:start_link({local, T}, ?MODULE, [Address], [{timeout,Timeout}]) of
		{error,{already_started,P}} -> P;
		{ok, P} -> P
	end,
	{ok, LS} = gen_tcp:listen(0, [{ip,{127,0,0,1}},{backlog,1}]),
	{ok, {{127,0,0,1}, LP}} = inet:sockname(LS),
	Pid ! {setup, LP, Port, Opts},
	{ok, Sock} = gen_tcp:accept(LS, Timeout),
	ok = gen_tcp:close(LS),
	ok = inet:setopts(Sock, Opts),
	{ok, SSHSocket} = gen_server:call(Pid, getll),
	{ok, InetAddress} = inet:peername(SSHSocket),
	{ok, Sock, InetAddress}.

callback(PeerName, Fingerprint) ->
	io:fwrite(standard_error, "unknown host ~p fp = ~p~n", [PeerName, Fingerprint]),
	false.


init([Address]) ->
	undefined == application:get_application(ssh) andalso ssh:start(),
	BaseOpts = [{user_interaction,false},{silently_accept_hosts,fun callback/2}],
	{HostPart, Opts} = case string:split(Address, "@") of [U,H] -> {H, [{user, U}|BaseOpts] }; [H] -> {H, BaseOpts} end,
	{Host, Port} = case string:split(HostPart, ":") of [A,P] -> {A, list_to_integer(P)}; [A] -> {A, 22} end,
	{ok,#hostent{ h_addr_list = HL }} = inet:gethostbyname(Host),
	{ok, SSH} = lists:foldr(fun(_IP, S = {ok, _SSH}) -> S; (IP, _) -> ssh:connect(IP, Port, Opts) end, {error, enoent}, HL),
	[{socket,Sock}] = ssh:connection_info(SSH,[socket]),
	{ok, #state{ ssh = SSH, sock = Sock }}.

handle_call(getll, _From, State) -> {reply, {ok, State#state.sock}, State};
handle_call(_Message, _From, State) -> {reply, nyi, State}.
handle_cast(_Message, State) -> {noreply, State}.

handle_info({setup, LocalPort, RemotePort, _Opts}, State = #state{ ssh = SSH }) ->
	H = <<"localhost">>, L=byte_size(H), P = 10000 + rand:uniform(55000),
	M = <<L:32/big, H/binary, RemotePort:32/big, L:32/big, H/binary, P:32/big>>,
	{open, Ref} = ssh_connection_handler:open_channel(SSH, "direct-tcpip", M, ?WINDOW_SIZE, ?PACKET_SIZE, infinity),
	{ok, Conn} = gen_tcp:connect({127,0,0,1},LocalPort,[binary,{active,true}]),
	{noreply, State#state{ cr = maps:put(Conn, Ref, State#state.cr), rc = maps:put(Ref, Conn, State#state.rc) } };
handle_info({tcp, Conn, Data}, State) ->
	%io:fwrite(standard_error, "tcp from erlang to ssh ~p~n",[Data]),
	Ref = maps:get(Conn, State#state.cr),
	ssh_connection:send(State#state.ssh, Ref, Data),
	{noreply, State};
handle_info({tcp_closed, Conn}, State) ->
	%io:fwrite(standard_error, "tcp closed by erlang~n",[]),
	Ref = maps:get(Conn, State#state.cr),
	ssh_connection:send_eof(State#state.ssh, Ref),
	{noreply, State#state{ cr = maps:remove(Conn, State#state.cr) }};
handle_info({tcp_error, Conn, _Reason}, State) ->
	%io:fwrite(standard_error, "tcp error from erlang: ~p~n",[Reason]),
	Ref = maps:get(Conn, State#state.cr),
	ssh_connection:close(State#state.ssh, Ref),
	{noreply, State#state{ cr = maps:remove(Conn, State#state.cr), rc = maps:remove(Ref, State#state.rc) } };
handle_info({ssh_cm, SSH, {data, Ref, _, Data}}, State = #state{ ssh = SSH }) ->
	%io:fwrite(standard_error, "ssh data: ~p~n",[Data]),
	Conn = maps:get(Ref, State#state.rc),
	ok = gen_tcp:send(Conn, Data),
	{noreply, State};
handle_info({ssh_cm, SSH, {eof, Ref}}, State = #state{ ssh = SSH }) ->
	%io:fwrite(standard_error, "ssh eof~n",[]),
	Conn = maps:get(Ref, State#state.rc),
	gen_tcp:shutdown(Conn, write),
	{noreply, State#state{ rc = maps:remove(Ref, State#state.rc) }};
handle_info({ssh_cm, SSH, {closed, Ref}}, State = #state{ ssh = SSH }) ->
	%io:fwrite(standard_error, "ssh eof~n",[]),
	Conn = maps:get(Ref, State#state.rc, undefined),
	Conn =/= undefined andalso (catch gen_tcp:shutdown(Conn, read)),
	{noreply, State#state{ cr = maps:remove(Conn, State#state.cr), rc = maps:remove(Ref, State#state.rc) }};
handle_info(Else, State) ->
	io:fwrite(standard_error,"inet_ssh_proxy got ~p~n", [Else]),
	{noreply, State}.

code_change(_Old, State, _Extra) -> {ok, State}.
terminate(_Reason, State = #state{ ssh = SSH }) -> 
	lists:foreach(fun (Conn) -> gen_tcp:close(Conn) end, maps:values(State#state.rc)),
	lists:foreach(fun (Ref) -> ssh_connection:close(SSH, Ref) end, maps:values(State#state.cr)),
	normal.

