-module(inet_ssh_backend).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %gen_server
-record(state, { ssh, chan }).
-record(chan, { recv_list = false, packet = 0, buffer = <<>>, eof_write = false, eof_read = false, sleepers = [] }).

-define(PACKET_SIZE, 32768).
-define(WINDOW_SIZE, 4*?PACKET_SIZE).

init([Address]) ->
	undefined == application:get_application(ssh) andalso ssh:start(),
	{Host, Opts} = case string:split(Address, "@") of [U,H] -> {H, [{user, U}] }; [H] -> {H, []} end,
	{ok, SSH} = ssh:connect(Host, 22, Opts),
	{ok, #state{ ssh = SSH, chan = #{} }}.

handle_call({connect, Port, Opts}, _From, State = #state{ chan = Channels, ssh = SSH }) ->
	H = <<"localhost">>, L=byte_size(H), P = 10000 + rand:uniform(55000),
	M = <<L:32/big, H/binary, Port:32/big, L:32/big, H/binary, P:32/big>>,
	{open, Ref} = ssh_connection_handler:open_channel(SSH, "direct-tcpip", M, ?WINDOW_SIZE, ?PACKET_SIZE, infinity),
	%io:fwrite(standard_error, "open channel ~p (port ~p)~n", [Ref,Port]),
	{reply, Ref, State#state{ chan = maps:put(Ref, set_options(#chan{}, Opts), Channels) } };
handle_call(ll, _From, State = #state{ ssh = SSH }) ->
	[{socket,Sock}] = ssh:connection_info(SSH,[socket]),
	{reply, Sock, State};
handle_call({recv, Ref, Length}, From, State) ->
	self() ! {wait, From, Ref, Length},
	{noreply, State}.

handle_cast({close, Ref, write}, State = #state{ ssh = SSH, chan = Channels }) ->
	C = #chan{ eof_write = A, eof_read = B } = maps:get(Ref, Channels),
	if
		byte_size(C#chan.buffer) == 0, B -> handle_cast({close, Ref, read_write}, State);
		true -> 
			A orelse ssh_connection:send_eof(SSH, Ref),
			{noreply, State#state{ chan = maps:put(Ref, C#chan{ eof_write = true }, Channels) } }
	end;
handle_cast({close, Ref, read}, State = #state{ chan = Channels }) ->
	C = #chan{ eof_write = A } = maps:get(Ref, Channels),
	if
		A -> handle_cast({close, Ref, read_write}, State);
		true -> {noreply, State#state{ chan = maps:put(Ref, C#chan{ eof_read = true }, Channels) } }
	end;
handle_cast({close, Ref, read_write}, State = #state{ ssh = SSH, chan = Channels }) ->
	ssh_connection:close(SSH, Ref),
	{noreply, State#state{ chan = maps:remove(Ref, Channels) } };
handle_cast({set_options, Ref, Opts}, State) ->
	{noreply, with_chan(fun(C) -> set_options(C, Opts) end, Ref, State)};
handle_cast({send, Ref, Data, _Opts}, State = #state{ ssh = SSH, chan = Channels }) ->
	#chan{ packet = P } = maps:get(Ref, Channels),
	Packet = encode_packet(P, iolist_to_binary(Data)),
	%io:fwrite(standard_error, "~p send ~p~n", [Ref,Packet]),
	ssh_connection:send(SSH, Ref, Packet), {noreply, State};
handle_cast(_, State) -> {noreply, State}.

set_options(C = #chan{}, Opts) ->
	P = proplists:get_value(packet, Opts, 0),
	LM = lists:member(list, Opts),
	C#chan{ packet = P, recv_list = LM }.

encode_packet(1, D) -> DL = byte_size(D), <<DL, D/binary>>;
encode_packet(2, D) -> DL = byte_size(D), <<DL:16/big, D/binary>>;
encode_packet(4, D) -> DL = byte_size(D), <<DL:32/big, D/binary>>;
encode_packet(_, D) -> D. % raw, 0

with_chan(F, Ref, State = #state{ chan = Channels }) ->
	case maps:get(Ref, Channels, undefined) of undefined -> State; C -> State#state{ chan = maps:put(Ref, F(C), Channels) } end.

with_chan_data(F, Ref, State) ->
	with_chan(fun(C = #chan{ sleepers = W }) ->
		P = self(), lists:foreach(fun(X) -> P ! X end, W),
		F(C#chan{ sleepers = [] })
	end, Ref, State).

xlate(#chan{ recv_list = true }, X) -> binary_to_list(X);
xlate(#chan{ recv_list = false }, X) -> X.

handle_info({ssh_cm, SSH, {data, Ref, _, Data}}, State = #state{ ssh = SSH }) ->
	%io:fwrite("~p data ~p~n",[Ref, Data]),
	{noreply, with_chan_data(fun(C = #chan{ buffer = B }) -> C#chan{ buffer = <<B/binary, Data/binary>> } end, Ref, State)};
handle_info({ssh_cm, SSH, {eof, Ref}}, State = #state{ ssh = SSH }) ->
	%io:fwrite("~p eof~n",[Ref]),
	{noreply, with_chan_data(fun(C) -> C#chan{ eof_read = true } end, Ref, State)};
handle_info(X = {wait,From,Ref,Length}, State = #state{ chan = Channels }) ->
	C = #chan{ buffer = B, sleepers = W, packet = P } = maps:get(Ref, Channels),
	C2 = if
		P =/= 0, C#chan.eof_read ->
			gen_server:reply(From, {error, closed}),
			C;
		P =/= 0 ->
			case erlang:decode_packet(P, B, []) of
				{ok, Left, Right} -> gen_server:reply(From, {ok, xlate(C, Left)}), C#chan{ buffer = Right };
				{more, _Needmore} -> C#chan{ sleepers = [X|W] };
				E = {error, _Reason} -> gen_server:reply(From, E), C
			end;
		byte_size(B) >= Length ->
			Left = binary:part(B, 0, Length), Right = binary:part(B, Length, byte_size(B) - Length),
			gen_server:reply(From, {ok, xlate(C, Left)}),
			C#chan{ buffer = Right };
		C#chan.eof_read, byte_size(B) > 0 ->
			gen_server:reply(From, {ok, xlate(C, B)}),
			C#chan{ buffer = <<>> };
		C#chan.eof_read ->
			gen_server:reply(From, {error, closed}),
			C;
		true -> 
			C#chan{ sleepers = [X|W] }
	end,
	{noreply, State#state{ chan = maps:put(Ref, C2, Channels) } };
handle_info(_Else, State) ->
	{noreply, State}.

code_change(_Old, State, _Extra) -> {ok, State}.
terminate(_Reason, #state{ ssh = SSH, chan = Channels }) -> 
	lists:foreach(fun
		({Ref, #chan{ eof_read = A, eof_write = B }}) when A == false; B == false -> ssh_connection:close(SSH, Ref);
		(_) -> ok
	end, maps:to_list(Channels)),
	normal.
