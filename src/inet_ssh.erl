-module(inet_ssh).

-export([getserv/1, getaddr/1, getaddr/2, getaddrs/1, getaddrs/2]).
-export([connect/3, connect/4, listen/2, accept/1, accept/2, close/1]).
-export([send/2, send/3, recv/2, recv/3, shutdown/2]).

-record(inet_ssh, { pid, ref }).

getserv(X) -> inet_tcp:getserv(X).
getaddr(X) -> inet_tcp:getaddr(X).    getaddr(X,Timer) -> inet_tcp:getaddr(X,Timer).
getaddrs(X) -> inet_tcp:getaddrs(X).  getaddrs(X,Timer) -> inet_tcp:getaddrs(X,Timer).

connect(Address, Port, Opts) -> connect(Address, Port, Opts, infinity).
connect(Address, Port, Opts, Timeout) ->
	T = binary_to_atom(iolist_to_binary(io_lib:format("~p", [{inet_ssh_backend, Address}])),utf8),
	Pid = case gen_server:start_link({local, T}, inet_ssh_backend, [Address], [{timeout,Timeout}]) of
		{error,{already_started,P}} -> P;
		{ok, P} -> P
	end,
	Ref = gen_server:call(Pid, {connect, Port, Opts}, Timeout),
	{ok, #inet_ssh{ pid = Pid, ref = Ref }}.

listen(Port, Opts) -> inet_tcp:listen(Port, [{bind_to_device,{127,0,0,1}} | Opts]).
accept(X) -> inet_tcp:accept(X).  accept(X,T) -> inet_tcp:accept(X,T).

close(X) -> shutdown(X, read_write).

send(Socket, Packet) -> send(Socket, Packet, []).
send(#inet_ssh{ pid = Pid, ref = Ref }, Packet, Opts) -> gen_server:cast(Pid, {send, Ref, Packet, Opts}).

recv(Socket, Length) -> recv(Socket, Length, infinity).
recv(#inet_ssh{ pid = Pid, ref = Ref }, Length, Timeout) -> gen_server:call(Pid, {recv, Ref, Length}, Timeout).

shutdown(#inet_ssh{ pid = Pid, ref = Ref }, How) -> gen_server:cast(Pid, {close, Ref, How}), ok.
