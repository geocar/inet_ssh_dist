-module(inet_ssh_epms).
-export([remote_port/2]).

remote_port(NodeName, SSH_Host) ->
	{ok, Sock, _, _} = inet_ssh_proxy:setup("direct-tcpip", SSH_Host, 4369, [binary,{active,false}], infinity),
	BinaryName = iolist_to_binary(NodeName), N = byte_size(BinaryName), L = N + 1,
	ok = gen_tcp:send(Sock, <<L:16/big,122,BinaryName/binary>>), %PORT_PLEASE2_REQ
	{ok, <<119, 0, UsePort:16/big, _NodeType, _Protocol, Version:16/big>>} = gen_tcp:recv(Sock, 8),
	gen_tcp:close(Sock),
	{port, UsePort, Version}.

