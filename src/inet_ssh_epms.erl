-module(inet_ssh_epms).
-export([port_please/2]).

port_please(NodeName, SSH_Host) ->
	{ok, Sock, _InetAddress} = inet_ssh_proxy:setup(SSH_Host, 4369, [binary, {active, false}]),
  BinaryName = iolist_to_binary(NodeName), L = byte_size(BinaryName) + 1,
	gen_tcp:send(Sock, <<L:16/big,122,BinaryName/binary>>), %PORT_PLEASE2_REQ
  {ok, <<119, 0, UsePort:16/big, _NodeType, _Protocol, Version:16/big>>} = gen_tcp:recv(Sock, 8),
	gen_tcp:close(Sock),
  {port, UsePort, Version}.

