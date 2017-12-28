-module(inet_ssh_dist).
-define(dist_debug,true).

-export([childspecs/0, listen/1, accept/1, accept_connection/5, setup/5, close/1, select/1, is_node_name/1]).

-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").
-include_lib("kernel/include/net_address.hrl").

setup(Node, Type, MyNode, LongOrShortNames,SetupTime) ->
	[U1,H1] = string:split(atom_to_list(Node),"@"),
	[_U2,H2] = string:split(atom_to_list(MyNode),"@"),
	if
		H1 == H2 -> inet_tcp_dist:setup(Node, Type, MyNode, LongOrShortNames, SetupTime); % bypass ssh
		true -> 
			Kernel = self(), 
			H1Pre = case init:get_argument(ssh_default_user) of {ok, [[A]]} -> A ++ "@"; _ -> "" end,
			spawn_link(fun () ->
				process_flag(priority, max),
				do_setup(Node, Kernel, U1, H1Pre ++ H1, H1, Type, MyNode, LongOrShortNames,SetupTime)
			end)
	end.

pre_nodeup({Socket,Pid}) ->
	ok = gen_server:call(Pid, {packet_mode, Socket, 4}),
	gen_server:call(Pid, {pre_nodeup, Socket}),
	inet:setopts(Socket, [{active,false}]),
	ok.

post_nodeup({Socket,Pid}) ->
	ok = gen_server:call(Pid, {packet_mode, Socket, 4}),
	gen_server:call(Pid, {post_nodeup, Socket}),
	inet:setopts(Socket, [{deliver,port},{active,true}]),
	ok.

mf_tick({Socket,_Pid}) -> inet_tcp_dist:tick(inet_tcp, Socket).
mf_getstat({Socket,_Pid}) -> inet_tcp_dist:getstat(Socket).

my_recv({Socket,_Pid}, Count, Timeout) -> gen_tcp:recv(Socket, Count, Timeout).
my_send({Socket,_Pid}, Data) -> gen_tcp:send(Socket, Data).
my_getll({Socket, _Pid}) -> inet:getll(Socket).

do_setup(Node, Kernel, RequestedName, SSH_Host, Hostname, Type, MyNode, _LongOrShortNames,SetupTime) ->
	Timer = dist_util:start_timer(SetupTime),
	{port, UsePort, Version} = inet_ssh_epms:remote_port(RequestedName, SSH_Host),
	dist_util:reset_timer(Timer),
	{ok, Socket, {IP, _Port}, Pid} = inet_ssh_proxy:setup("direct-tcpip", SSH_Host, UsePort, [list,{active,false}], infinity),
	ok = gen_server:call(Pid, {packet_mode, Socket, 2}),
	%ErlEpmd = net_kernel:epmd_module(), ErlEpmd:register_node(Node, Port, inet_tcp),
	dist_util:handshake_we_started(#hs_data{
		kernel_pid = Kernel,
		other_node = Node,
		this_node = MyNode,
		socket = {Socket, Pid},
		timer = Timer,
		this_flags = 0,
		other_version = Version,
		f_send = fun my_send/2,
		f_recv = fun my_recv/3,
		f_setopts_pre_nodeup = fun pre_nodeup/1,
		f_setopts_post_nodeup = fun post_nodeup/1,
		f_getll = fun my_getll/1,
		f_address = fun(_,_) -> #net_address{ host = Hostname, address = {IP,UsePort}, family = inet, protocol = proxy } end,
		mf_tick = fun mf_tick/1,
		mf_getstat = fun mf_getstat/1,
		request_type = Type
	}).


childspecs() -> {ok, []}.
select(Node) -> inet_tcp_dist:select(Node).
listen(Name) -> erlang:display({being_asked_to_listen,Name}), inet_tcp_dist:listen(Name).

accept(Listen) -> erlang:display({being_asked_to_accept,Listen}), inet_tcp_dist:gen_accept(inet_tcp, Listen).
close(Socket) -> gen_tcp:close(Socket).

is_node_name(Node) -> inet_tcp_dist:is_node_name(Node).

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
	inet_tcp_dist:gen_accept_connection(inet_tcp, AcceptPid, Socket, MyNode, Allowed, SetupTime).
