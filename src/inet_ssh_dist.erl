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

pre_nodeup(Socket) -> inet:setopts(Socket, [{active,false}, {packet,4}]), ok.
post_nodeup(Socket) -> inet:setopts(Socket, [{deliver,port},{active,true}, {packet,4}]), ok.

mf_tick(Socket) -> gen_tcp:send(Socket, []).
mf_getstat(Socket) -> inet_tcp_dist:getstat(Socket).

do_setup(Node, Kernel, RequestedName, SSH, Hostname, Type, MyNode, _LongOrShortNames,SetupTime) ->
	Timer = dist_util:start_timer(SetupTime),
	{port, UsePort, Version} = inet_ssh_epms:port_please(RequestedName, SSH),
	dist_util:reset_timer(Timer),
	{ok, Socket, {IP, _}} = inet_ssh_proxy:setup(SSH, UsePort, [list,{active,false},{packet, 2}]),
	dist_util:handshake_we_started(#hs_data{
		kernel_pid = Kernel,
		other_node = Node,
		this_node = MyNode,
		socket = Socket,
		timer = Timer,
		this_flags = 0,
		other_version = Version,
		f_send = fun gen_tcp:send/2,
		f_recv = fun gen_tcp:recv/3,
		f_setopts_pre_nodeup = fun pre_nodeup/1,
		f_setopts_post_nodeup = fun post_nodeup/1,
		f_getll = fun inet:getll/1,
		f_address = fun(_,_) -> #net_address{ host = Hostname, address = {IP,UsePort}, family = inet, protocol = proxy } end,
		mf_tick = fun mf_tick/1,
		mf_getstat = fun mf_getstat/1,
		request_type = Type
	}).


childspecs() -> inet_tcp_dist:childspecs().
select(Node) when is_atom(Node) -> length(string:split(atom_to_list(Node), "@")) == 2; select(_) -> false.
listen(Name) -> inet_tcp_dist:listen(Name).

accept(Listen) -> inet_tcp_dist:gen_accept(inet_tcp, Listen).
close(Socket) -> gen_tcp:close(Socket).

is_node_name(Node) when is_atom(Node) -> select(Node);
is_node_name(_Node) -> false.

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
	inet_tcp_dist:gen_accept_connection(inet_tcp, AcceptPid, Socket, MyNode, Allowed, SetupTime).
