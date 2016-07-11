-module(chat_noir_server).

-export([start/0, loop/2, server_test/0]).

start()->
	tcp_server:start(?MODULE, 8888, {?MODULE, loop}, []).

do_recv(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			binary_to_list(Data);
		{error, closed} -> 
			ok
	end.

parse_message(SomeString) ->
	{M, B} = lists:splitwith(fun(S) -> [S] =/= "{" end, SomeString).

loop(Socket, Clients) ->
	Message = do_recv(Socket),
	{Method, Body} = parse_message(Message),
	case Method of
		"NEWCLIENT" ->
			gen_tcp:send(Socket, "New Client Arrived! \n"),
			loop(Socket, Clients),
			ok;
		_ ->
			gen_tcp:send(Socket, "Error\n"),
			gen_tcp:close(Socket)
	end.

server_test() ->
	"NEWCLIENT", _ = parse_message("NEWCLIENT{NAME=douglas;}").
