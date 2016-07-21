-module(chat_noir_server).

-export([start/0, loop/2, server_test/0]).

-record(client, {
		name,
		info,
		socket
	}).

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

parse_client(SomeString, Socket) ->
	#client{name="Douglas", info="ada", socket=Socket}.

parse_message(SomeString, Socket) ->
	{10, 11}.

public_message(User, Message, Clients)->
	ok.

loop(Socket, Clients) ->
	Message = do_recv(Socket),
	io:format("New Client ~p ", [Clients]),
	{Method, Body} = parse_message(Message),
	case Method of
		"NEWCLIENT" ->
			gen_tcp:send(Socket, "New Client Arrived! \n"),
			NewClient = parse_client(Body, Socket),
			io:format("New Client ~p ", [NewClient]),
			loop(Socket, [NewClient | Clients]);
		"PUBLIC" ->
			User, ChatMsg = parse_message(Body, Socket),
			public_message(User, ChatMsg, Clients),
			loop(Socket, Clients);
		_ ->
			gen_tcp:send(Socket, "Error\n"),
			gen_tcp:close(Socket)
	end.

server_test() ->
	"NEWCLIENT", _ = parse_message("NEWCLIENT{NAME=douglas;}").
