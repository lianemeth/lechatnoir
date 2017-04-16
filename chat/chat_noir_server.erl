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
			Data;
		{error, closed} -> 
			ok
	end.

get_message_type(MessageMap) ->
	string:to_upper(
	  binary_to_list(
	    maps:get(<<"type">>, MessageMap))). 

parse_client(SomeString, Socket) ->
	#client{name="Douglas", info="ada", socket=Socket}.

parse_message(SomeString) ->
	jsone:decode(SomeString). 

loop(Socket, Clients) ->
	MessageMap = parse_message(do_recv(Socket)),
	case get_message_type(MessageMap) of
		"NEWCLIENT" ->
			gen_tcp:send(Socket, "New Client Arrived! \n"),
			NewClient = parse_client(MessageMap, Socket),
			io:format("New Client ~p ", [NewClient]),
			loop(Socket, [NewClient | Clients]);
		"PUBLIC" ->
			loop(Socket, Clients);
		"PRIVATE" ->
			loop(Socket, Clients);
		_ ->
			gen_tcp:send(Socket, "Error\n"),
			gen_tcp:close(Socket)
	end.

server_test() ->
	NewClientMap = parse_message(<<"{\"type\":\"newclient\", \"name\":\"douglas\"}">>),
	"NEWCLIENT" = get_message_type(NewClientMap).
