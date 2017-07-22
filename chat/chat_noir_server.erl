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
        maps:get(<<"TYPE">>, MessageMap). 

parse_client(MsgMap, Socket) ->
	Name = maps:get(<<"NAME">>, MsgMap),
	Info = maps:get(<<"INFO">>, MsgMap),
	#client{name=Name, 
	        info=Info, 
	        socket=Socket}.

parse_message(SomeString) ->
	jsone:decode(SomeString). 

send_public_message(Message, Clients) ->
	lists:foreach(fun(Cli) ->
			gen_tcp:send(Cli#client.socket, Message) end
		     ,Clients).

loop(Socket, Clients) ->
	MessageMap = parse_message(do_recv(Socket)),
	case get_message_type(MessageMap) of
		<<"NEWCLIENT">> ->
			NewClient = parse_client(MessageMap, Socket),
			send_public_message(<<"New Client Arrived! \n">>, [NewClient | Clients]),
			loop(Socket, [NewClient | Clients]);
		<<"PUBLIC">> ->
			loop(Socket, Clients);
		<<"PRIVATE">> ->
			loop(Socket, Clients);
		_ ->
			gen_tcp:send(Socket, "Error\n"),
			gen_tcp:close(Socket)
	end.

server_test() ->
	NewClientMap = parse_message(<<"{\"TYPE\":\"NEWCLIENT\", \"NAME\":\"DOUGLAs\", \"INFO\":\"ADADA\"}">>),
	<<"NEWCLIENT">> = get_message_type(NewClientMap),
	{client, <<"DOUGLAs">>, <<"ADADA">>, 123} = parse_client(NewClientMap, 123).

