-module(chat_noir_server).

-export([start/0, loop/1]).

start()->
	tcp_server:start(?MODULE, 8888, {?MODULE, loop}).

loop(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			gen_tcp:send(Socket, "Hello world"),
			loop(Socket);
		{error, closed} -> 
			ok
	end.

