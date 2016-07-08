-module(chat_noir_server).

-export([start/0, loop/1]).

start()->
	tcp_server:start(?MODULE, 8888, {?MODULE, loop}).

loop(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			io:format("Data: ~p~n", [binary_to_list(Data)]),
			Message = binary_to_list(Data),
			case Message of
				"end\r\n" ->
					gen_tcp:send(Socket, "Goodbye\n"),
					gen_tcp:close(Socket),
					ok;
				_ ->
					gen_tcp:send(Socket, "Hello world\n"),
					loop(Socket)
			end;
		{error, closed} -> 
			ok
	end.

