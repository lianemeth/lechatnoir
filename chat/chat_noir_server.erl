-module(chat_noir_server).

-export([start/0, loop/1, server_test/0]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(client, {
		name,
		info,
		socket
	}).

-record(server_state, {
	  port,
	  nickserver,
	  socket=null}).

start(Name, Port, Loop)->
	NickServer = nickserver:start_nickserver(),
	ServerState = #server_state{port = Port, nickserver = NickServer},
	register(Name, spawn(fun() -> init(ServerState) end)).

accept(ServerState=#server_state{socket=Socket, nickserver=NickServer}) ->
	{ok, Socket} = gen_tcp:accept(Socket),
	% gen_server:cast(self(), {accepted, self()}),
	loop(Socket, NickServer).

init(ServerState = #server_state{port=Port, nickserver=NickServer}) ->
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{ok, Socket} ->
			NewState = ServerState#server_state{socket = Socket},
			{ok, spawn(?MODULE, accept, NewState)};
		{error, Reason} ->
			{stop, Reason}
	end.

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

loop(Socket, NickServer) ->
	MessageMap = parse_message(do_recv(Socket)),
	case get_message_type(MessageMap) of
		<<"NEWCLIENT">> ->
			NewClient = parse_client(MessageMap, Socket),
			send_public_message(<<"New Client Arrived! \n">>),
			loop(Socket);
		<<"PUBLIC">> ->
			loop(Socket);
		<<"PRIVATE">> ->
			loop(Socket);
		_ ->
			gen_tcp:send(Socket, "Error\n"),
			gen_tcp:close(Socket)
	end.

server_test() ->
	NewClientMap = parse_message(<<"{\"TYPE\":\"NEWCLIENT\", \"NAME\":\"DOUGLAs\", \"INFO\":\"ADADA\"}">>),
	<<"NEWCLIENT">> = get_message_type(NewClientMap),
	{client, <<"DOUGLAs">>, <<"ADADA">>, 123} = parse_client(NewClientMap, 123).
