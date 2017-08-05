-module(chat_noir_server).

-export([start/0, start/2, loop/2, accept_loop/1, server_test/0]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).


-record(client, {
		name,
		info,
		socket
	}).

-record(public, {
	  from,
	  message
        }).

-record(server_state, {
	  port,
	  nickserver,
	  socket=null}).

start()->
	start('LeChatNoir', 8888).

start(Name, Port)->
	NickServer = nickserver:start(),
	ServerState = #server_state{port = Port, nickserver = NickServer},
	register(Name, spawn(fun() -> init(ServerState) end)).


accept_loop(State=#server_state{socket=ListenSocket, nickserver=NickServer}) ->
	case  gen_tcp:accept(ListenSocket) of
		{ok, AcceptSocket} ->
			spawn(?MODULE, loop, [AcceptSocket, NickServer]),
			accept_loop(State);
		{error, Reason} ->
			io:format(<<"WHAT">>),
			io:format(Reason),
			{stop, Reason}
	end.

init(ServerState = #server_state{port=Port, nickserver=NickServer}) ->
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{ok, ListenSocket} ->
			NewState = ServerState#server_state{socket = ListenSocket, nickserver=NickServer},
			accept_loop(NewState);
		{error, Reason} ->
			{stop, Reason}
	end.


get_message_type([]) -> null;
get_message_type([<<"PUBLIC">> | _]) -> public;
get_message_type([<<"NEWCLIENT">> | _]) -> newclient;
get_message_type([<<"PVT">>|_]) -> private;
get_message_type([_]) ->  null.

%get_message_type(MessageList) ->
%	lists:nth(1, MessageList).

parse_client(MsgList, Socket) ->
	Name = lists:nth(2, MsgList),
	Info = lists:nth(3, MsgList),
	#client{name=Name, 
	        info=Info, 
	        socket=Socket}.

parse_public(MsgList)->
	FromName = lists:nth(2, MsgList),
	Message = lists:nth(3, MsgList),
	#public{from=FromName,
		message=Message}.


parse_message([]) ->
	[];
parse_message(<<"">>) ->
	[];
parse_message(SomeBinary) ->
	binary:split(SomeBinary, <<";">>,  [global, trim_all]). 


loop(Socket, NickServer) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			process_mesage(Data, Socket, NickServer);
		{error, closed} -> 
			{error, closed}
	end.



process_mesage(Data, Socket, NickServer) ->
	MessageList = parse_message(Data),
	case get_message_type(MessageList) of
		newclient ->
			NewClient = parse_client(MessageList, Socket),
			NickServer ! {self(), {newclient, NewClient}},
			loop(Socket, NickServer);
		public ->
			NewPublicMessage = parse_public(MessageList),
			NickServer ! {self(), {public, NewPublicMessage}},
			loop(Socket, NickServer);
		private ->
			loop(Socket, NickServer);
		_ ->
			gen_tcp:send(Socket, "Error\n"),
			gen_tcp:close(Socket)
	end.


server_test() ->
	NewClientMap = parse_message(<<"NEWCLIENT;DOUGLAs;ADADA">>),
	<<"NEWCLIENT">> = get_message_type(NewClientMap),
	{client, <<"DOUGLAs">>, <<"ADADA">>, 123} = parse_client(NewClientMap, 123).
