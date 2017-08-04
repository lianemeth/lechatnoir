-module(nickserver).

-export([start/0, loop/1]).

-record(client, {
		name,
		info,
		socket
	}).

-record(public, {
	  from,
	  message
        }).

start() ->
	spawn(fun() -> loop([]) end).

send_public_message(Clients, Message)->
	lists:foreach(fun(Cli) ->
			gen_tcp:send(Cli#client.socket, Message) end
		     ,Clients).

loop(Clients)->
	receive
		{From, {newclient, Client}}->
			NewClients = [Client|Clients],
			Message = <<"New Client Arrived!">>,
			send_public_message(Clients, Message),
			From ! {ok, sent},
			loop(NewClients);
		{From, {public, Message}} ->
			send_public_message(Clients, Message#public.message),
			From ! {ok, sent},
			loop(Clients);
		stop ->
			true
	end.

