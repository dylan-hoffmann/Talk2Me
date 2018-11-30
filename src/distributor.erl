%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Comp 50CP 
% Final Project: Talk2Me
% Xiang Gao
% Updated: Nov. 29, 2018
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(distributor).
-behavior(gen_server).

% Clients
-export([join/3, receiving/1]).
% Server
-export([start_link/0, stop/0, listen/0, start_python/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([join_play/2, leave_play/1, send_message/2, list_actors/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Client part
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
join(Actor_name, Node_name, Play_name) ->
	{ok, P1} = python:start([{python_path, "."}, {python, "python3"}])
	python:call(P1, actor, start, [Actor_name]),
	Pid = spawn(distributor, receiving, [Actor_name]),
	rpc:call(Node_name, distributor, join_play, [Actor_name, Pid, P1]),
	% only parser sends message
	io:format("~p: joining.~n", [Actor_name]).

receiving(Actor_name) ->
	receive
		% Actor_name should match Receiver name
		{message, Actor_name, Text} ->
			% receive message from Python
			io:format("~p: ~p~n", [Actor_name, Text]);
			%call_python(Actor_name, Text);
		% add actor to a list of actors
		{list, Actors}	->
			io:format("Current actors are:~n", []),
			print(Actors);
		{notice, Notice} ->
			io:format("~p~n", [Notice]);
		_other ->
			dunno		
	end,
	receiving(Actor_name).

% this function can only be used by the parser
% parser is like a controller of the whole play
% sending()
% 	receive
% 		{Actor_name, Text} ->
% 			io:format("~p ~p, ~n", [Actor_Name, Text]),
% 			case {Actor_name, Text} of
% 				{Actor_name, "--remove"} ->
% 					% the play ends
% 					rpc:call(Node_name, distributor, leave_play, [Actor_name]),
% 					sending(parser, Node_name, Play_name, Pid);
% 				{Actor_name, "--quit"} ->
% 					rpc:call(Node_name, distributor, stop, []),
% 					ok;
% 				{_, "--list"} ->
% 					rpc:call(Node_name, distributor, list_actors, [Pid]),
% 					sending(parser, Node_name, Play_name, Pid);
% 				_Other ->
% 					rpc:call(Node_name, distributor, send_message, [Actor_name, Text]),
% 					sending(parser, Node_name, Play_name, Pid)
% 			end,	
% 	end.

% Erlang call python function
call_python(Name, Text) ->
	{ok, PID} = python:start(),
	python:call(PID, speech, speech, [Name, Text]),
	python:stop(PID).
% print the names of all actors
print([]) -> ok;
print([First | Rest]) ->
	io:format("~p~n", [First]),
	print(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Server part
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% API for play server
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_python() ->
	P2 = spawn(distributor, listen, []),
    {ok, P1} = python:start([{python_path, "."}, {python, "python3"}]),
    python:call(P1, talk2me, start, [P2]),
	ok.


stop() ->
	%gen_server:stop().
	gen_server:cast(?MODULE, {stop}).

% add an actor client to the server
join_play(Name, Pid) ->
	gen_server:cast(?MODULE, {subscribe, Name, Pid}).
% remove a chat client from the server
leave_play(Name) ->
	gen_server:cast(?MODULE, {unsubscribe, Name}).
% send message from parser to an actor
send_message(Receiver, Text) ->
	gen_server:call(?MODULE, {send_message, Receiver, Text}).
% list all the names of actors
% FromPid should be the pid of parser
list_actors(FromPid) ->
	gen_server:call(?MODULE, {list_actors, FromPid}).	

listen() ->
    receive
        {Actor_name, Msg}-> rpc:call(node(), distributor, send_message, [Actor_name, Msg])
    end,
	listen().



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Server part: callback module
%
% Parameters
% Name: Name of client/actor)
% Pid: Pid of client/actor)
% State: list of tuple {Actor_Name, Pid}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	io:format("Our play starts.~n"),
	%RoomName = io:fread('Chat room name: ', "~a"),
	State = [],
	{ok, State}.

handle_call({send_message, Receiver, Text}, _From, State) ->
	io:format("~p: ~p~n", [Receiver, Text]), 
	Reply = message(Receiver, Text, State),
	{reply, Reply, State};

handle_call({list_actors, Pid}, _From, State) ->
	Reply = get_actors(Pid, State),
	{reply, Reply, State}.

handle_cast({stop}, State) ->
	{stop, play_ends, State};

handle_cast({subscribe, Name, Pid}, State) ->
	NewState = add_actor(Name, Pid, State),
	{noreply, NewState}; 		

handle_cast({unsubscribe, Name}, State) ->
	NewState = remove_actor(Name, State),
	{noreply, NewState};

handle_cast(_Message, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	io:format("The play ends.~n"),
	ok.				


% subscribe handler
add_actor(Name, Pid, State) ->
	Find = is_exist(Name, State),
	if
		Find == find ->
			io:format("~p already exists.~n", [Name]),
			State;
		true ->
			io:format("New actor ~p joins.~n", [Name]),
			NewState = [{Name, Pid} | State], 
			NewState
	end.	

% unsubscribe handler
remove_actor(Name, State) ->
	Find = is_exist(Name, State),
	if 
		Find == find ->
			io:format("Actor ~p leaves.~n", [Name]),
			Pid = get_pid(Name, State),	
			NewState = lists:delete({Name, Pid}, State),
			NewState;
		true -> 
			io:format("~p does not exist.~n", [Name]),
			State
	end.		

% send_message handler
message(Receiver, Text, State) ->
	Find = is_exist(Receiver, State),
	if
	 	Find == find ->
	 		FullMessage = {message, Receiver, Text},
			Pid = get_pid(Receiver, State),
			send_to_actor(Pid, FullMessage),
			ok;
		true ->
			io:format("~p does not exist.~n", [Receiver]),
			error			
	 end.

send_to_actor(Pid, FullMessage) ->
	%io:format("Sending message to all: ~p~n", [FullMessage]),
	Pid ! FullMessage.

get_pid(_, []) ->
	error;
get_pid(Receiver, [First | Rest]) ->
	{Name, Pid} = First,
	if
		Name == Receiver -> Pid;
		true -> get_pid(Receiver, Rest)
	end.

% get_actors handler
get_actors(From, State) ->
	Actors = get_names(State, []), 
	From ! {list, Actors},
	ok.

get_names([], Res) -> Res;	
get_names([First | Rest], Res) ->
	{Name, _} = First,
	get_names(Rest, [Name | Res]).

% check if actor exists
is_exist(_, []) -> not_find;
is_exist(Actor_name, [First | Rest]) ->
	{Name, _} = First,
	if 
		Actor_name == Name -> find;
		true -> is_exist(Actor_name, Rest)
	end.	