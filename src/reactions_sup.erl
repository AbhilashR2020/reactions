%%%-------------------------------------------------------------------
%%% File    : reactions_sup.erl
%%% Description : A supervisor module for reactions counter handler. 
%%%-------------------------------------------------------------------
-module(reactions_sup).

-behaviour(supervisor).

-define(CHILD(Id, Module, Type, Args),
          {Id, {Module, start_link, Args}, permanent, infinity, Type, []}).
		  
-export([
	  start_link/0,
          create_children/0
        ]).

-export([
	  init/1
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervior CALLBACK functions
%%====================================================================
init([]) ->
   Children = create_children(),
   {ok,{{one_for_one, 10, 1}, Children}}.


%% As of now, i don't suggest dynamic addition of child processes
create_children() ->
  ProcessCount = application:get_env(reactions, processes, 5),
  Procs = [ {X, proc_name(X)} || X <- lists:seq(1, ProcessCount)],
  [?CHILD(ProcessName, reaction_server, worker, [X]) || {X, ProcessName} <- Procs].


proc_name(X) ->  
  list_to_atom("reaction_server_" ++integer_to_list(X)). 
