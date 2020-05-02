%%%-------------------------------------------------------------------
%%% File    : reactions_app.erl
%%% Description : A reactions application is a counter based application
%%%               which holds like/dislike/spam etc..  
%%%-------------------------------------------------------------------
-module(reactions_app).

-behaviour(application).


-export([start/2, stop/1]).


start(_Type, _StartArgs) ->
  case reactions_sup:start_link() of
    {ok, Pid} ->
      alarm_handler:clear_alarm({application_stopped, reactions}),
      {ok, Pid};
    Error ->
      alarm_handler:set_alarm({{application_stopped, reactions}, []}),
      Error
  end.

stop(_State) ->
  alarm_handler:set_alarm({{application_stopped, reactions}, []}),
  ok.

