%%% File    : reactions_i.erl
%%% Description : An initialization script

-module(reactions_i).

-include("../include/reactions.hrl").

-export([
	 install/0
	]).

install() ->
  Res = (catch mnesia:create_table(reactions, table_options())),
  error_logger:info_msg("reactions application installation result ~p~n", [Res]).


table_options() ->
  Nodes = [{disc_copies, application:get_env(reactions, nodes, [node()])}],
  error_logger:info_msg("Installing reactions tables on ~1000.p nodes.~n", [Nodes]),
  Options =   
    [
     {attributes, record_info(fields, reactions)}
    ],
  FragOptions = [{frag_properties,[{n_disc_copies, 1},{n_fragments, 2}]}],
  Options ++ Nodes ++ FragOptions. 
