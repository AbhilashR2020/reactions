-module(reaction_mnesia).

-export([
         update_counter/4,
         lookup/1
        ]).

-include_lib("reactions/include/reactions.hrl").


update_counter(From, MsgId, ReqType, Action) ->
  Fun =
    fun() ->
      Record = 
      case fragmentation:dirty_read(reactions, MsgId) of
        [Data] ->
           Counters = do_update_counter(Data#reactions.counters, ReqType, Action),
           Data#reactions{counters = Counters,
                          modified = {date(), time()}};
        [] ->
           Counters = do_update_counter([], ReqType, Action),
           #reactions{ message_id = MsgId,
                       counters = Counters,
                       created = {date(), time()},
                       modified = {date(), time()}} 
      end,
      fragmentation:dirty_write(Record),
      {ok, Record#reactions.counters} 
    end, 
    case catch fragmentation:transaction(Fun) of
      {ok, Counters} ->
         {ok, Counters};
      Reason ->
        {error, Reason}
    end.
		
do_update_counter(Counters, ReqType, Action) ->
  Value_1 =
  case lists:keysearch(ReqType, 1, Counters) of
    {value,{ReqType, Value}} ->
       if 
         Action == <<"increment">> ->
             Value+1;
         true ->
             Value-1
       end;
    false -> 0
  end,
  lists:keystore(ReqType, 1, Counters, {ReqType, Value_1}). 
         

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc This is to do a lookup with primary_key in 
%%      the table. primary_key consists of MessageId
%%      & Type(Emotions Type i.e. like, dislike, spam etc) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup(MsgId) ->
  case fragmentation:dirty_read(reactions, MsgId) of
    [Record] ->
      {ok, Record#reactions.counters};
    [] ->
      {error, record_not_found}
  end.

