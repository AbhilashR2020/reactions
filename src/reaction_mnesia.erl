-module(reaction_mnesia).

-export([
         update_counter/4,
         lookup/1,
         create_record_entry/1,
         delete_record_entry/1
        ]).

-include_lib("reactions/include/reactions.hrl").


update_counter(From, MsgId, ReqType, Action) ->
    case fragmentation:dirty_read(reactions, MsgId) of
        [Data] ->
            Counters = do_update_counter(Data#reactions.counters, ReqType, Action),
            Record = 
            Data#reactions{counters = Counters,
                          modified = {date(), time()}},
            fragmentation:dirty_write(Record),
            {ok, Counters};
        [] ->
            {error, record_not_found};
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
    false -> 1
  end,
  lists:keystore(ReqType, 1, Counters, {ReqType, Value_1}). 

         
create_record_entry(MsgId) ->
   Fun = 
   fun() ->
       case fragmentation:dirty_read(reactions, MsgId) of 
          [] ->
              Record = 
              #reactions{ message_id = MsgId,
                          counters = [],
                          created = {date(), time()},
                          modified = {date(), time()}},
              ok = fragmentation:dirty_write(Record),
              {ok, success};
          [Data] ->
              {error, record_exists}
        end
    end,
    case catch fragmentation:transaction(Fun) of
      {ok, success} ->
         {ok, success};
      {error, record_exists} ->
         {error, record_exists};
      Reason ->
         {error, Reason}
    end.

delete_record_entry(MsgId) ->
    case fragmentation:dirty_delete({reactions, MsgId}) of 
       ok -> {ok, success};
       Reason -> {error, Reason}
    end. 
             
      
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


