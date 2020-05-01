-module(fragmentation).

-export([
          dirty_read/2,
	  dirty_index_read/3,
	  dirty_write/1,
	  dirty_write/2,
	  dirty_delete/1,
	  transaction/1,
	  transaction/2,
	  transaction/3
       ]).

dirty_read(Table, Key) ->
  Fun = 
    fun(T, K) ->
      mnesia:read(T, K, read)
    end,
  mnesia:activity(async_dirty, Fun, [Table, Key], mnesia_frag).

dirty_index_read(Table, SecondaryKey, Pos) ->
  F = fun(T, Sk, P) -> mnesia:index_read(T,Sk,P) end,
  mnesia:activity(async_dirty, F, [Table, SecondaryKey, Pos], mnesia_frag).


dirty_write(Record) ->
  Table = erlang:element(1, Record),
  dirty_write(Table, Record).

dirty_write(Table, Record) ->
  F = 
    fun(T, R) ->
      mnesia:write(T, R, write)
    end,
  mnesia:activity(async_dirty, F, [Table, Record], mnesia_frag).

dirty_delete({Table, Key}) ->
  F = fun(T, K) ->
	mnesia:delete(T, K, write)
      end,
  mnesia:activity(async_dirty, F, [Table, Key], mnesia_frag).

transaction(Fun) ->
  transaction(Fun, [], infinity).

transaction(Fun, Args) ->
  transaction(Fun, Args, infinity).

transaction(Fun, Args, Retries) ->
  case catch mnesia:activity({transaction, Retries}, Fun, Args, mnesia_frag) of
    {'EXIT', Reason} ->
      {error, Reason};
    Result ->
      Result
  end.

