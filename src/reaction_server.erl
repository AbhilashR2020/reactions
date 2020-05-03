-module(reaction_server).

-behaviour(gen_server).

-export([start_link/1]).

-export([
         init/1, 
         handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 code_change/3,
	 terminate/2
       ]).

-export([
         update_counter/5,
         read_counter/2,
         create_record_entry/2
        ]).
		
start_link(ProcId) ->
    ProcName = process_name(ProcId),
    gen_server:start_link({local, ProcName}, ?MODULE, [], []).

process_name(Id) ->
    list_to_atom("reaction_server_" ++ integer_to_list(Id)).

create_record_entry(ProcId, MsgId) when is_integer(ProcId) ->
   create_record_entry(process_name(ProcId), MsgId); 

create_record_entry(ProcName, MsgId) ->
    gen_server:call(ProcName, {create_record_entry, MsgId}). 

update_counter(ProcId, From, MsgId, ReqType, Action) when is_integer(ProcId) ->
    update_counter(process_name(ProcId), From, MsgId, ReqType, Action);

update_counter(ProcName, From, MsgId, ReqType, Action) ->
    gen_server:call(ProcName, {update_counter, From, MsgId, ReqType, Action}).

read_counter(ProcId, MsgId) when is_integer(ProcId) ->
    read_counter(process_name(ProcId), MsgId);

read_counter(ProcName, MsgId) ->
    gen_server:call(ProcName, {read_counter, MsgId}).

%%------------------------------------------------
%%
%%   CALLBACK FUNCTIONS
%%
%%------------------------------------------------

init([]) ->
    {ok, []}.

handle_call({read_counter, MsgId}, _From, State) ->
    Reply = reaction_mnesia:lookup(MsgId),
    {reply, Reply, State};
   
handle_call({update_counter, From, MsgId, ReqType, Action}, _From, State) ->
    Reply = reaction_mnesia:update_counter(From, MsgId, ReqType, Action),
    {reply, Reply, State};

handle_call({create_record_entry, MsgId}, _From, State) ->
    Reply = reaction_mnesia:create_record_entry(MsgId),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, invalid_request, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
