%% -*- mode: erlang; indent-tabs-mode: nil -*-
-module(flexihash_server).
-author('Christopher Vance <cjsv@abacorix.com>').
-copyright('Copyright (c) 2012 Christopher Vance').

-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2]). % required
-export([terminate/2,code_change/3]). % required
%%-export([format_status/2]). % optional
-export([start_link/0]). % gen_server api

%%% implementation records

-record(state, {replicas, tables}).
-record(table, {name, sorted, list}).

%%% required callbacks

%% invoked by
%% gen_server:start_link, gen_server:start
%% (gen_server:init_it)
%%
%% @doc Standard gen_server callback. Initial state for the gen_server
%% server process.
%%
-spec init(Args :: term()) ->
                  {ok, State :: term()} |
                  {ok, State :: term(), timeout() | hibernate} |
                  {stop, Reason :: term()} |
                  ignore.
init([]) ->
    {ok, #state{replicas = 64, tables = []}}.

%% invoked by
%% gen_server:call, gen_server:multi_call
%% (gen_server:handle_msg)
%%
%% @doc Standard gen_server callback. Handle synchronous requests.
%% Implement the api routines in flexihash.erl.
%%
-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()},
                  State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply,
                          Reply :: term(),
                          NewState :: term(),
                          timeout() | hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), timeout() | hibernate} |
                         {stop,
                          Reason :: term(),
                          Reply :: term(),
                          NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call({replicas, Replicas}, _From, State) ->
    State1 = State#state{replicas = Replicas},
    {reply, ok, State1};
handle_call({newtable, Name}, _From, State) ->
    OldTables = State#state.tables,
    NewTable = #table{name = Name, sorted = true, list = []},
    case picktable(OldTables, Name) of
        {ok, _Table, Rest} ->
            State1 = State#state{tables = [NewTable | Rest]},
            {reply, ok, State1};
        _ ->
            State1 = State#state{tables = [NewTable | OldTables]},
            {reply, ok, State1}
    end;
handle_call({deltable, Name}, _From, State) ->
    case picktable(State#state.tables, Name) of
        {ok, _Table, Rest} ->
            State1 = State#state{tables = Rest},
            {reply, ok, State1};
        _ ->
            {reply, ok, State}
    end;
handle_call({addtarget, Name, Target}, _From, State) ->
    case picktable(State#state.tables, Name) of
        {ok, Table, Rest} ->
            NewTable = addtarget(Table, Target, State#state.replicas),
            State1 = State#state{tables = [NewTable | Rest]},
            {reply, ok, State1};
        _ ->
            {reply, {error, notable}, State}
    end;
handle_call({addtargetlist, Name, TargetList}, _From, State) ->
    case picktable(State#state.tables, Name) of
        {ok, Table, Rest} ->
            NewTable = addtargets(Table, TargetList, State#state.replicas),
            State1 = State#state{tables = [NewTable | Rest]},
            {reply, ok, State1};
        _ ->
            {reply, {error, notable}, State}
    end;
handle_call({deltarget, Name, Target}, _From, State) ->
    case picktable(State#state.tables, Name) of
        {ok, Table, Rest} ->
            NewTable = deltarget(Table, Target),
            State1 = State#state{tables = [NewTable | Rest]},
            {reply, ok, State1};
        _ ->
            {reply, {error, notable}, State}
    end;
handle_call({lookup, Name, Value}, _From, State) ->
    case picktable(State#state.tables, Name) of
        {ok, Table, Rest} ->
            Sorted = sorttable(Table),
            State1 = State#state{tables = [Sorted | Rest]},
            {reply, lookup(Sorted, Value), State1};
        _ ->
            {reply, {error, notable}, State}
    end;
handle_call(dump, _From, State) ->
    {reply, State, State};
handle_call(Request, _From, State) ->
    {reply, {error, Request}, State}.

%% invoked by
%% gen_server:cast, gen_server:abcast
%% (gen_server:dispatch)
%%
%% @doc Standard gen_server callback. Handle a request not requiring a
%% reply. Unused in flexihash.
%%
-spec handle_cast(Request :: term(),
                  State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), timeout() | hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% invoked by
%% (gen_server:dispatch)
%%
%% @doc Standard gen_server callback. Handle non-request information.
%% Unused in flexihash.
%%
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), timeout() | hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% invoked by
%% (gen_server:terminate)
%%
%% @doc Standard gen_server callback. Clean up State before stopping.
%%
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) ->
                       Ignored :: term().
terminate(_Reason, _State) ->
    ok.

%% invoked by
%% (gen_server:system_code_change)
%%
%% @doc Standard gen_server callback. Change State as a result of a code
%% change during release upgrade or downgrade.
%%
-spec code_change(OldVsn :: (term() | {down, term()}),
                  State :: term(),
                  Extra :: term()) ->
                         {ok, NewState :: term()} |
                         {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% optional callback

-ifdef(NOTDEF).
%% invoked by
%% (gen_server:format_status,gen_server:terminate)
%%
%% @doc Standard (optional) gen_server callback. Format the process'
%% dictionary and state for output.
%%
%% *** If you want to use the default function instead of this
%% callback, remove format_status/2 from the -export list above and
%% delete this -spec and function. ***
%%
-spec format_status(Opt :: normal | terminate,
                    [{PDict :: [{Key :: atom(), Value :: term()}],
                      State :: term()}]) ->
                           Status :: term().
format_status(_Opt, [_PDict, State]) ->
    [{data, [{"State", State}]}].
-endif.

%%% gen_server api

%%
%% @doc API suggested in OTP Design Principles User's Guide. Start
%% this gen_server process.
%%
-spec start_link() ->
                        {ok, Pid :: pid()} |
                        ignore |
                        {error,
                         Error :: {already_started, Pid :: pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% functions internal to your implementation

%% Pick the named table from a list of tables.
%%
picktable(List, Name) ->
    picktable(List, Name, []).
%%
picktable([], _Name, _Fron) ->
    {error, notfound};
picktable([H | T], Name, Front) ->
    case H#table.name == Name of
        true ->
            {ok, H, lists:append(Front, T)};
        _ ->
            picktable(T, Name, [H | Front])
    end.

%% Generate the positions in the table for the specified target.
%%
positions(Target, Replicas) ->
    positions(Target, erlang:crc32(Target), 0, [], Replicas).
%%
positions(_Target, Replicas, _BaseCRC, Replicas, Acc) ->
    Acc;
positions(Target, Replicas, BaseCRC, N, Acc) when N < Replicas ->
    New = erlang:crc32(BaseCRC, integer_to_list(N)),
    Acc1 = [New | Acc],
    positions(Target, Replicas, BaseCRC, N + 1, Acc1).

%% Add the replicas for one target to the table. Target is a binary.
%%
addtarget(Table, Target, Replicas) when is_list(Target) ->
    addtarget(Table, list_to_binary(Target), Replicas);
addtarget(Table, Target, Replicas) when is_binary(Target) ->
    Pos = positions(Target, Replicas),
    New = lists:map(fun(P) -> {P, Target} end, Pos),
    Table#table{sorted = false, list = lists:append(New, Table#table.list)}.

%% Add the replicas for a list of targets to the table.
%%
addtargets(Table, [], _Replicas) ->
    Table;
addtargets(Table, [H | T], Replicas) ->
    addtargets(addtarget(Table, H, Replicas), T, Replicas).

%% Delete all replicas for one target from the specified table.
%%
deltarget(Table, Target) when is_list(Target) ->
    deltarget(Table, list_to_binary(Target));
deltarget(Table, Target) when is_binary(Target) ->
    Table#table{sorted = false,
                list = lists:filter(fun({_Pos, Targ}) -> Targ =/= Target end,
                                    Table#table.list)}.

%% Sort the targets in a table by their position. Required before lookup.
%%
sorttable(Table) ->
    case Table#table.sorted of
        true ->
            Table;
        _ ->
            Table#table{sorted = true, list = lists:sort(Table#table.list)}
    end.

%% Given a value, find the appropriate target in the table.
%%
lookup(Table, Value) ->
    Hash = erlang:crc32(Value),
    listlook(Hash, Table#table.list, Table#table.list).

%% Internal implementation of lookup. Requires the table's list to be sorted.
%%
listlook(Hash, [{PosN, _}], [{Pos0, Targ0} | _]) ->
    case (PosN < Hash) and (Hash =< Pos0) of
        true ->
            Targ0
    end;
listlook(Hash, [{Pos0, _}, {Pos1, Targ1} | T], All) ->
    case (Pos0 < Hash) and (Hash =< Pos1) of
        true ->
            Targ1;
        _ ->
            listlook(Hash, [{Pos1, Targ1} | T], All)
    end;
listlook(_Hash, [], []) ->
    {error, empty}.
