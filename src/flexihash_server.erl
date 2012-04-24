-module(flexihash_server).
-author('Christopher Vance <cjsv@abacorix.com>').
-copyright('Copyright (c) 2012 Christopher Vance').

-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]). % required
%-export([format_status/2]). % optional
-export([start_link/0]). % api

%% implementation records

-record(state, {replicas, tables}).
-record(table, {name, sorted, list}).

%% required callbacks

% invoked by
% gen_server:start_link, gen_server:start
% (gen_server:init_it)
%
-spec init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
%
init([]) ->
    % default 64 replicas per target
    {ok, #state{replicas = 64, tables = []}}.

% invoked by
% gen_server:call, gen_server:multi_call
% (gen_server:handle_msg)
%
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
%
handle_call({replicas, Replicas}, _From, State) ->
    % only affects new targets
    State1 = State#state{replicas = Replicas},
    {reply, ok, State1};
handle_call({newtable, Name}, _From, State) ->
    OldTables = State#state.tables,
    NewTable = #table{name = Name, sorted = true, list = []},
    case picktable(OldTables, Name) of
        {ok, _Table, Rest} ->
            % replace existing table
            State1 = State#state{tables = [NewTable | Rest]},
            {reply, ok, State1};
        _ ->
            % table did not exist
            State1 = State#state{tables = [NewTable | OldTables]},
            {reply, ok, State1}
    end;
handle_call({deltable, Name}, _From, State) ->
    case picktable(State#state.tables, Name) of
        {ok, _Table, Rest} ->
            % remove existing table
            State1 = State#state{tables = Rest},
            {reply, ok, State1};
        _ ->
            % already does not exist
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
handle_call(Request, _From, State) ->
    {reply, {error, Request}, State}.

% invoked by
% gen_server:cast, gen_server:abcast
% (gen_server:dispatch)
%
-spec handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
%
handle_cast(_Request, State) -> {noreply, State}.

% invoked by
% (gen_server:dispatch)
%
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
%
handle_info(_Info, State) -> {noreply, State}.

% invoked by
% (gen_server:terminate)
%
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                           term()),
                    State :: term()) ->
    term().
%
terminate(_Reason, _State) -> ok.

% invoked by
% (gen_server:system_code_change)
%
-spec code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                  Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.
%
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% optional callback

% invoked by
% (gen_server:format_status,gen_server:terminate)
%
%format_status(_Opt, [_PDict, _State]) -> status.

%% api

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% implementation

picktable(List, Name) ->
    picktable(List, Name, []).

picktable([], _Name, _Fron) ->
    {error, notfound};
picktable([H | T], Name, Front) ->
    case H#table.name == Name of
        true ->
            {ok, H, lists:append(Front, T)};
        _ ->
            picktable(T, Name, [H | Front])
    end.

positions(Target, Replicas) ->
    positions(Target, erlang:crc32(Target), 0, [], Replicas).

positions(_Target, Replicas, _BaseCRC, Replicas, Acc) ->
    Acc;
positions(Target, Replicas, BaseCRC, N, Acc) when N < Replicas ->
    New = erlang:crc32(BaseCRC, integer_to_list(N)),
    Acc1 = [New | Acc],
    positions(Target, Replicas, BaseCRC, N + 1, Acc1).

addtarget(Table, Target, Replicas) when is_list(Target) ->
    addtarget(Table, list_to_binary(Target), Replicas);
addtarget(Table, Target, Replicas) when is_binary(Target) ->
    Pos = positions(Target, Replicas),
    New = lists:map(fun(P) -> {P, Target} end, Pos),
    Table#table{sorted = false, list = lists:append(New, Table#table.list)}.

addtargets(Table, [], _Replicas) ->
    Table;
addtargets(Table, [H | T], Replicas) ->
    addtargets(addtarget(Table, H, Replicas), T, Replicas).

% uncertain whether deltarget can disturb sorting, so don't assume it's ok
deltarget(Table, Target) when is_list(Target) ->
    deltarget(Table, list_to_binary(Target));
deltarget(Table, Target) when is_binary(Target) ->
    Table#table{sorted = false,
        list = lists:filter(fun({_Pos, Targ}) -> Targ =/= Target end,
            Table#table.list)}.

sorttable(Table) ->
    case Table#table.sorted of
        true ->
            Table;
        _ ->
            Table#table{sorted = true, list = lists:sort(Table#table.list)}
    end.

lookup(Table, Target) ->
    Hash = erlang:crc32(Target),
    listlook(Hash, Table#table.list, Table#table.list).

% listlook requires the list be sorted
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
    end.
