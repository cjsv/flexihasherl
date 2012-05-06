%% -*- mode: erlang; indent-tabs-mode: nil -*-
-module(flexihash).
-author('Christopher Vance <cjsv@abacorix.com>').
-copyright('Copyright (c) 2012 Christopher Vance').

-export([replicas/1,newtable/1,deltable/1]). % flexihash api
-export([addtarget/2,addtargetlist/2,deltarget/2,lookup/2]). % flexihash api
-export([start/0]). % start application

%%% flexihash api

%% @doc Change the number of replicas created when adding targets to a
%% table. The initial value is 64. Implemented via flexihash_server.
%%
-spec replicas(Replicas :: non_neg_integer()) ->
                      ok.
replicas(Replicas) when is_integer(Replicas), Replicas > 0 ->
    gen_server:call(flexihash_server, {replicas, Replicas}).

%% @doc Create a new empty table. If the name refers to an existing table
%% it will be replaced. Implemented via flexihash_server.
%%
-spec newtable(Name :: atom()) ->
                      ok.
newtable(Name) when is_atom(Name) ->
    gen_server:call(flexihash_server, {newtable, Name}).

%% @doc Delete a table. Succeeds quietly if the table does not
%% exist. Implemented via flexihash_server.
%%
-spec deltable(Name :: atom()) ->
                      ok.
deltable(Name) when is_atom(Name) ->
    gen_server:call(flexihash_server, {deltable, Name}).

%% @doc Add one Target (with current number of replicas) to the
%% specified table, which must exist. Implemented via flexihash_server.
%%
-spec addtarget(Name :: atom(), Target :: string() | binary()) ->
                       ok |
                       {error, notable}.
addtarget(Name, Target) when is_atom(Name) ->
    gen_server:call(flexihash_server, {addtarget, Name, Target}).

%% @doc Equivalent to invoking addtarget multiple times. Implemented
%% via flexihash_server.
%%
-spec addtargetlist(Name :: atom(), TargetList :: [string() | binary()]) ->
                           ok |
                           {error, notable}.
addtargetlist(Name, TargetList) when is_atom(Name) ->
    gen_server:call(flexihash_server, {addtargetlist, Name, TargetList}).

%% @doc Delete all replicas of the specified target from the named
%% table, which must exist. Succeeds quietly if the target was not
%% present. Implemented via flexihash_server.
%%
-spec deltarget(Name :: atom(), Target :: string() | binary()) ->
                       ok |
                       {error, notable}.
deltarget(Name, Target) when is_atom(Name) ->
    gen_server:call(flexihash_server, {deltarget, Name, Target}).

%% @doc Determine which target stored in the named table, which
%% must exist, is appropriate to the requested Value. Error if
%% the table is empty. Implemented via flexihash_server.
%%
-spec lookup(Name :: atom(), Value :: string() | binary()) ->
                    ok |
                    {error, notable} |
                    {error, empty}.
lookup(Name, Value) when is_atom(Name) ->
    gen_server:call(flexihash_server, {lookup, Name, Value}).

%%% start the application

%% @doc Start the application.
-spec start() ->
                   ok |
                   {error, Reason :: term()}.
start() ->
    flexihash_app:start().

%%% functions internal to your implementation

%%% testing

-ifdef(TEST).
-include("flexihash.hrl"). % only the tests need the record structure
-include_lib("eunit/include/eunit.hrl").

client_test() ->
    Target1 = "Y7tGZe}9pLhB$1TQ",
    Target2 = "j=}?c5/_*J&3#Mp*",
    start(),
    ok = replicas(2),
    ok = newtable(rep2),
    ok = addtargetlist(rep2, [Target1, Target2]),
    Result = gen_server:call(flexihash_server, dump),
    TableList = Result#state.tables,
    [Table] = TableList,
    List = Table#table.list,
    Binary1 = list_to_binary(Target1),
    Binary2 = list_to_binary(Target2),
    Key = "This is a key",
    CRC32 = erlang:crc32(Key), 
    %% these 5 values are identical to what the PHP flexihash produces
    Binary1 = proplists:get_value(2194496399, List),
    Binary1 = proplists:get_value(4123683609, List),
    Binary2 = proplists:get_value(1727109567, List),
    Binary2 = proplists:get_value(301377833, List),
    943121022 = CRC32,
    Lookup = lookup(rep2, Key),
    Binary2 = Lookup,
    ok = deltable(rep2),
    application:stop(flexihash).

rep3_test() ->
    start(),
    ok = replicas(3),
    ok = newtable(rep3),
    ok = addtargetlist(rep3, ["A", "B"]),
    ok = newtable(rep3a),
    ok = addtarget(rep3a, "B"),
    ok = addtarget(rep3a, "A"),
    Lookup = lookup(rep3, "C"),
    Lookup1 = lookup(rep3a, "C"),
    Lookup = Lookup1,
    ok = addtarget(rep3, "D"),
    ok = deltarget(rep3, "D"),
    Lookup2 = lookup(rep3, "C"),
    Lookup = Lookup2,
    Result = gen_server:call(flexihash_server, dump),
    [Table1, Table2] = Result#state.tables,
    List1 = Table1#table.list,
    List2 = Table2#table.list,
    List1 = List2,
    ok = deltable(rep3),
    ok = deltable(rep3a),
    application:stop(flexihash).

-endif.
