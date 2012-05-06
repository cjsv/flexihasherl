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
