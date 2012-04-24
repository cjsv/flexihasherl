-module(flexihash).
-author('Christopher Vance <cjsv@abacorix.com>').
-copyright('Copyright (c) 2012 Christopher Vance').

-export([start/0]).
-export([replicas/1,newtable/1,deltable/1,addtarget/2,addtargetlist/2,deltarget/2,lookup/2]).

start() ->
    application:start(flexihash).

replicas(Replicas) ->
    gen_server:call(flexihash_server, {replicas, Replicas}).

newtable(Name) ->
    gen_server:call(flexihash_server, {newtable, Name}).

deltable(Name) ->
    gen_server:call(flexihash_server, {deltable, Name}).

addtarget(Name, Target) ->
    gen_server:call(flexihash_server, {addtarget, Name, Target}).

addtargetlist(Name, TargetList) ->
    gen_server:call(flexihash_server, {addtargetlist, Name, TargetList}).

deltarget(Name, Target) ->
    gen_server:call(flexihash_server, {deltarget, Name, Target}).

lookup(Name, Value) ->
    gen_server:call(flexihash_server, {lookup, Name, Value}).
