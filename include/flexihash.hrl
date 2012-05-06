%% -*- mode: erlang; indent-tabs-mode: nil -*-
%% author Christopher Vance <cjsv@abacorix.com>
%% Copyright (c) 2012 Christopher Vance

%%% implementation records

-record(state, {replicas, tables}).
-record(table, {name, sorted, list}).
