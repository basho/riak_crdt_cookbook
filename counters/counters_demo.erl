%% -------------------------------------------------------------------
%%
%% counters_demo: Sets up and clears node partitions
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

-module(counters_demo).

-export([part/2]).

% Instructions:
% Attach a shell to your cluster, and partition by running:
% > Healer = counters_demo:part(['dev3@127.0.0.1'], "newcookie").
% Then to heal the partition, run:
% > Healer().
%

part(Friends0, Cookie) ->
    OldCookie = erlang:get_cookie(),
    Enemies = nodes() -- Friends0,
    Friends = [node()|Friends0],
    [ rpc:call(F, erlang, set_cookie, [F, Cookie]) || F <- Friends ],
    [ rpc:call(F, erlang, disconnect_node, [E]) || F <- Friends,
                                                   E <- Enemies ],
    fun() ->
            heal(Friends, Enemies, OldCookie)
    end.

heal(Friends, Enemies, Cookie) ->
    [ rpc:call(F, erlang, set_cookie, [F, Cookie]) || F <- Friends ],
    rpc:sbcast(Friends ++ Enemies, riak_core_node_watcher, broadcast).
