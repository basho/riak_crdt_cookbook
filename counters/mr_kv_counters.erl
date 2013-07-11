%% -------------------------------------------------------------------
%% mr_kv_counters: utilities for map/reducing on KV Counters
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%  http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(mr_kv_counters).

% Map Functions
-export([value/3]).

% Reduce Functions
-export([sum/2, maximum/2, strip_date/2]).


% @doc value/3 takes a Riak Object, and returns a proplist containing only
%      its key, and the counter value. Use it as the first Map step of an
%      operation.
%
%      In the case that none of the object's siblings are counters, the
%      count returned will be 0.
%
value(RiakObject, _KeyData, _Arg) ->
    Key   = riak_object:key(RiakObject),
    Count = riak_kv_counter:value(RiakObject),
    [ {Key, Count} ].

% @doc sum/2 takes a list of either counts or {key, count} pairs, and 
%      adds up all the counts. 
%
%      The overall result is the total returned as list 
%      containing the pair {<<"total">>, integer total}.
%
%      If PairList = [], then the total will be 0.
%
sum(PairList, _Arg) ->
    [ {<<"total">>, lists:foldl(fun add_sum/2, 0, PairList)} ].

add_sum({_Key, Count}, Acc) ->
    Acc + Count;
add_sum(Count, Acc) when is_integer(Count) ->
    Acc + Count.

% @doc maximum/2 takes a list of {key, count} pairs, and returns
%      a list of *all* pairs with the maximum count.
%
%      If PairList = [], then the result will be []
%
maximum([], _Arg) ->
    [];
maximum(PairList, _Arg) ->
    lists:foldl(fun choose_max/2, [], PairList).

choose_max({Key, Count}, []) ->
    [ {Key, Count} ];
choose_max({Key, Count}, [{_MaxKey, MaxCount}|_] = Maximums) ->
    if
        Count  >  MaxCount -> [ {Key, Count} ];
        Count =:= MaxCount -> [ {Key, Count} | Maximums ];
        true               -> Maximums
    end.

% @doc strip_date/2 splits the date off the start of the key if it has the format
%      "YYYYMMDD!<rest>", as it is in our example. If that's not present, it leaves
%      the key as-is. It does absolutely nothing to the count part of the pairs.
%
strip_date(PairList, _Arg) ->
    dict:to_list(lists:foldl(fun strip_date_and_sum/2, dict:new(), PairList)).

strip_date_and_sum({Key, Count}, Dict) ->
    BangLocation = 8,
    Key1 = case binary:at(Key, BangLocation) of
        $! -> binary:part(Key, BangLocation+1, byte_size(Key)-(BangLocation+1));
        _  -> Key
    end,
    dict:update_counter(Key1, Count, Dict).
