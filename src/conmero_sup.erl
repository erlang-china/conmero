%% -------------------------------------------------------------------
%% Copyright (c) 2013 Honghu Huang (huanghonghu@adsage.com)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% -------------------------------------------------------------------


-module(conmero_sup).
-behaviour(supervisor).
-export([init/1]).


%% ====================================================================
%% Macro definition
%% ====================================================================
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 1000, Type, [I]}).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

start_link() ->
    ensure_started(ets_mgr),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
    SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
    RestartStrategy :: one_for_all
                     | one_for_one
                     | rest_for_one
                     | simple_one_for_one,
    ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
    StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
    RestartPolicy :: permanent
                   | transient
                   | temporary,
    Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    Stretagy        = {one_for_all,1000,1000},
    ConmeroApptable = ?CHILD(conmero_app_table, worker),
    ConmeroManager  = ?CHILD(conmero_manager,   worker),
    {ok,{Stretagy, [ConmeroApptable,ConmeroManager]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


