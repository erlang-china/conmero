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

-module(conmero_app_table).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("conmero.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).
-export([create_app_table/1,
         create_node_table/1,
         is_exist_ets_table/1
        ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_app_table(Application) when is_atom(Application)->
    gen_server:call(?MODULE, {create_app_table, Application}).
create_node_table(Application) when is_atom(Application)->
    gen_server:call(?MODULE, {create_node_table, Application}).

is_exist_ets_table(TableName)->
    case ets:info(TableName) of
         undefined->
            false;
         _->
            true
    end.
%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({create_app_table, Application}, _From, State) ->
    TableName = priv_get_app_table_name(Application),
    NewState  = 
        case create_ets_table(TableName,[{mode,append}],
                              [public, ordered_set, named_table,
                               {keypos,#conmero_app_info.application}]) of
            {new,TableName}->
                [TableName|State];
            {old,TableName}->
                State
        end,
    Reply=TableName,
    {reply, Reply, NewState};
handle_call({create_node_table,Application}, _From, State) ->
    TableName = priv_get_node_table_name(Application),
    NewState  =
        case create_ets_table(TableName,[{mode,append}], 
                              [public, ordered_set, named_table,
                               {keypos,#conmero_app_node.hash_index},
                               {read_concurrency,true}]) of
            {new,TableName}->
                [TableName|State];
            {old,TableName}->
                State
        end,
    Reply=TableName,
    {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
create_ets_table(TableName, Options, ETSProprty) 
  when is_atom(TableName), is_list(Options), is_list(ETSProprty)->
    case is_exist_ets_table(TableName) of
        false->
            {new,ets:new(TableName, ETSProprty)};
        true->
            case proplists:get_value(mode, Options, ETSProprty) of 
                overwrite->
                    ets:delete(TableName),
                    {new,ets:new(TableName,ETSProprty)};
                _->
                    {old,TableName}
            end
    end.

priv_get_app_table_name(Application)->
    list_to_atom(lists:concat([?ETS_APPS_PREFIX,Application])).

priv_get_node_table_name(Application)->
    list_to_atom(lists:concat([?ETS_NODES_PREFIX,Application])).


