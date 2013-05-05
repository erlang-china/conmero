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


-module(conmero).

-include("../include/conmero.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0]).
-export([call/2,call/3,
         cast/2, cast/3]).

start()->
    application:start(conmero).

stop()->
    application:stop(conmero).

call(Application, Message)->
    call(Application, "", Message).
call(Application, HashKey, Message) when is_atom(Application)->
    send(call, Application, HashKey, Message).

cast(Application, Message)->
    cast(Application, "", Message).
cast(Application, HashKey, Message) when is_atom(Application)->
    send(cast, Application, HashKey, Message).

%% ====================================================================
%% Internal functions
%% ====================================================================
send(Type, Application, HashKey, Message)->
    try
        case get_app_info(Application) of
            AppInfo when is_record(AppInfo, conmero_app_info)->
                send_to_server(Type, AppInfo, HashKey, Message);
            _->
                {error,no_app_info}
        end
    catch
        E:R->
            {E,R}
    end.


send_to_server(Type, AppInfo, HashKey, Message)->
    #conmero_app_info{
                        application     = Application,
                        call_type       = CallType,
                        gen_server_type = GenServerType,
                        timeout         = Timeout,
                        master_node     = MasterNode,
                        server_name     = ServerName}
                        = AppInfo,
    case CallType of
        general->
            send_to_server(GenServerType, Type, MasterNode, ServerName, Message, Timeout);
        consistent->
            {ok,FinalApplicationNode} = get_consistent_node(Application, HashKey),
            #conmero_node{
                        node        = ConNode,
                        server_name = ConServerName,
                        timeout     = ConTimeout
                     } = FinalApplicationNode,
            send_to_server(GenServerType, Type, ConNode, ConServerName, Message, ConTimeout)
    end.

get_consistent_node(Application,Key)->
    TableName = get_node_table_name(Application),
    case conmero_app_table:is_exist_ets_table(TableName) of
        true->
            KeyHash = conmero_manager:get_key_hash(Key),
            get_final_online_node(TableName, KeyHash);
        false->
            {error,application_not_exists};
        _->
            {error,get_node_failed}
    end.

get_node_table_name(Application)->
    case get({conmero, node_info, Application}) of
        undefined->
            TableName = list_to_atom(lists:concat([?ETS_NODES_PREFIX, Application])),
            put({conmero, node_info, Application}, TableName),
            TableName;
        TableName->
            TableName
    end.

get_app_info(Application)->
    TableName=
    case get({conmero,app_info,Application}) of
        undefined->
            TableNameTmp = list_to_atom(lists:concat([?ETS_APPS_PREFIX, Application])),
            put({conmero, app_info, Application}, TableNameTmp),
            TableNameTmp;
        TableNameTmp->
            TableNameTmp
    end,
    hd(ets:lookup(TableName, Application)).

get_final_online_node(TableName, KeyHash)->
    case ets:select(TableName, get_online_node_match_spec(KeyHash), 1) of
        {[MatchedNode],_Continuation} ->
            {ok, MatchedNode};
        '$end_of_table'->
            get_default_online_node(TableName)
    end.

get_default_online_node(TableName)->
    case ets:select(TableName, get_whole_online_nodes_match_spec(),1) of 
        {[MatchedNode],_Continuation}->
            {ok, MatchedNode};
        '$end_of_table'->
            {error,no_node_online}
    end.

get_online_node_match_spec(KeyHash)->
    [{#conmero_node{id = '_', v_node_id = '_', node = '_', server_name = '_',
                    hash = '$1', status = '$2', source_tag='_'},
    [{'==','$2',online},{'>','$1',KeyHash}],
    ['$_']}].

get_whole_online_nodes_match_spec()->
    [{#conmero_node{id = '_', v_node_id = '_', node = '_', server_name = '_',
                    hash = '_', status = '$2', source_tag='_'},
    [{'==','$2',online}],
    ['$_']}].


send_to_server(1, Type, MasterNode, ServerName, Message, Timeout)->
    send_to_gen_server(Type, MasterNode, ServerName, Message, Timeout);
send_to_server(2, Type, MasterNode, ServerName, Message, Timeout)->
    send_to_gen_server2(Type, MasterNode, ServerName, Message, Timeout).

send_to_gen_server(call, Node, ServerName, Message, Timeout)->
    gen_server:call({ServerName, Node}, Message, Timeout);
send_to_gen_server(cast, Node, ServerName, Message,_Timeout)->
    gen_server:cast({ServerName, Node}, Message).

send_to_gen_server2(call, Node, ServerName, Message, Timeout)->
    gen_server2:call({ServerName, Node}, Message, Timeout);
send_to_gen_server2(cast, Node, ServerName, Message,_Timeout)->
    gen_server2:cast({ServerName, Node}, Message).





