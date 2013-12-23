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


-module (conmero).

-include ("conmero.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export ([start/0, stop/0]).

%%% conmero:call(record.conmero_params)
%%% conmero:cast(record.conmero_params)

-export ([call/1, cast/1]).
-export ([get_calling_node_tag/2]).

start()->
    application:start(conmero).

stop()->
    application:stop(conmero).

call( Info) when is_record(Info, conmero_params)->
    send(call, Info);
call(_Info)->
    {error, bad_arg}.

cast( Info) when is_record(Info, conmero_params)->
    send(cast, Info);
cast(_Info)->
    {error, bad_arg}.

get_calling_node_tag( Application, Key) when is_atom(Application)->
    get_node_tag(Application,Key);
get_calling_node_tag(_Application,_Key)->
    {error, bad_arg}.

%% ====================================================================
%% Internal functions
%% ====================================================================
get_node_tag(Application, Key)->
    case get_app_info(Application) of 
        AppInfo when is_record(AppInfo, conmero_app_info)->
            case AppInfo#conmero_app_info.call_algorithm_type of
                0->{ok, AppInfo#conmero_app_info.node_tag};
                1->
                    KeyHash   = conmero_manager:get_key_hash(Key),
                    TableName = get_table_name(Application, node_info),
                    case get_final_next_node(TableName, KeyHash) of
                        {ok, MatchedNode}->
                            {ok, MatchedNode#conmero_app_node.node_tag};
                        {error, Reason}->
                            {error,Reason}
                    end
            end;
        _ ->
            {error,get_app_faild}
    end.

send(CallType, Info)->
    #conmero_params {
                application  = App,
                message      = Msg,
                hashkey      = HashKey,
                specify_type = SpecfyType}
        = Info,
    send(CallType, App, HashKey, Msg, SpecfyType).

send(CallType, App, HashKey, Msg, SpecfyType) when is_atom(App)->
    AppInfo = get_app_info(App),
    send_to_server(CallType, AppInfo, HashKey, Msg, SpecfyType);

send(_Type,_App,_HashKey,_Msg,_SpecfyType)->
    {error, valid_app}.

get_app_info(App)->
    try
        TableName = get_table_name(App, app_info),
        hd(ets:lookup(TableName, App))
    catch
        _X:_Y ->
            {error, table_not_exists_in_ets}
    end.

send_to_server(CallType, AppInfo, HashKey, Msg, SpecfyType)
    when is_record(AppInfo, conmero_app_info) ->

    case AppInfo#conmero_app_info.call_algorithm_type of
        0->
            send_to_direct_server(CallType, AppInfo, Msg, SpecfyType);
        1->
            send_to_consistent_server(CallType,
                                      AppInfo,
                                      HashKey,
                                      Msg,
                                      SpecfyType);
        _->
            {error, no_algo_handler_function}
    end;
send_to_server(_CallType, AppInfo,_HashKey,_Msg,_SpecfyType)->
    {error, AppInfo}.

get_specify_node(_CallType, undefined,  undefined)->{error,node_error};
get_specify_node(undefined, MasterNode, SlaveNode)->{MasterNode, SlaveNode};
get_specify_node(master,    undefined, _SlaveNode)->{error,node_error};
get_specify_node(master,    MasterNode,_SlaveNode)->MasterNode;
get_specify_node(slave,     MasterNode, undefined)->MasterNode;
get_specify_node(slave,    _MasterNode, SlaveNode)->SlaveNode.

send_to_direct_server(CallType, AppInfo, Msg, SpecfyType)->
    #conmero_app_info{
                    sync_func       = SyncFunc,
                    async_func      = ASyncFunc,
                    master_node     = MasterNode,
                    slave_node      = SlaveNode,
                    timeout         = Timeout,
                    switch_to_slave = Switch}
        = AppInfo,
    call_node_server(Switch, CallType, SyncFunc, ASyncFunc, MasterNode,
                     SlaveNode, Msg, SpecfyType, Timeout).

send_to_consistent_server(CallType, AppInfo, HashKey, Msg, SpecfyType)->
    case get_consistent_node(AppInfo#conmero_app_info.application, HashKey) of
        {ok, FinalAppNode} ->
            #conmero_app_node{
                        sync_func       = SyncFunc,
                        async_func      = ASyncFunc,
                        master_node     = MasterNode,
                        slave_node      = SlaveNode,
                        timeout         = Timeout,
                        switch_to_slave = Switch
                        }
                = FinalAppNode,
            call_node_server(Switch, CallType, SyncFunc, ASyncFunc, MasterNode,
                             SlaveNode, Msg, SpecfyType, Timeout);
        {error,Reason} ->
            {error,Reason}
    end.

call_node_server(Switch, CallType, SyncFunc, ASyncFunc, MasterNode, SlaveNode,
                 Msg, SpecfyType, Timeout)->
    case get_specify_node(SpecfyType, MasterNode, SlaveNode) of
        {error, Type} ->
            {error,Type};
        {MasterNode, SlaveNode} ->
            switch_server(Switch, CallType, SyncFunc, ASyncFunc, MasterNode,
                          SlaveNode, Msg, Timeout);
        CallingNode->
            calling_server(CallType, SyncFunc, ASyncFunc,
                           CallingNode, Msg, Timeout)
    end.

switch_server(0, CallType, SyncFunc, ASyncFunc, MasterNode,
              _SlaveNode, Msg, Timeout)->
    case calling_server(CallType, SyncFunc, ASyncFunc, MasterNode, Msg, Timeout) of
        {error,Reason}->
            {error,Reason};
        Return->
            Return
    end;
switch_server(1, CallType, SyncFunc, ASyncFunc, MasterNode,
              SlaveNode, Msg, Timeout)->
    case calling_server(CallType, SyncFunc, ASyncFunc, MasterNode, Msg, Timeout) of
        {ok,Ret}->
            {ok,Ret};
        {_E,_R}->
            calling_server(CallType, SyncFunc, ASyncFunc,
                           SlaveNode, Msg, Timeout);
        _Error->
            calling_server(CallType, SyncFunc, ASyncFunc,
                           SlaveNode, Msg, Timeout)
    end.

calling_server(call, SyncFunc,_ASyncFunc, CallingNode, Msg, Timeout)->
    try
        {ok,SyncFunc(CallingNode, Msg, Timeout)}
    catch
        TryE:TryR->
            {error,{TryE,TryR}}
    end;
calling_server(cast,_SyncFunc, ASyncFunc, CallingNode, Msg,_Timeout)->
    ASyncFunc(CallingNode, Msg).

get_consistent_node(App, Key)->
    TableName = get_table_name(App, node_info),
    case conmero_app_table:is_exist_ets_table(TableName) of
        true->
            KeyHash = conmero_manager:get_key_hash(Key),
            get_final_next_node(TableName, KeyHash);
        false->
            {error, application_not_exists};
        _->
            {error, get_node_failed}
    end.

get_final_next_node(TableName, KeyHash)->
    NodeHashNum =
        case ets:next(TableName, KeyHash) of
            '$end_of_table' ->
                ets:next(TableName, 0);
            Value ->
                Value
        end,
    {ok, hd(ets:lookup(TableName, NodeHashNum))}.

get_table_name(Application, node_info)->
    get_table_name(Application, node_info, ?ETS_NODES_PREFIX);
get_table_name(Application, app_info)->
    get_table_name(Application, app_info, ?ETS_APPS_PREFIX);
get_table_name(_Application,_AppProperty)->
    {error, input_property_error}.

get_table_name(App, AppProperty, TablePrefix)->
    case get({conmero, App, AppProperty}) of
        undefined->
            TableName = list_to_atom(lists:concat([TablePrefix, App])),
            put({conmero, App, AppProperty}, TableName),
            TableName;
        TableName->
            TableName
    end.

