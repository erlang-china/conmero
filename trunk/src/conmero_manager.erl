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


-module(conmero_manager).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../include/conmero.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, start/0, stop/0]).
-export([get_key_hash/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start()->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE,stop).


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
    load_config(),
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
load_config()->
    {ok, Filename} = application:get_env(config),
    {ok, Terms}    = file:consult(Filename),
    CallTypeList   = proplists:get_value(call_type, Terms),
    [begin
         AppsList = proplists:get_value(CallType, Terms),
         init_apps(CallType, Terms, AppsList)
     end
    || CallType<-CallTypeList].

init_apps(_CallType,_Terms, [])->ok;
init_apps(_CallType,_Terms, undefined)->ok;
init_apps( CallType, Terms, AppsList)->
    [begin 
         ServerNodes = proplists:get_value(App, Terms),
         insert_app_into_ets(CallType, App, ServerNodes)
     end||App<-AppsList].


generate_app(_CallType,_NodeTableName, _App, undefined)->ok;
generate_app(_CallType,_NodeTableName, _App, [])->ok;
generate_app( general,  NodeTableName,  App, [ServerNode|T])->
    case proplists:get_value(id, ServerNode) of
        Id when Id=:=1->
            AppConfig = generate_general_app(App, ServerNode),
            ets:insert(NodeTableName,AppConfig);
        _->
            generate_app(general, NodeTableName, App, T)
    end;
generate_app(consistent, NodeTableName, App, [ServerNode|T])->
    AppConfig = generate_consistent_app(App, ServerNode),
    ets:insert(NodeTableName,AppConfig),
    generate_app(consistent, NodeTableName, App, T).

generate_general_app(App, ServerNode)->
    GenServerType = proplists:get_value(gen_server_type,ServerNode),
    MasterNode    = proplists:get_value(master_node,    ServerNode),
    SlaveNode     = proplists:get_value(slave_node,     ServerNode),
    ServerName    = proplists:get_value(server_name,    ServerNode),
    SourceTag     = proplists:get_value(source_tag,     ServerNode),
    Timeout       = proplists:get_value(timeout,        ServerNode),
    Status        = proplists:get_value(status,         ServerNode),
    #conmero_app_info{
                      application     = App,
                      call_type       = general,
                      gen_server_type = GenServerType,
                      master_node     = MasterNode,
                      slave_node      = SlaveNode,
                      server_name     = ServerName,
                      timeout         = Timeout,
                      source_tag      = SourceTag,
                      status          = Status}.
generate_consistent_app(App, ServerNode)->
    Id            = proplists:get_value(id, ServerNode),
    GenServerType = proplists:get_value(gen_server_type,ServerNode),
    MasterNode    = proplists:get_value(master_node,    ServerNode),
    SlaveNode     = proplists:get_value(slave_node,     ServerNode),
    ServerName    = proplists:get_value(server_name,    ServerNode),
    VNodeNums     = proplists:get_value(v_node_num,     ServerNode),
    HashBaseKey   = proplists:get_value(hash_base_key,  ServerNode),
    SourceTag     = proplists:get_value(source_tag,     ServerNode),
    Timeout       = proplists:get_value(timeout,        ServerNode),
    Status        = proplists:get_value(status,         ServerNode),
    
    AppNode=
    #conmero_app_info{
                      id              = Id,
                      application     = App,
                      call_type       = consistent,
                      gen_server_type = GenServerType,
                      master_node     = MasterNode,
                      slave_node      = SlaveNode,
                      server_name     = ServerName,
                      v_node_num      = VNodeNums,
                      hash_base_key   = HashBaseKey,
                      timeout         = Timeout,
                      source_tag      = SourceTag,
                      status          = Status},
    
    ConmeroNodes = make_nodes(Id, MasterNode, ServerName, VNodeNums, HashBaseKey, SourceTag, Timeout, Status),
    TableName    = conmero_app_table:create_node_table(App),
    insert_nodes_to_ets(TableName,ConmeroNodes),
    
    AppNode.

make_nodes( Id, Node, ServerName, VNodeNums, HashBaseKey, SourceTag, Timeout, Status)
  when is_integer(Id),is_atom(Node),is_atom(ServerName),
       is_integer(VNodeNums),VNodeNums>0,is_list(SourceTag),is_integer(Timeout),is_atom(Status)->
    make_nodes_1(Id,Node,ServerName,0,VNodeNums,HashBaseKey,SourceTag,Timeout,Status,[]);
make_nodes(_Id,_Node,_ServerName,_VNodeNums,_HashBaseKey,_SourceTag,_Timeout,_Status)->
    [].
    
make_nodes_1(_Id,_Node,_ServerName,_BaseVNode, 0,_HashBaseKey,_SourceTag,_Timeout,_Status, AccOut)-> AccOut;
make_nodes_1(_Id,_Node,_ServerName, BaseVNode, VNodeId,_HashBaseKey,_SourceTag,_Timeout,_Status, AccOut) 
  when BaseVNode==VNodeId -> AccOut;
make_nodes_1( Id, Node, ServerName, BaseVNode, VNodeId, HashBaseKey, SourceTag, Timeout, Status, AccOut)->
    Key     = io_lib:format("~s:~b",[HashBaseKey,VNodeId]),
    KeyHash = get_key_hash(Key),
    NodeRec = #conmero_node{
                            id          = Id,
                            v_node_id   = VNodeId,
                            node        = Node,
                            server_name = ServerName,
                            timeout     = 1000,
                            hash        = KeyHash,
                            status      = Status,
                            source_tag  = SourceTag},
    make_nodes_1(Id,Node,ServerName,BaseVNode,VNodeId-1,HashBaseKey,SourceTag,Timeout,Status,[NodeRec|AccOut]).

insert_app_into_ets(general, App, ServerNodes)->
    NodeTableName=conmero_app_table:create_app_table(App),
    generate_app(general, NodeTableName, App, ServerNodes);
insert_app_into_ets(consistent, App, ServerNodes)->
    NodeTableName=conmero_app_table:create_app_table(App),
    generate_app(consistent, NodeTableName, App, ServerNodes).


insert_nodes_to_ets(_TableName,[])->ok;
insert_nodes_to_ets( TableName,[NodeRec|T])
  when is_record(NodeRec,conmero_node)->
    ets:insert(TableName,NodeRec),
    insert_nodes_to_ets(TableName,T).






get_key_hash(Key)->
    ByteKeyHash                 = erlang:md5(Key),
    <<KeyHash:32/big,_/binary>> = ByteKeyHash,
    KeyHash.






