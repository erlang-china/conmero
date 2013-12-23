-record (conmero_params,{
    application  = undefined :: atom(),   %% The application which you are calling to.
    message      = "",                    %% The message to be sent to application.
    hashkey      = ""        :: string(), %% Consistent services needs this key to get hashing number.
    specify_type = undefined :: atom()    %% master | slave
}).


-record (conmero_app_info, {
    application         = undefined :: atom(),      %% The application which you are calling to.
    call_algorithm_type = 0         :: integer(),   %% 0 is direct | 1 is consistent
    sync_func           = undefined :: function(),  %% mod_sender:call(Node, Msg, Timeout)
    async_func          = undefined :: function(),  %% mod_sender:cast(Node,Msg)
    master_node         = undefined :: atom(),      %% first to call
    slave_node          = undefined :: atom(),      %% backup to call
    timeout             = 1000      :: integer(),   %% call node timeout
    switch_to_slave     = 0         :: integer(),   %% on is 1| off is 0
    v_node_nums         = 0         :: integer(),
    hash_base_key       = ""        :: string(),
    node_tag            = "1"       :: string()     %% calling node's tag
}).

-record (conmero_app_node, {
    id              = 1         :: integer(),
    application     = undefined :: atom(),
    sync_func       = undefined :: function(),
    async_func      = undefined :: function(),
    master_node     = undefined :: atom(),
    slave_node      = undefined :: atom(),
    timeout         = 1000      :: integer(),
    switch_to_slave = 0         :: integer(),   %% 1 is need switch to call slave server when called master node error.
    v_node_id       = 1         :: integer(),   %% this virtual node's number
    node_tag        = "1"       :: string(),    %% calling node's tag
    hash_index      = 0         :: integer()    %% this virtual node's hash value
}).


-define(ETS_APPS_PREFIX,   conmero_apps_dict_).
-define(ETS_NODES_PREFIX,  conmero_nodes_dict_).

