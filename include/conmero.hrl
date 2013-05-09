-record (conmero_params,{
                            application  = undefined,   %%% atom()
                            message      = "",          %%% The message to server
                            hashkey      = "",          %%% Consistent to a service need this value
                            specify_type = undefined    %%% master | slave
                        }).


-record (conmero_app_info, {
                            application         = undefined,    %%% atom()
                            call_algorithm_type = 0,            %%% 0 is direct|1 is consistent
                            sync_func           = undefined,    %%% mod_sender:call(Node,Msg,Timeout)
                            async_func          = undefined,    %%% mod_sender:cast(Node,Msg)
                            master_node         = undefined,    %%% first to call
                            slave_node          = undefined,    %%% backup to call
                            timeout             = 1000,         %%% call node timeout
                            switch_to_slave     = 0,            %%% on is 1| off is 0
                            v_node_nums         = 0,
                            hash_base_key       = "",
                            node_tag            = "1",          %%% calling node's tag
                            node_status         = 1             %%% 1 is online|0 is offline
                           }).

-record (conmero_app_node, {
                            id                  = 1,
                            application         = undefined,
                            sync_func           = undefined,
                            async_func          = undefined,
                            master_node         = undefined,
                            slave_node          = undefined,
                            timeout             = 1000,
                            switch_to_slave     = 0,
                            v_node_id           = 1,            %%% this virtual node's number
                            hash_index          = 0,            %%% this virtual node's hash value
                            node_tag            = "1",
                            node_status         = 1
                           }).


-define(ETS_APPS_PREFIX,   conmero_apps_dict_).
-define(ETS_NODES_PREFIX,  conmero_nodes_dict_).

