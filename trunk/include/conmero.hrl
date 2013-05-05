-define(ETS_APPS_PREFIX,   conmero_apps_dict_).
-define(ETS_NODES_PREFIX,  conmero_nodes_dict_).


-record(conmero_app_info,{
                            id              = 1,
                            application     = undefined,
                            call_type       = undefined,
                            gen_server_type = undefined,
                            master_node     = undefined,
                            slave_node      = undefined,
                            server_name     = undefined,
                            timeout         = 1000,
                            v_node_num      = 0,
                            hash_base_key   = "",
                            source_tag      = "1",
                            status          = online
                         }).

-record(conmero_node,{
                        id          = 1,
                        v_node_id   = 1,
                        node        = undefined,
                        server_name = undefined,
                        timeout     = 1000,
                        hash        = 0,
                        source_tag  = "1",
                        status       = online
                     }).