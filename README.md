CONMERO
=======
#### information
Conmero is a class library used for routing the nodes in a single computer or in the cluster. And sending a message by the call and cast way in gen_server or gen_server2 

#### config
If you want to direct to call or cast a server, setting config like this:

```erlang
{conmero, [
    {call_algorithm_type,   [direct]},
    {direct,                [log_engine]},
    {log_engine,[
        [
            %%% calling node id
            {id,                1},

            %%% conmero will generate two functions: mod_sender:call/3 and mod_sender:cast/2.
            %%% if this mod_sender has call function, your module must implement api function 
            %%% with 3 params (CallingNode, Msg, Timeout).
            %%% also cast funtion, but 2 params: (CallingNode, Msg).
            {mod_sender,        gen_server2},

            %%% the first calling node
            {master_node,       {'log_engine','log_engine1@henryhuang'}},

            %%% backup to call when master is error and also switch_to_slave must be on.
            {slave_node,        {'log_engine','log_engine2@henryhuang'}},

            %%% the switch is on that maybe calling slave_node.
            {switch_to_slave,   off},

            %%% calling node timeout.
            {timeout,           1000},

            %%% calling node's tag.
            {node_tag,          "1"},

            %%% calling node's status,online is in used.
            {node_status,       online}
        ]
    ]}
}.
```

If you want to get through consistent hashing and access to distributed nodes, setting config like this:

```erlang
{conmero, [
    {call_algorithm_type,   [consistent]},
    {consistent,            [log_engine2]},

    %%% the application in consistent list
    {log_engine2,[
        [
            {id,                1},
            {mod_sender,        gen_server2},
            {master_node,       {'log_engine','log_engine1@henryhuang'}},
            {slave_node,        undefined},
            {switch_to_slave,   off},
            {timeout,           2000},
            %%% hashkey = hash_base_key:v_node_[0,num]
            {hash_base_key,     "log_engine1"},
            {v_node_num,        100},
            {node_tag,          "1"},
            {node_status,       online}
        ],
        [
            {id,                3},
            {mod_sender,        gen_server2},
            {master_node,       {'log_engine','log_engine2@henryhuang'}},
            {slave_node,        undefined},
            {switch_to_slave,   off},
            {timeout,           2000},
            {hash_base_key,     "log_engine3"},
            {v_node_num,        100},
            {node_tag,          "3"},
            {node_status,       offline}
        ],
        [
            {id,                2},
            {mod_sender,        gen_server2},
            {master_node,       {'log_engine','log_engine2@henryhuang'}},
            {slave_node,        undefined},
            {switch_to_slave,   off},
            {timeout,           2000},
            {hash_base_key,     "log_engine2"},
            {v_node_num,        100},
            {node_tag,          "2"},
            {node_status,       online}
        ]
    ]}
]}.
```