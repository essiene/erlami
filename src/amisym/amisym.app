{application, amisym,
        [
            {description, "Asterisk AMI Symulator"},
            {vsn, "1.0"},
            {modules, [
                        gen_listener_tcp,
                        amisym_actions,
                        amisym_client_sup,
                        amisym,
                        amisym_event_bus,
                        amisym_interp,
                        amisym_server,
                        amisym_session,
                        amisym_sup
                      ]
            },
            {registered, [
                            amisym_server,
                            amisym_sup,
                            amisym_client_sup,
                            amisym_event_bus
                         ]
            },
            {applications, [
                            kernel, stdlib
                          ]
            },
            {mod, {amisym, []}},
            {env, []}
        ]
}.

