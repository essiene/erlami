{application, erlami,
 [{description, "Asterisk AMI Library for Erlang"},
  {vsn, "0.1"},
  {modules, 
      [
          ami_interp, amievent,
          amilist, amitcp, 
          interp, messaging, 
          util, gen_listener_tcp,

          amisym_sup, amisym_server,
          amisym_client_sup, amisym_event_bus, 
          amisym_actions, amisym_interp, 
          amisym_session, amisym,

          amievent_manager,
          amiclient_event_handler,
          amiclient_interp,
          amiclient_session,
          ami
      ]
  },
  {registered, [ 
      amisym_server, amisym_sup,
      amisym_client_sup, amisym_event_bus
          ]
  },
  {mod, {amisym, []}},
  {env, []},
  {applications, [kernel, stdlib]}
 ]
}.
