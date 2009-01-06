{application, erlami,
 [{description, "Asterisk AMI Library for Erlang"},
  {vsn, "0.1"},
  {modules, 
      [
          ami_interp, amievent,
          amilist, amitcp, 
          interp, messaging, 
          util,

          amisym_sup, amisym_server,
          amisym_bus, amisym_actions,
          amisym_interp, amisym_session,
          amisym,

          amievent_manager,
          amiclient_event_handler,
          amiclient_interp,
          amiclient_session,
          ami
      ]
  },
  {registered, []},
  {env, []},
  {applications, [kernel, stdlib]}
 ]
}.
