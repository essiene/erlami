-define(MARK, "PROCESSORMARK").
-define(CONNECT_OPTION, [list, inet, {active, false}]).

-define(SYM_SERVER_OPTION, [list, inet, {active, true}, {backlog, 10}, {reuseaddr, true}]).
-define(SYM_BANNER, "Asterisk Call Manager").
-define(SYM_VERSION, "1.0").
-define(SYM_REG_NAME, erlami_amisym).

-define(SYM_ACTION_FUNCTION_PREFIX, "a_").

-define(VARNAME_AMI_NAME, "__erlami-name").

-define(AMI_SOCKET_RETRY, 5000).

-record(client_session, {
		conn,
		username,
		secret,
		owner=undefined,
		data="",
		interp
	}).

-record(client_interp, {
        session,
        tid=0,
        evt_mngr=undefined,
        senders=undefined
    }).

-record(ami_socket_state, {
        host,
        port,
        opts,
        wait_retry,
        sock=undefined
    }).

-record(ami_socket, {
        pid
    }).
