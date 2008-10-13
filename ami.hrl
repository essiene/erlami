-define(MARK, "PROCESSORMARK").
-define(CONNECT_OPTION, [list, inet, {active, false}]).

-define(SYM_SERVER_OPTION, [list, inet, {active, true}, {backlog, 10}, {reuseaddr, true}]).
-define(SYM_BANNER, "Asterisk Call Manager").
-define(SYM_VERSION, "1.0").
-define(SYM_REG_NAME, reg_amisym_server).
