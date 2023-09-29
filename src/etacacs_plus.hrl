-ifndef(_ETACACS_PLUS_).
-define(_ETACACS_PLUS_, true).

-include_lib("kernel/include/logger.hrl").

-ifdef(DEBUG).
-define(debug(FmtStr,Args), io:format(FmtStr, Args)).
-else.
-define(debug(FmtStr,Args), ok).
-endif.

-define(ETACACS_LOG(MsgMap), ?LOG_INFO(MsgMap, #{etacacs_plus => true })).

%% Type of packet
-define(AUTHENTICATION, 16#1).
-define(AUTHORIZATION,  16#2).
-define(ACOUNTING,      16#3).

%% Reply status
-define(STATUS_PASS,    16#1).
-define(STATUS_FAIL,    16#2).
-define(STATUS_GETDATA, 16#3).
-define(STATUS_GETUSER, 16#4).
-define(STATUS_GETPASS, 16#5).
-define(STATUS_RESTART, 16#6).
-define(STATUS_ERROR,   16#7).
-define(STATUS_FOLLOW,  16#21).

%% Bitmapped flags
-define(CONTINUE_FLAG_ABORT, 16#1).
-define(UNENCRYPTED_FLAG, 16#1).

%% Authentication method used (when Authorizing)
-define(METH_NOT_SET,    16#0).
-define(METH_NONE,       16#1).
-define(METH_KRB5,       16#2).
-define(METH_LINE,       16#3).
-define(METH_ENABLE,     16#4).
-define(METH_LOCAL,      16#5).
-define(METH_TACACSPLUS, 16#6).
-define(METH_GUEST,      16#8).
-define(METH_RADIUS,     16#10).
-define(METH_KRB4,       16#11).
-define(METH_RCMD,       16#20).


%% Authorization status
-define(AUTHOR_STATUS_PASS_ADD, 16#1).
-define(AUTHOR_STATUS_FAIL, 16#10).
-define(AUTHOR_STATUS_ERROR, 16#11).


-define(is_set(Flags, Mask), ((Flags band Mask) > 0)).

-endif.
