%%%-------------------------------------------------------------------
%%% @author Torbjorn Tornkvist <kruskakli@gmail.com>
%%% @copyright (C) 2023, Torbjorn Tornkvist
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(server_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SERVER, etacacs_plus_server).


%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Returns list of tuples to set default properties
%%  for the suite.
%%
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%
%% @spec suite() -> Info
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{minutes,10}}].

%%--------------------------------------------------------------------
%% @doc
%% Initialization before the whole suite
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ok = write_config("db.conf"),
    ok = application:load(etacacs_plus),
    ok = application:set_env(etacacs_plus, listen_ip, {0,0,0,0}),
    ok = application:set_env(etacacs_plus, port, 5049),
    ok = application:set_env(etacacs_plus, key, "tacacs123"),
    ok = application:set_env(etacacs_plus, db_conf_file, "db.conf"),
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after the whole suite
%%
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case
%%
%% TestCase - atom()
%%   Name of the test case that is about to be run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case
%%
%% TestCase - atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of test case group definitions.
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% @spec: groups() -> [Group]
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%%  Returns the list of groups and test cases that
%%  are to be executed.
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% @end
%%--------------------------------------------------------------------
all() ->
    [ start_stop_server
    , authenticate
    , authorize
    ].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Test case info function - returns list of tuples to set
%%  properties for the test case.
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: This function is only meant to be used to return a list of
%% values, not perform any other operations.
%%
%% @spec TestCase() -> Info
%% @end
%%--------------------------------------------------------------------
start_stop_server() ->
    [].

%%--------------------------------------------------------------------
%% @doc Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
start_stop_server(_Config) ->

    %% Start the server and give it some runtime
    ok = application:start(etacacs_plus),
    timer:sleep(1000),

    %% Verify the server is up and running
    Msg = hello,
    ?assertEqual(ok, gen_server:call(?SERVER, Msg)),

    %% Verify that the server is listening
    IpPortState = ip_port_state(),
    ?assertEqual(true, is_listening({0,0,0,0}, 5049, IpPortState)),

    %% Stop the server and give it some runtime
    application:stop(etacacs_plus),
    timer:sleep(1000),

    %% Verify that the server is *not* listening
    IpPortState2 = ip_port_state(),
    ?assertEqual(false, is_listening({0,0,0,0}, 5049, IpPortState2)),

    ok.



authenticate(_Config) ->

    %% Start the server and give it some runtime
    ok = application:start(etacacs_plus),
    timer:sleep(1000),

    X = os:cmd("tacacs_client -v -H 127.0.0.1 -p 5049 -k tacacs123 "
               "-u tacadmin authenticate  --password tacadmin"),

    ?assertEqual("status: PASS\n", X),

    %% Stop the server and give it some runtime
    application:stop(etacacs_plus),
    timer:sleep(1000),

    ok.

authorize(_Config) ->

    %% Start the server and give it some runtime
    ok = application:start(etacacs_plus),
    timer:sleep(1000),

    %% Authorize the use of service: nso
    X = os:cmd("tacacs_client -v -H 127.0.0.1 -p 5049 -k tacacs123 "
               "-u tacadmin authorize  -c service=nso"),

    ?assertEqual("status: PASS\n"
                 "av-pairs:\n"
                 "  groups=admin netadmin private\n"
                 "  uid=1000\n"
                 "  gid=100\n"
                 "  home=/tmp\n"
                 , X),

    %% Authorize the use of (the unknown) service: hello
    Y = os:cmd("tacacs_client -v -H 127.0.0.1 -p 5049 -k tacacs123 "
               "-u tacadmin authorize  -c service=hello"),

    ?assertEqual("status: FAIL\n", Y),

    %% Stop the server and give it some runtime
    application:stop(etacacs_plus),
    timer:sleep(1000),

    ok.





%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

is_listening(Ip, Port, [{Ip, Port, States} | Tail]) ->
    case lists:member(listen, States) of
        true ->
            true;
        false ->
            is_listening(Ip, Port, Tail)
    end;
is_listening(Ip, Port, [_ | Tail]) ->
    is_listening(Ip, Port, Tail);
is_listening(_, _, []) ->
    false.





%%
%% ip_port_state/1
%%
%% Example:
%% 1> ip_port_state().
%% [{{0,0,0,0},44679,[accepting,listen,open]},
%%  {{127,0,0,1},48530,[connected,open]},
%%  {{127,0,0,1},9999,[accepting,listen,open]}]
%%
ip_port_state() ->
    TcpSockets = tcp_sockets(),
    ok([{prim_inet:sockname(S),prim_inet:getstatus(S)} || S <- TcpSockets]).

%% Unbox and remove non-ok values
ok([{{ok,{IP,Port}},{ok,State}} | Tail]) -> [{IP,Port,State} | ok(Tail)];
ok([_ | Tail])                           -> ok(Tail);
ok([])                                   -> [].

%% Return a list of all tcp sockets
tcp_sockets() ->
    port_list("tcp_inet").

%% Return all ports having the name 'Name'
port_list(Name) ->
    lists:filter(
      fun(Port) ->
	      case erlang:port_info(Port, name) of
		  {name, Name} -> true;
		  _ -> false
	      end
      end, erlang:ports()).



write_config(Fname) ->
    write_config(Fname, config()).

write_config(Fname, Config) ->
    {ok, Fd} = file:open(Fname, [write]),
    try
        [io:fwrite(Fd, "~p.~n", [C]) || C <- Config],
        ok
    after
        file:close(Fd)
    end.


config() ->
[{user, tacadmin,
 [{login, {cleartext, "tacadmin"}},
  {service, nso,
   [{groups, [admin, netadmin, private]},
    {uid, 1000},
    {gid, 100},
    {home, "/tmp"}
   ]
  },
  {member, [netadmin]}
 ]
},
{user, tester,
 [{login, {cleartext, "tester"}},
  {service, nso,
   [{groups, [admin, private]},
    {uid, 1001},
    {gid, 101},
    {home, "/home/tester"}
   ]
  },
  {member, admin}
 ]
},
{user, operator,
 [{login, {cleartext, "operator"}},
  {service, nso,
   [{groups, [oper, public]},
    {uid, 1002},
    {gid, 102},
    {home, "/operator"}
   ]
  },
  {member, oper}
 ]
},
{user, admin,
 [{login, {cleartext, "blaffa"}},
  {service, nso,
   [{groups, [admin]},
    {uid, 1003},
    {gid, 103},
    {home, "/tmp"}
   ]
  },
  {member, oper}
 ]
}].
