%%%-------------------------------------------------------------------
%%% @author Torbjorn Tornkvist <kruskakli@gmail.com>
%%% @copyright (C) 2023, Torbjorn Tornkvist
%%% @doc TACACS+ according to RFC-8907.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(etacacs_plus_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).


%%-define(DEBUG, true).
-ifdef(DEBUG).
-define(debug(FmtStr,Args), io:format(FmtStr, Args)).
-else.
-define(debug(FmtStr,Args), ok).
-endif.


-define(SERVER, ?MODULE).

-define(PORT, 5049).
-define(SECRET_KEY, <<"tacacs123">>).

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

-define(is_set(Flags, Mask), ((Flags band Mask) > 0)).

-record(packet,
        { type
        , seq_no
        , flags
        , session_id
        , msg
        }).

-record(start_authentication,
        { action
        , priv_lvl
        , auth_type
        , auth_service
        , user
        , port
        , rem_addr
        , data
        }).

-record(authentication_continue,
       {
        state,
        user_msg,
        data
       }).


-record(state,
        {
         lpid
        }).

%% Worker state
-define(INIT, init).
-define(GET_PASS, get_pass).
-define(FINISHED, finished).
-record(wstate,
        {
         state = ?INIT
        }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
          {ok, State :: term(), Timeout :: timeout()} |
          {ok, State :: term(), hibernate} |
          {stop, Reason :: term()} |
          ignore.
init([]) ->
    erlang:process_flag(trap_exit, true),
    Self = self(),
    Pid = erlang:spawn_link(
            fun() ->
                    erlang:process_flag(trap_exit, true),
                    {ok, ListenSock} = gen_tcp:listen(get_port(),
                                                      [binary,
                                                       {active, false}]),
                    io:format("--- ~p: listening to port: ~p~n",[self(), get_port()]),
                    Lself = self(),
                    Apid = spawn_link(fun() -> acceptor(Lself, ListenSock) end),
                    lloop(Self, Apid, ListenSock)
            end),
    {ok, #state{lpid = Pid}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
          {reply, Reply :: term(), NewState :: term()} |
          {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
          {reply, Reply :: term(), NewState :: term(), hibernate} |
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
          {stop, Reason :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
          {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%
%% Listen server
%%
lloop(Controller, Acceptor, ListenSock) ->
    receive
        {Acceptor, connected} ->
            Self = self(),
            Apid = spawn_link(fun() -> acceptor(Self, ListenSock) end),
            lloop(Self, Apid, ListenSock);

        {'EXIT', Acceptor, _Reason} ->
            Self = self(),
            Apid = spawn_link(fun() -> acceptor(Self, ListenSock) end),
            lloop(Self, Apid, ListenSock);

        {'EXIT', Controller, _Reason} ->
            exit(shutdown)
    end.

get_port() ->
    ?PORT.

secret_key() ->
    ?SECRET_KEY.

%%
%% Worker process
%%
acceptor(LPid, ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    LPid ! {self(), connected},
    wloop(Socket, #wstate{}).

wloop(Socket, #wstate{state = ?FINISHED}) ->
    ?debug("--- worker ~p finished!~n",[self()]),
    gen_tcp:close(Socket),
    exit(normal);
%%
wloop(Socket, State) ->
    inet:setopts(Socket, [{active, once}]),
    receive

        {tcp, Socket, Msg} ->
            Packet = decode_packet(State, Msg),
            NewState = process_packet(Socket, State, Packet),
            wloop(Socket, NewState);

        {tcp_closed, Socket} ->
            exit(normal)

    end.

process_packet(Socket,
               #wstate{state = ?INIT} = State,
               #packet{msg = Msg = #start_authentication{}} = Req) ->

    Flags = SrvMsgLen = DataLen = 0,
    ReplyBody = <<?STATUS_GETPASS:8, Flags:8, SrvMsgLen:16, DataLen:16>>,

    Reply = mk_packet(?AUTHENTICATION,        % type
                      Req#packet.seq_no + 1,
                      0 ,                     % flags
                      Req#packet.session_id,
                      ReplyBody),

    ?debug("--- Sending reply packet: ~p~n",[Reply]),
    ok = gen_tcp:send(Socket, Reply),
    State#wstate{state = ?GET_PASS};
%%
process_packet(Socket,
               #wstate{state = ?GET_PASS} = State,
               #packet{msg = #authentication_continue{
                                user_msg = Passwd
                               }
                      } = Req) ->

    Status =
        case Passwd of
            <<"tacadmin">> ->
                ?STATUS_PASS;
            _ ->
                ?STATUS_FAIL
        end,

    Flags = SrvMsgLen = DataLen = 0,
    ReplyBody = <<Status:8, Flags:8, SrvMsgLen:16, DataLen:16>>,

    Reply = mk_packet(?AUTHENTICATION,        % type
                      Req#packet.seq_no + 1,
                      0 ,                     % flags
                      Req#packet.session_id,
                      ReplyBody),

    ?debug("--- Sending reply packet (Status=~p): ~p~n",[Status,Reply]),
    ok = gen_tcp:send(Socket, Reply),
    State#wstate{state = ?FINISHED}.


%%
%% Construct a packet (header + body)
%%
mk_packet(Type, SeqNo, Flags, SessionId, UnhashedBody) ->
    BodyLen = size(UnhashedBody),
    Version = <<16#c:4, 0:4>>,
    PseudoPad = pseudo_pad(BodyLen,
                           <<SessionId:32>>,
                           secret_key(),
                           Version,
                           <<SeqNo:8>>),

    Body = obfuscate(UnhashedBody, PseudoPad),

    <<Version/binary,
      Type:8,
      SeqNo:8,
      Flags:8,
      SessionId:32,
      BodyLen:32,
      Body/binary>>.


%%
%% Decode a packet (header + body)
%%
decode_packet(State,
              <<MajVsn:4,     % major TACACS+ version number (0xc)
                MinVsn:4,     % minor TACACS+ version number
                %%              default (0x0) , one (0x1)
                Type:8,       % type, Authentication (0x01)
                %%                    Authorization (0x02)
                %%                    Accounting (0x03)
                SeqNo:8,      % sequence number of the current packet
                Flags:8,      % various bitmapped flags
                %%              unencrypted_flag (0x01)
                %%              single_connect_flag (0x04)
                SessionId:32, % id for this TACACS+ session
                Length:32,    % total length of the packet body
                %%              (not including the header)
                Body0/binary>> % packet body
             ) ->

    ?debug("--- ~p worker got msg:~n"
              "  MajVsn = ~p~n"
              "  MinVsn = ~p~n"
              "  Type   = ~p~n"
              "  SeqNo  = ~p~n"
              "  Flags  = ~p~n"
              "  SessId = ~p~n"
              "  Length = ~p~n"
              "  Body0  = ~p~n",
              [self(),MajVsn,MinVsn,
               type_to_str(Type),
               SeqNo,Flags,SessionId,Length,Body0]
             ),

    Version = <<MajVsn:4, MinVsn:4>>,
    PseudoPad = pseudo_pad(Length,
                           <<SessionId:32>>,
                           secret_key(),
                           Version,
                           <<SeqNo:8>>),

    Body = deobfuscate(is_obfuscated(Flags), Body0, PseudoPad),

    Msg = decode_body(State, Type, Body),

    #packet{type = Type,
            seq_no = SeqNo,
            flags = Flags,
            session_id = SessionId,
            msg = Msg}.


is_obfuscated(Flags)  when (Flags band ?UNENCRYPTED_FLAG) == 1 -> false;
is_obfuscated(_Flags)                                          -> true.

deobfuscate(true, Body0, PseudoPad) ->
    obfuscate(Body0, PseudoPad);
deobfuscate(false, Body0, _PseudoPad) ->
    Body0.

obfuscate(<<B:8>>, <<P:8>>) ->
    X = B bxor P,
    <<X:8>>;
obfuscate(<<B:8,Brest/binary>>, <<P:8,Prest/binary>>) ->
    X = B bxor P,
    Bin = obfuscate(Brest, Prest),
    <<X:8, Bin/binary>>.


decode_body(#wstate{state = ?INIT},
            ?AUTHENTICATION,         % packet type
            <<Action:8,
              PrivLvl:8,
              AuthType:8,
              AuthService:8,
              UserLen:8,
              PortLen:8,
              RemAddrLen:8,
              DataLen:8,
              Rest/binary
            >>) ->

    ?debug("  ------~n"
           "  Action      = ~p~n"
           "  PrivLvl     = ~p~n"
           "  AuthType    = ~p~n"
           "  AuthService = ~p~n"
           "  UserLen     = ~p~n"
           "  PortLen     = ~p~n"
           "  RemAddrLen  = ~p~n"
           "  DataLen     = ~p~n"
           "  Rest        = ~p~n",
           [action_to_str(Action),
            PrivLvl,
            auth_type_to_str(AuthType),
            auth_service_to_str(AuthService),
            UserLen,
            PortLen,
            RemAddrLen,
            DataLen,
            %%User,
            Rest]),

    {Items, Left} =
        lists:mapfoldl(
          fun(Len, Bin) ->
                  get_item(Len, Bin)
          end,
          Rest,
          [UserLen, PortLen, RemAddrLen, DataLen]),

    [User,Port,RemAddr,Data] = Items,

    ?debug("  ------~n"
           "  User        = ~p~n"
           "  Port        = ~p~n"
           "  RemAddr     = ~p~n"
           "  Data        = ~p~n",
           [User, Port, RemAddr, Data]),

    #start_authentication{action = Action,
                          priv_lvl = PrivLvl,
                          auth_type = AuthType,
                          auth_service = AuthService,
                          user = User,
                          port = Port,
                          rem_addr = RemAddr,
                          data = Data
                         };
%%
decode_body(#wstate{state = ?GET_PASS},
            ?AUTHENTICATION,         % packet type
            <<UserMsgLen:16,
              DataLen:16,
              Flags:8,
              Rest/binary
            >>) ->

    if ?is_set(Flags, ?CONTINUE_FLAG_ABORT) ->
            io:format("...ABORTING...~n",[]),
            exit(abort); % FIXME do something better...
        true ->
            true
    end,

    {Items, Left} =
        lists:mapfoldl(
          fun(Len, Bin) ->
                  get_item(Len, Bin)
          end,
          Rest,
          [UserMsgLen, DataLen]),

    [UserMsg,Data] = Items,

    ?debug("  ------~n"
           "  Password    = ~p~n"
           "  Data        = ~p~n",
           [UserMsg , Data]),

    #authentication_continue{state = ?GET_PASS,
                             user_msg = UserMsg,
                             data = Data}.

%% If Length == 0 then no Item exist, else
%% extract the Item according to its Length.
get_item(0 = _ItemLen, Bin) ->
    {<<"">>, Bin};
get_item(ItemLen, Bin) ->
    <<Item:ItemLen/binary-unit:8, Rest/binary>> = Bin,
    {Item, Rest}.


%%
%% The pad is generated by concatenating a series of MD5 hashes
%% (each 16 bytes long) and truncating it to the length of the input data.
%%
%% The first MD5 hash is generated by concatenating the session_id,
%% the secret key, the version number, and the sequence number, and then
%% running MD5 over that stream.
%%
%% Subsequent hashes are generated by using the same input stream but
%% concatenating the previous hash value at the end of the input stream.
%%
pseudo_pad(Length, SessionId, SecretKey, Version, SeqNo)
  when is_binary(SessionId) andalso
       is_binary(SecretKey) andalso
       is_binary(Version) andalso
       is_binary(SeqNo) ->
    N = Length div 16,
    Unhashed = <<SessionId/binary,
                 SecretKey/binary,
                 Version/binary,
                 SeqNo/binary>>,
    X = erlang:md5(Unhashed),
    Res = pseudo_pad(N, SessionId, SecretKey, Version, SeqNo, [X]),
    B = list_to_binary(lists:reverse(Res)),
    <<Pad:Length/binary-unit:8,_/binary>> = B,
    Pad.

pseudo_pad(0, _SessionId, _SecretKey, _Version, _SeqNo, Acc) ->
    Acc;
pseudo_pad(N, SessionId, SecretKey, Version, SeqNo, [MDn_1|_] = Acc) ->
    X = erlang:md5(<<SessionId/binary,
                   SecretKey/binary,
                   Version/binary,
                   SeqNo/binary,
                   MDn_1/binary>>),
    pseudo_pad(N-1, SessionId, SecretKey, Version, SeqNo, [X|Acc]).



bin2hexstring(Bin) ->
    [io_lib:format("0x~2.16.0b ", [Byte]) || Byte <- erlang:binary_to_list(Bin)].


type_to_str(1) -> "Authenticate";
type_to_str(2) -> "Authorize";
type_to_str(3) -> "Accounting".

action_to_str(1) -> "LOGIN";
action_to_str(2) -> "CHPASS";
action_to_str(4) -> "SENDAUTH".

auth_type_to_str(1) -> "ASCII";
auth_type_to_str(2) -> "PAP";
auth_type_to_str(3) -> "CHAP";
auth_type_to_str(5) -> "MSCHAP";
auth_type_to_str(6) -> "MSCHAPV2".

auth_service_to_str(0) -> "NONE";
auth_service_to_str(1) -> "LOGIN";
auth_service_to_str(2) -> "ENABLE";
auth_service_to_str(3) -> "PPP";
auth_service_to_str(5) -> "PT";
auth_service_to_str(6) -> "RCMD";
auth_service_to_str(7) -> "X25";
auth_service_to_str(8) -> "NASI";
auth_service_to_str(9) -> "FWPROXY".
