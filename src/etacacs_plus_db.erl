%%%-------------------------------------------------------------------
%%% @author Torbjorn Tornkvist <kruskakli@gmail.com>
%%% @copyright (C) 2023, Torbjorn Tornkvist
%%% @doc Config DB
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(etacacs_plus_db).

-behaviour(gen_server).

%% API
-export([start_link/0,
         authorize_user/2,
         login_user/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

%%-define(DEBUG, true).
-include("etacacs_plus.hrl").


-define(SERVER, ?MODULE).

-record(state,
        {db = []
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


login_user(User, Passwd)
  when is_binary(User) andalso is_binary(Passwd) ->
    UserStr = string:trim(binary_to_list(User), both, "\n"),
    PasswdStr = string:trim(binary_to_list(Passwd), both, "\n"),
    gen_server:call(?SERVER, {login, to_atom(UserStr), PasswdStr}).


authorize_user(User, Args0) when is_binary(User) ->
    UserStr = string:trim(binary_to_list(User), both, "\n"),
    Args = [{to_atom(K),to_atom(V)} || {K,V} <- Args0],
    gen_server:call(?SERVER, {authorize, to_atom(UserStr), Args}).


to_atom(A) when is_atom(A)   -> A;
to_atom(L) when is_list(L)   -> list_to_atom(L);
to_atom(B) when is_binary(B) ->
    list_to_atom(binary_to_list(B)).


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
    process_flag(trap_exit, true),
    {ok, DbConfFile} = application:get_env(etacacs_plus, db_conf_file),
    {ok, DB} = file:consult(DbConfFile),
    {ok, #state{db = DB}}.

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

handle_call({login, User, Passwd}, _From, State) ->
    Reply = do_login(State#state.db, User, Passwd),
    {reply, Reply, State};

handle_call({authorize, User, Args}, _From, State) ->
    Reply = do_authorize(State#state.db, User, Args),
    {reply, Reply, State};

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

do_login(DB, User, Passwd) ->
    ?debug("--- DB do_login, User=~p , Passwd=~p , DB=~p~n",[User,Passwd,DB]),
    L = [{U,D} || {user, U, D} <- DB,
                  U == User],

    {User, Data} = hd(L),  % just pick the first user...

    ?debug("--- DB do_login, User=~p , Data=~p~n",[User,Data]),

    case lists:keyfind(login, 1, Data) of
        {login, {cleartext, Str}} ->
            case match(Passwd, Str) of
                true ->
                    {ok, Data};
                false ->
                    {error, nomatch}
            end;
        _ ->
            {error, nopasswd}
    end.

match(X,X) ->
    true;
match(X,L) when is_binary(X) andalso is_list(L) ->
    match(X, list_to_binary(L));
match(_,_) ->
    false.



do_authorize(DB, User, Args) ->
    L = [{U,D} || {user, U, D} <- DB,
                  U == User],

    {User, Data} = hd(L),  % just pick the first user...

    %% NOTE: (atm) we only handle "service" authorization (i.e no "cmd" etc..)
    {service, Service} = lists:keyfind(service, 1, Args),

    case lists:keyfind(service, 1, Data) of
        {service, Service, ServiceData0} ->
            ServiceData = prepare_authorized_data(ServiceData0),
            {ok, ServiceData};
        _ ->
            {error, nomatch}
    end.

prepare_authorized_data([{groups,Gs} | T]) ->
    [list_to_binary("groups="++string:join([to_list(X) || X <- Gs], " "))
    | prepare_authorized_data(T)];
%%
prepare_authorized_data([{uid,Uid} | T]) ->
    [list_to_binary("uid="++to_list(Uid))
    | prepare_authorized_data(T)];
%%
prepare_authorized_data([{gid,Gid} | T]) ->
    [list_to_binary("gid="++to_list(Gid))
    | prepare_authorized_data(T)];
%%
prepare_authorized_data([{home,Home} | T]) ->
    [list_to_binary("home="++to_list(Home))
    | prepare_authorized_data(T)];
%%
prepare_authorized_data([_Ignore | T]) ->
    prepare_authorized_data(T);
%%
prepare_authorized_data([]) ->
    [].

to_list(L) when is_list(L)    -> L;
to_list(A) when is_atom(A)    -> atom_to_list(A);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(B) when is_binary(B)  -> binary_to_list(B).
