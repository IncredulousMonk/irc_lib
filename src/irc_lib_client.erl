%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Irc client with ssl support.
%%% @end
%%%-----------------------------------------------------------------------------
-module(irc_lib_client).

-behaviour(gen_server).

-export([start_link/1]).

%% Public API
-export([connect/4, login/2, join/2, join/3, part/2, send/3, quit/1, quit/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-callback handle_message(Pid :: pid(), Verb :: string(), Source :: string(), Params :: [string()]) -> any().

-type login() :: #{
    'nick' := binary(),
    'password' => binary(),
    'username' => binary(),
    'realname' => binary()
}.


%% Interface functions.

-spec start_link(atom()) -> any().
start_link(CallbackModule) ->
    gen_server:start_link(?MODULE, CallbackModule, []).

-spec connect(Pid, Server, Port, Encrypted) -> any() when
    Pid :: pid(),
    Server :: binary(),
    Port :: pos_integer(),
    Encrypted :: boolean().
connect(Pid, Server, Port, Encrypted) ->
    gen_server:cast(Pid, {connect, Server, Port, Encrypted}).

-spec login(Pid, Details) -> any() when
    Pid :: pid(),
    Details :: login().
login(Pid, Details) ->
    gen_server:cast(Pid, {login, Details}).

-spec join(Pid, Channel) -> any() when
    Pid :: pid(),
    Channel :: binary().
join(Pid, Channel) ->
    gen_server:cast(Pid, {join, Channel}).

-spec join(Pid, Channel, Key) -> any() when
    Pid :: pid(),
    Channel :: binary(),
    Key :: binary().
join(Pid, Channel, Key) ->
    gen_server:cast(Pid, {join, Channel, Key}).

-spec part(Pid, Channel) -> any() when
    Pid :: pid(),
    Channel :: binary().
part(Pid, Channel) ->
    gen_server:cast(Pid, {part, Channel}).

-spec send(Pid, Target, Message) -> any() when
    Pid :: pid(),
    Target :: binary(),
    Message :: binary().
send(Pid, Target, Message) ->
    gen_server:cast(Pid, {send, Target, Message}).

-spec quit(Pid :: pid()) -> 'ok'.
quit(Pid) ->
    gen_server:cast(Pid, quit).

-spec quit(Pid :: pid(), Message :: string()) -> 'ok'.
quit(Pid, Message) ->
    gen_server:cast(Pid, {quit, Message}).


%% Callback functions.

init(CallbackModule) ->
    logger:debug("IRC client initialising."),
    process_flag(trap_exit, true),
    % Initialise process internal state.
    State = #{
        callback_module => CallbackModule,
        socket_mod => undefined,
        socket => undefined,
        ms_per_line => 250,
        last_line_sent => 0,
        bot => undefined
    },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({connect, Server, Port, Encrypted}, State) ->
    SocketMod = case Encrypted of
        true -> ssl;
        false -> gen_tcp
    end,
    Options = case SocketMod of
        ssl -> [{delay_send, false}, {verify, verify_none}, {nodelay, true}, {packet, line}, {mode, list}];
        gen_tcp -> [{delay_send, false}, {nodelay, true}, {packet, line}, {mode, list}]
    end,
    case SocketMod:connect(binary_to_list(Server), Port, Options) of
        {ok, Socket} ->
            #{bot := Pid} = State,
            NewState = State#{socket_mod := SocketMod, socket := Socket},
            Pid ! connected,
            {noreply, NewState};
        {error, Reason} ->
            logger:error("Unable to connect to IRC server with reason ~p", [Reason]),
            % Give up.
            {stop, normal, State}
    end;

handle_cast({login, Details}, State) ->
    #{socket_mod := SocketMod, socket := Socket} = State,
    #{nick := Nick} = Details,
    if is_map_key(password, Details) ->
            SocketMod:send(Socket, "PASS " ++ binary_to_list(map_get(password, Details)) ++ "\r\n");
        true -> ok
    end,
    Username = if is_map_key(username, Details) -> map_get(username, Details); true -> <<"erlbot">> end,
    Realname = if is_map_key(realname, Details) -> map_get(realname, Details); true -> <<"Erlang IRC bot">> end,
    SocketMod:send(Socket, "NICK " ++ binary_to_list(Nick) ++ "\r\n"),
    SocketMod:send(Socket, "USER " ++ binary_to_list(Username) ++ " 0 * :" ++ binary_to_list(Realname) ++ "\r\n"),
    {noreply, State};

handle_cast({join, Channel}, State) ->
    #{socket_mod := SocketMod, socket := Socket} = State,
    SocketMod:send(Socket, "JOIN " ++ binary_to_list(Channel) ++ "\r\n"),
    {noreply, State};

handle_cast({join, Channel, Key}, State) ->
    #{socket_mod := SocketMod, socket := Socket} = State,
    SocketMod:send(Socket, "JOIN " ++ binary_to_list(Channel) ++ " " ++ binary_to_list(Key) ++ "\r\n"),
    {noreply, State};

handle_cast({part, Channel}, State) ->
    #{socket_mod := SocketMod, socket := Socket} = State,
    SocketMod:send(Socket, "PART " ++ binary_to_list(Channel) ++ "\r\n"),
    {noreply, State};

handle_cast({send, Target, Message}, State) ->
    limited_send("PRIVMSG " ++ binary_to_list(Target) ++ " :" ++ binary_to_list(Message), State);

handle_cast(quit, State) ->
    #{socket_mod := SocketMod, socket := Socket} = State,
    SocketMod:send(Socket, "QUIT\r\n"),
    {noreply, State};

handle_cast({quit, Message}, State) ->
    #{socket_mod := SocketMod, socket := Socket} = State,
    SocketMod:send(Socket, "QUIT :" ++ Message ++ "\r\n"),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({peer, Pid}, State) ->
    logger:debug("Bot Pid is ~p.", [Pid]),
    NewState = State#{bot := Pid},
    Pid ! ready,
    {noreply, NewState};

handle_info({join, Socket, Message}, State) ->
    #{socket_mod := SocketMod} = State,
    SocketMod:send(Socket, Message),
    {noreply, State};

handle_info({ssl_closed, _Socket}, State) ->
    logger:info("Socket closed"),
    {stop, normal, State};

handle_info({ssl_error, _Socket, Reason}, State) ->
    logger:error("ssl_error: ~p", [Reason]),
    {stop, {ssl_error, Reason}, State};

handle_info({tcp_closed, _Socket}, State) ->
    logger:info("Socket closed"),
    {stop, normal, State};

handle_info({tcp_error, _Socket, Reason}, State) ->
    logger:error("tcp_error: ~p", [Reason]),
    {stop, {tcp_error, Reason}, State};

%% @doc Incoming message
handle_info({_, _Socket, Data}, State) ->
    #{callback_module := CallbackModule, bot := Pid, socket_mod := SocketMod, socket := Socket} = State,
    % Parse incoming data
    {Source, Code, Params} = irc_parser:parse_irc(Data),
    Verb = irc_command_codes:translate_command(Code),
    if Verb =:= "PING" ->
            [Token] = Params,
            SocketMod:send(Socket, "PONG :" ++ Token ++ "\r\n");
        Verb =/= "PING" ->
            CallbackModule:handle_message(Pid, Verb, Source, Params)
    end,
    {noreply, State};

handle_info(Args, State) ->
    logger:warning("Wildcard handle_info: ~p", [Args]),
    {noreply, State}.

terminate(_Reason, State) ->
    logger:debug("IRC client terminating."),
    #{socket := Socket, socket_mod := SocketMod} = State,
    % Check active socket
    case Socket of
        undefined ->
            ok;
        _ ->
            SocketMod:send(Socket, "QUIT :Session off\r\n"),
            SocketMod:shutdown(Socket, write)
    end,
    % terminate
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal functions

%% rate limiting
limited_send(Line, State) ->
    limited_send(Line, State, now_ms()).

%% rate limit green
limited_send(Line, #{last_line_sent := LastSent, ms_per_line := MsPerLine, socket_mod := SocketMod, socket := Socket} = State, Now) when Now - LastSent >= MsPerLine ->
    logger:debug("Sending raw: ~p", [Line]),
    SocketMod:send(Socket, Line ++ "\r\n"),
    {noreply, State#{last_line_sent := Now}};

%% rate limit yellow
limited_send(Line, State, Now) ->
    #{last_line_sent := LastSent} = State,
    Delay = Now - LastSent,
    logger:debug("Raw send rate limited, sleeping ~p", [Delay]),
    timer:sleep(Delay),
    limited_send(Line, State).

%% get current time in milliseconds
now_ms() ->
    {NowMegaSecs, NowSecs, NowMicroSecs} = os:timestamp(),
    (((NowMegaSecs * 1000000) + NowSecs) * 1000) + round(NowMicroSecs/1000).
