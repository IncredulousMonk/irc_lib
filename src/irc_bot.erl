-module(irc_bot).

-behaviour(gen_statem).
-behaviour(irc_lib_client).

-export([start_link/1]).
-export([start/2, handle_message/4]).

-export([init/1, callback_mode/0, terminate/3]).
-export([initialising/3, connecting/3, login/3, joining/3, joined/3]).


%% Interface functions.

start_link(Config) ->
    gen_statem:start_link(?MODULE, Config, []).

-spec start(Name, Config) -> any() when
    Name :: string(),
    Config :: any().
start(Name, Config) ->
    irc_lib_sup:start_irc_session(Name, ?MODULE, Config).

handle_message(Pid, Verb, Source, Params) ->
    gen_statem:cast(Pid, {message, Verb, Source, Params}).


%% Callback functions.

-define(SERVER, <<"127.0.0.1">>).
-define(PORT, 6667).
-define(NICK, "erlbot").
-define(USE_SSL, false).

init(Config) ->
    logger:debug("IRC bot initialising."),
    process_flag(trap_exit, true),
    {ok, initialising, #{config => Config}}.

callback_mode() ->
    [state_functions, state_enter].

terminate(_Reason, _State, _Data) ->
    logger:debug("IRC bot terminating."),
    ok.


%% State functions.

initialising(enter, _, Data) ->
    {keep_state, Data};
initialising(info, {peer, Pid}, Data) ->
    logger:debug("Client Pid is ~p.", [Pid]),
    NewData = Data#{client => Pid},
    if is_map_key(ready, NewData) -> {next_state, connecting, maps:remove(ready, NewData)};
        true -> {keep_state, NewData}
    end;
initialising(info, ready, Data) ->
    logger:debug("Received ready message."),
    if is_map_key(client, Data) -> {next_state, connecting, Data};
        true -> {keep_state, Data#{ready => true}}
    end.

connecting(enter, _, Data) ->
    logger:debug("Entering the connecting state."),
    #{client := Pid} = Data,
    irc_lib_client:connect(Pid, ?SERVER, ?PORT, ?USE_SSL),
    {keep_state, Data};
connecting(info, connected, Data) ->
    logger:debug("Received connected message."),
    {next_state, login, Data}.

login(enter, _, Data) ->
    logger:debug("Entering the login state."),
    #{client := Pid} = Data,
    irc_lib_client:login(Pid, #{nick => <<?NICK>>}),
    {keep_state, Data};
login(cast, {message, "RPL_WELCOME" = Verb, Source, Params}, Data) ->
    [Nick | _] = Params,
    logger:info("Message: [~s] ~s, ~p", [Source, Verb, Params]),
    logger:notice("Server reports that my nick is ~s", [Nick]),
    NewData = Data#{nick => Nick},
    {keep_state, NewData};
login(cast, {message, "RPL_MYINFO" = Verb, Source, Params}, Data) ->
    logger:info("Message: [~s] ~s, ~p", [Source, Verb, Params]),
    #{client := Pid} = Data,
    irc_lib_client:join(Pid, <<"#chat">>),
    {next_state, joining, Data};
login(cast, {message, "ERR_NICKNAMEINUSE" = Verb, Source, Params}, Data) ->
    logger:info("Message: [~s] ~s, ~p", [Source, Verb, Params]),
    logger:notice("Server reports nickname already used."),
    Suffix = if
        is_map_key(suffix, Data) -> map_get(suffix, Data) + 1;
        true -> 1
    end,
    NewData = Data#{suffix => Suffix},
    SuffixBin = integer_to_binary(Suffix),
    NewNick = <<?NICK, SuffixBin/binary>>,
    #{client := Pid} = Data,
    irc_lib_client:login(Pid, #{nick => NewNick}),
    {keep_state, NewData};
login(cast, {message, "ERR_ERRONEUSNICKNAME" = Verb, Source, Params}, _Data) ->
    logger:info("Message: [~s] ~s, ~p", [Source, Verb, Params]),
    logger:error("Server reports invalid nickname."),
    {stop, normal};
login(cast, {message, "ERR_NICKCOLLISION" = Verb, Source, Params}, _Data) ->
    logger:info("Message: [~s] ~s, ~p", [Source, Verb, Params]),
    logger:error("Server reports nickname collision."),
    {stop, normal};
login(cast, {message, Verb, Source, Params}, Data) ->
    logger:info("Message: [~s] ~s, ~p", [Source, Verb, Params]),
    {keep_state, Data}.

joining(enter, _, Data) ->
    logger:debug("Entering the joining state."),
    {keep_state, Data};
joining(cast, {message, Verb, Source, Params}, Data) ->
    logger:info("Message: [~s] ~s, ~p", [Source, Verb, Params]),
    #{nick := Nick} = Data,
    [MessageNick | _] = irc_parser:split_nick(Source),
    UpperVerb = string:uppercase(Verb),
    if Nick =:= MessageNick andalso UpperVerb =:= "JOIN" ->
            [Channel | _] = Params,
            NewData = Data#{channel => Channel},
            {next_state, joined, NewData};
        true -> {keep_state, Data}
    end.

joined(enter, _, Data) ->
    logger:debug("Entering the joined state."),
    #{client := Pid, channel := Channel} = Data,
    irc_lib_client:send(Pid, list_to_binary(Channel), <<"erlbot is in your chat room, reading all your messages.">>),
    {keep_state, Data};
joined(cast, {message, "PRIVMSG" = Verb, Source, Params}, Data) ->
    logger:info("Message: [~s] ~s, ~p", [Source, Verb, Params]),
    #{nick := Nick, client := Pid} = Data,
    [Target | _] = Params,
    if Nick =:= Target ->
            [FromNick | _] = irc_parser:split_nick(Source),
            irc_lib_client:send(Pid, list_to_binary(FromNick), <<"Are you talking to me?">>);
        true -> ok
    end,
    {keep_state, Data};
joined(cast, {message, Verb, Source, Params}, Data) ->
    logger:info("Message: [~s] ~s, ~p", [Source, Verb, Params]),
    {keep_state, Data}.
