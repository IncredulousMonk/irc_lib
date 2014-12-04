%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Irc client with ssl support.
%%% @end
%%%-----------------------------------------------------------------------------
-module(irc_lib_client).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

-export([start_link/7]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% irc client state
-record(state, {
    % irc nick
    login = <<>> :: binary(),
    % irc server host
    host = <<>> :: binary(),
    % irc server port
    port = 0 :: integer(),
    % irc server password
    password = <<>> :: binary(),
    % channels
    channels = [] :: list({binary(), binary()}),
    % irc connection socket
    socket = null,
    % socket manager
    socket_mod = ssl :: atom(),
    % auth or not
    is_auth = false :: boolean(),
    % calback module
    callback = null,
    % reconnect timeout
    reconnect_timeout = 0 :: integer(),
    % rate limit
    ms_per_line = 250 :: integer(),
    % when last line was sent (in ms since epoch)
    last_line_sent = 0 :: integer()
    }).

-include("proto.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(TIMEOUT, 15000).

start_link(CallbackModule, Host, Port, SocketMod, ChanList, Nick, ReconnectTimeout) ->
    gen_server:start_link(?MODULE, [CallbackModule, Host, Port, SocketMod, ChanList, Nick, ReconnectTimeout], []).

init([CallbackModule, Host0, Port, SocketMod, ChanList, Nick, ReconnectTimeout]) ->
    % Get host and password
    {Host, Pass} = Host0,
    % try to connect
    gen_server:cast(self(), {connect, Host, Port}),
    % init process internal state
    {ok, #state{login = Nick, host = Host, password = Pass, channels = ChanList, port = Port,
                socket_mod = SocketMod, callback = CallbackModule, reconnect_timeout = ReconnectTimeout}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc Try to connect to irc server and join to channel
handle_cast({connect, Host, Port}, State) ->
    % Try to connect to irc server
    Options = case State#state.socket_mod of
      ssl -> [{delay_send, false}, {verify, 0}, {nodelay, true}];
      gen_tcp -> [{delay_send, false}, {nodelay, true}, {packet, line}, {mode, list}]
    end,
    case (State#state.socket_mod):connect(binary_to_list(Host), Port, Options) of
        {ok, Socket} ->
            irc_connect(Socket, State),
            {noreply, State#state{socket = Socket, is_auth = false}};
        {error, Reason} ->
            % Some log
            lager:error("Unable to connect to irc server with reason ~s", [Reason]),
            % Try reconnect
            try_reconnect(State)
        end;

%% Send message to irc
handle_cast({send_message, From, Message}, State) ->
    % Split messages by \r\n
    MessagesList = string:tokens(Message, "\r\n"),
    % Check private message or public
    case From of
        {channel, Chan} ->
            % Send messages
            lists:foreach(fun(Mes) ->
                              timer:sleep(200),
                              % Send message to irc
                              (State#state.socket_mod):send(State#state.socket, "PRIVMSG " ++ Chan ++ " :" ++ Mes ++ "\r\n")
                          end, 
                          MessagesList);
        {user, FullUser} ->
            % Send messages
            lists:foreach(fun(Mes) ->
                              timer:sleep(200),
                              % Get username
                              [UserName | _] = string:tokens(FullUser, "!"),
                              % Send private message to irc
                              (State#state.socket_mod):send(State#state.socket, "PRIVMSG " ++ UserName ++ " :" ++ Mes ++ "\r\n")
                          end, 
                          MessagesList)
    end,
    % return
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({join, Socket, Message}, State) ->
    (State#state.socket_mod):send(Socket, Message),
    {noreply, State};

handle_info({ssl_closed, Reason}, State) ->
    lager:info("ssl_closed with reason: ~p~n", [Reason]),
    try_reconnect(State);

handle_info({ssl_error, _Socket, Reason}, State) ->
    lager:error("tcp_error: ~p~n", [Reason]),
    try_reconnect(State);

handle_info({tcp_closed, Reason}, State) ->
    lager:info("tcp_closed with reason: ~p~n", [Reason]),
    try_reconnect(State);

handle_info({tcp_error, _Socket, Reason}, State) ->
    lager:error("tcp_error: ~p~n", [Reason]),
    try_reconnect(State);

%% @doc reconnect
handle_info(reconnect, State) ->
    % Close socket
    (State#state.socket_mod):close(State#state.socket),
    % Try reconnect
    gen_server:cast(self(), {connect, State#state.host, State#state.port}),
    % return
    {noreply, State};

%% @doc Incoming message
handle_info({_, Socket, Data}, State) ->
    % Parse incoming data
    Record = irc_parser:parse_irc(Data),

    %% Send the raw line
    State#state.callback ! {irc_line, Record},

    %% Do some client-local handling
    case Record of
        %%
        %% see https://www.alien.net.au/irc/irc2numerics.html or RFC 1459
        %%

        %% ping pong
        #irc_strings{cmd = "PING", args = [PongHost]} ->
            (State#state.socket_mod):send(Socket, "PONG " ++ PongHost ++ "\r\n"),
            {noreply, State};

        %% error
        #irc_strings{cmd = "ERROR", args = Err} ->
            lager:error("Error: ~p", [Err]),
            try_reconnect(State);

        %% on connection established
        #irc_strings{cmd = "004"} ->
            lager:info("Trying to join channels"),
            [ join_channel(State#state.socket_mod, Socket, Chan, ChanKey) || {Chan, ChanKey} <- State#state.channels ],
            {noreply, State};

        %% wrong server
        #irc_strings{cmd = "402"} ->
            lager:info("Wrong server address ~p", [State#state.host]);

        %% no such channel
        #irc_strings{cmd = "403", args = Body} ->
            %{match, [Chan]} = re:run(Body, "\#(.*) :"),
            lager:info("Wrong channel ~p", [Body]);

        %% invalid nickanme given
        #irc_strings{cmd = "432"} ->
            lager:info("No nickname given");

        %% nick name in use
        #irc_strings{cmd = "433"} ->
            lager:info("This nickname already in use"),

            % Make new nickname
            NewNickName = binary_to_list(State#state.login) ++ integer_to_list(lists:last(binary_to_list(State#state.login)) + 1),

            % try reconnect with new name
            try_reconnect(State#state{login = list_to_binary(NewNickName), socket = Socket});

        %% PRIVMSG
        #irc_strings{cmd = "PRIVMSG", args = [To|IncomingMessage], prefix = From} ->

            % Check private message or not
            [Symb | _] = To,
            % Check the first symbol   
            case Symb of
                % this is public message
                $# ->
                    % Send incomming message to callback
                    State#state.callback ! {incoming_message, IncomingMessage, {channel, To}};
                % this is private message
                _ ->
                    State#state.callback ! {incoming_message, IncomingMessage, {user, From}}
            end,
            % return
            {noreply, State#state{socket = Socket}};

        %% anything we didn't handle
        Line ->
            lager:debug("Unhandled message: ~p", [Line]),
            % return
            {noreply, State#state{socket = Socket}}
    end;

handle_info({raw, <<Line>>}, State) ->
    handle_info({raw, binary_to_list(Line)},State);

handle_info({raw, Line}, State) ->
    limited_send(Line, State);

handle_info(Args, State) ->
    lager:warning("Wildcard handle_info: ~p", [Args]),
    {noreply, State}.

%% rate limiting
limited_send(Line, State) ->
    limited_send(Line, State, now_ms()).

%% rate limit green
limited_send(Line, State, Now) when Now - State#state.last_line_sent >= State#state.ms_per_line ->
    lager:debug("Sending raw: ~p", [Line]),
    (State#state.socket_mod):send(State#state.socket, Line ++ "\r\n"),
    {noreply, State#state{last_line_sent = Now}};

%% rate limit yellow
limited_send(Line, State, Now) ->
    Delay = Now - State#state.last_line_sent,
    lager:debug("Raw send rate limited, sleeping ~p", [Delay]),
    timer:sleep(Delay),
    limited_send(Line, State).

terminate(_Reason, State) ->
    % Check active socket
    case State#state.socket of
        null ->
            ok;
        _ ->
            case State#state.is_auth of
                false ->
                    ok;
                _ ->
                    (State#state.socket_mod):send(State#state.socket, "QUIT :Session off \r\n")
            end
    end,
    % terminate
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions

%% get current time in milliseconds
now_ms() ->
    {NowMegaSecs, NowSecs, NowMicroSecs} = os:timestamp(),
    (((NowMegaSecs * 1000000) + NowSecs) * 1000) + round(NowMicroSecs/1000).

irc_connect(Socket, State) ->
    do_connect(State#state.socket_mod, Socket, State#state.password, State#state.login).

do_connect(Mod, Socket, Pass, Name) ->
    ok = pass_maybe(Mod, Socket, Pass),
    ok = sign_in(Mod, Socket, Name).

pass_maybe(_, _, <<>>) -> ok;
pass_maybe(M, Socket, Pass) when is_binary(Pass) -> 
    M:send(Socket, "PASS " ++ binary_to_list(Pass) ++ "\r\n").

sign_in(M, Socket, Name) ->
    M:send(Socket, "NICK " ++ binary_to_list(Name) ++ "\r\n"),
    M:send(Socket, "USER " ++ binary_to_list(Name) ++ " nohost noserver :Ybot\r\n").

join_channel(M, Socket, Chan, ChanKey) ->
    lager:info("Joining channel ~p", [Chan]),
    Delay = case M of 
      ssl -> ?TIMEOUT;
      _ -> 0
    end,
    (erlang:send_after(Delay, self(), {
        join, Socket, "JOIN " ++ binary_to_list(Chan) ++ " " ++ binary_to_list(ChanKey) ++ "\r\n"
    })),
    ok.

%% @doc try reconnect
-spec try_reconnect(State :: #state{}) -> {normal, stop, State} | {noreply, State}.
try_reconnect(#state{reconnect_timeout = Timeout} = State) ->
    case Timeout > 0 of
        false ->
            % no need in reconnect
            {stop, normal, State};
        true ->
            timer:send_after(Timeout, self(), reconnect),
            % return
            {noreply, State}
    end.

%% tests

now_ms_test() ->
    %% TODO this is stupid
    ?assert(0 < now_ms()).

