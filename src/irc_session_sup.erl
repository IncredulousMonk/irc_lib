%%%-------------------------------------------------------------------
%% @doc IRC session supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(irc_session_sup).

-behaviour(supervisor).

-export([start_link/3, connect_children/1]).
-export([init/1]).

-spec start_link(string(), atom(), any()) -> any().
start_link(Name, BotModule, BotConfig) ->
    supervisor:start_link(?MODULE, {Name, BotModule, BotConfig}).

connect_children(Pid) ->
    [{_, Child1, _, _}, {_, Child2, _, _}] = supervisor:which_children(Pid),
    Child1 ! {peer, Child2},
    Child2 ! {peer, Child1},
    ok.

init({Name, BotModule, BotConfig}) ->
    logger:debug("Session supervisor initialising"),
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 60,
        auto_shutdown => any_significant
    },
    Client = #{
        id => Name ++ "_client",
        start => {irc_lib_client, start_link, [BotModule]},
        restart => transient,
        significant => true
    },
    Bot = #{
        id => Name ++ "_bot",
        start => {BotModule, start_link, [BotConfig]},
        restart => transient,
        significant => true
    },
    ChildSpecs = [Client, Bot],
    {ok, {SupFlags, ChildSpecs}}.
