%%%-------------------------------------------------------------------
%% @doc irc_lib top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(irc_lib_sup).

-behaviour(supervisor).

-export([start_link/0, start_irc_session/3]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
%% @doc Start a new IRC session.
%% @end
-spec start_irc_session(Name, BotModule, BotConfig) -> any() when
    Name :: string(),
    BotModule :: atom(),
    BotConfig :: any().
start_irc_session(Name, BotModule, BotConfig) ->
    Child = #{
        id => Name ++ "_session",
        start => {irc_session_sup, start_link, [Name, BotModule, BotConfig]},
        restart => temporary
    },
    {ok, SessionPid} = supervisor:start_child(?MODULE, Child),
    irc_session_sup:connect_children(SessionPid).

init([]) ->
    logger:debug("Top-level supervisor initialising."),
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 60
    },
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
