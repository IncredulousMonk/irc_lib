%%%-------------------------------------------------------------------
%% @doc irc_lib public API
%% @end
%%%-------------------------------------------------------------------

-module(irc_lib_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    configure_logger(),
    logger:debug("Starting application."),
    irc_lib_sup:start_link().

stop(_State) ->
    logger:debug("Application has stopped."),
    ok.

%% internal functions

configure_logger() ->
    ConsoleHandler = #{
        filters => [
            {progress, {fun logger_filters:progress/2, stop}}
        ],
        formatter => {
            logger_formatter, #{
                single_line => false,
                template => [time, " [", level, "] ", msg, "\n"]
            }
        }
    },
    FileHandler = ConsoleHandler#{config => #{file => "log/console.log"}},
    logger:set_handler_config(default, ConsoleHandler),
    logger:add_handler(file_log, logger_std_h, FileHandler),
    logger:set_primary_config(level, all).
