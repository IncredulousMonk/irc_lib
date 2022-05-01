IRC client library for Erlang
=============================

irc_lib - is an IRC client library for Erlang applications.

This fork
=========

I have largely rewritten the original code, incorporating the improvements made in chooper's fork.

* Now a rebar3 application (install [rebar3](https://www.rebar3.org/) if you don't have it).
* Fixed the supervision tree.  Multiple bots may be run under one top-level supervisor.
* SSL connections work now.
* Changed logging from lager to the Erlang logger.


Usage
=====

Build
-----

    $ rebar3 compile

Run
---

    $ rebar3 shell

This will start the application's top-level supervisor, but no bots will run at this stage.

Running a bot
-------------

    irc_lib_sup:start_irc_session(Name, BotModule, BotConfig).

Where:
* Name is the name of the bot as a string.  This must be unique because it's used as an ID in the supervision tree.  It's not the IRC nick.
* BotModule is the name of the module that contains the bot code.
* BotConfig is any config term that you want to pass to the bot code, in the `start_link`.

The bot code
------------

The bot code will run under a supervisor, so it can be a `gen_server` or a `gen_statem`.

The code must implement the `irc_lib_client` behaviour:

```erlang
-behaviour(irc_lib_client).
```

This behaviour defines a `handle_message` callback, which will be called whenever a message is received from the IRC server:

```erlang
-callback handle_message(Pid :: pid(), Verb :: string(), Source :: string(), Params :: [string()]) -> any().
```

Where:
* Pid is the pid of your bot process (to be used in a `cast`, for example).
* Verb is the IRC message verb, as a string not a number (for example, "RPL_WELCOME" or "ERR_NICKNAMEINUSE").
* Source is the IRC message source.
* Params are the IRC message params, as a list of strings (may be an empty list).

After the bot process has been started it must wait for two `info` messages to be received before it tries to connect to an IRC server:

1. `{peer, Pid}`.  This message contains the pid of the irc_lib_client process.
2. `ready`.  This message is sent once the irc_lib_client process has started.

The order of these messages is not defined, because they are sent by different processes.

Example code
------------

See `irc_bot.erl` for an example of bot code that is a state machine using `gen_statem`.

Authors
=======

See the AUTHORS file.
