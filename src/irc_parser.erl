-module(irc_parser).
-include("proto.hrl").
-export([
    split_nick/1,
    parse_irc/1,
    parse_irc/2
]).
-compile([{parse_transform, lager_transform}]).
-include_lib("eunit/include/eunit.hrl").

%% Utility function to split how a nick (nick!ident@host)
split_nick(FullNick) ->
    [_FromNick, _IdentHost] = re:split(FullNick, "!", [{parts, 2}, {return, binary}]).

%% https://github.com/npwolf/erlbot/blob/master/src/irc_router.erl
%% Parse irc messages like Python's Twisted http://stackoverflow.com/questions/930700/python-parsing-irc-messages
%% parse_irc(<<":test!~test@test.com PRIVMSG #channel :Hey there">>)
%% #irc_msg{prefix = <<"test!~test@test.com">>, cmd = <<"PRIVMSG">>, args = [<<"#channel">>, <<"Hey there">>]}
parse_irc(Data) when is_list(Data) ->
    parse_irc(list_to_binary(Data));
parse_irc(<< ":", Rest/binary >>) ->
    [Prefix, NewRest] = re:split(Rest, " ", [{parts, 2}, {return, binary}]),
    parse_irc(Prefix, NewRest);
parse_irc(Rest) ->
    parse_irc(<<>>, Rest).

parse_irc(Prefix, Rest) ->
    RestStripped = binary:replace(Rest, <<"\r\n">>, <<>>, [global]),
    case re:run(RestStripped, " :") of 
        {match, _} ->
            [NewRest, Trailing] = re:split(RestStripped, " :", [{parts, 2}, {return, binary}]),
            ArgParts = re:split(NewRest, "\s+", [{return, binary}]),
            Args = [ binary_to_list(A) || A <- lists:append(ArgParts, [Trailing]) ];
        _ ->
            Args = [ binary_to_list(A) || A <- re:split(RestStripped, "\s+", [{return, binary}]) ]
    end,
    [Command|FinalArgs] = Args,
    PrefixList = binary_to_list(Prefix),
    lager:debug("Prefix '~p' Command '~p' Args '~p'~n", [PrefixList, Command, FinalArgs]),
    %%#irc_msg{prefix=Prefix, cmd=Command, args=FinalArgs}.
    #irc_strings{prefix=PrefixList, cmd=Command, args=FinalArgs}.

%% tests

parse_irc_trailing_test() ->
    %% test parsing of irc lines with trailing args
    Result = parse_irc(":test!test@test.com PRIVMSG #testchan :hello there tester"),
    ?assertEqual("test!test@test.com",                  Result#irc_strings.prefix),
    ?assertEqual("PRIVMSG",                             Result#irc_strings.cmd),
    ?assertEqual(["#testchan", "hello there tester"],   Result#irc_strings.args).

parse_irc_notrailing_test() ->
    %% test parsing of irc lines without trailing args
    Result = parse_irc(":WiZ!jto@tolsun.oulu.fi NICK Kilroy"),
    ?assertEqual("WiZ!jto@tolsun.oulu.fi",  Result#irc_strings.prefix),
    ?assertEqual("NICK",                    Result#irc_strings.cmd),
    ?assertEqual(["Kilroy"],                Result#irc_strings.args).

parse_irc_trailingonly_test() ->
    %% test parsing of irc lines that only contain a trailing arg
    Result = parse_irc(":syrk!kalt@millennium.stealth.net QUIT :Gone to have lunch"),
    ?assertEqual("syrk!kalt@millennium.stealth.net",    Result#irc_strings.prefix),
    ?assertEqual("QUIT",                                Result#irc_strings.cmd),
    ?assertEqual(["Gone to have lunch"],                Result#irc_strings.args).

parse_irc_binary_test() ->
    %% check that parse_irc returns the same results for lists and bit strings
    Line = ":test!test@test.com PRIVMSG #testchan :hello there tester",
    ListResult = parse_irc(Line),
    BinaryResult = parse_irc(list_to_binary(Line)),
    ?assertEqual(ListResult, BinaryResult).

