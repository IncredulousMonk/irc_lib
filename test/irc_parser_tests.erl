-module(irc_parser_tests).
-include_lib("eunit/include/eunit.hrl").

%% Manual tests

parse_irc_trailing_test() ->
    %% test parsing of irc lines with trailing args
    {Prefix, Cmd, Args} = irc_parser:parse_irc(":test!test@test.com PRIVMSG #testchan :hello there tester"),
    ?assertEqual("test!test@test.com",                  Prefix),
    ?assertEqual("PRIVMSG",                             Cmd),
    ?assertEqual(["#testchan", "hello there tester"],   Args).

parse_irc_notrailing_test() ->
    %% test parsing of irc lines without trailing args
    {Prefix, Cmd, Args} = irc_parser:parse_irc(":WiZ!jto@tolsun.oulu.fi NICK Kilroy"),
    ?assertEqual("WiZ!jto@tolsun.oulu.fi",  Prefix),
    ?assertEqual("NICK",                    Cmd),
    ?assertEqual(["Kilroy"],                Args).

parse_irc_trailingonly_test() ->
    %% test parsing of irc lines that only contain a trailing arg
    {Prefix, Cmd, Args} = irc_parser:parse_irc(":syrk!kalt@millennium.stealth.net QUIT :Gone to have lunch"),
    ?assertEqual("syrk!kalt@millennium.stealth.net",    Prefix),
    ?assertEqual("QUIT",                                Cmd),
    ?assertEqual(["Gone to have lunch"],                Args).

parse_irc_binary_test() ->
    %% check that parse_irc returns the same results for lists and bit strings
    Line = ":test!test@test.com PRIVMSG #testchan :hello there tester",
    ListResult = irc_parser:parse_irc(Line),
    BinaryResult = irc_parser:parse_irc(list_to_binary(Line)),
    ?assertEqual(ListResult, BinaryResult).


%% Tests generated from https://github.com/ircdocs/parser-tests/blob/master/tests/msg-split.yaml

parse_1_test() ->
    Input = "foo bar baz asdf",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("", Source),
    ?assertEqual("foo", Verb),
    ?assertEqual(["bar","baz","asdf"], Params).

parse_2_test() ->
    Input = ":coolguy foo bar baz asdf",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("coolguy", Source),
    ?assertEqual("foo", Verb),
    ?assertEqual(["bar","baz","asdf"], Params).

parse_3_test() ->
    Input = "foo bar baz :asdf quux",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("", Source),
    ?assertEqual("foo", Verb),
    ?assertEqual(["bar","baz","asdf quux"], Params).

parse_4_test() ->
    Input = "foo bar baz :",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("", Source),
    ?assertEqual("foo", Verb),
    ?assertEqual(["bar","baz",[]], Params).

parse_5_test() ->
    Input = "foo bar baz ::asdf",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("", Source),
    ?assertEqual("foo", Verb),
    ?assertEqual(["bar","baz",":asdf"], Params).

parse_6_test() ->
    Input = ":coolguy foo bar baz :asdf quux",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("coolguy", Source),
    ?assertEqual("foo", Verb),
    ?assertEqual(["bar","baz","asdf quux"], Params).

parse_7_test() ->
    Input = ":coolguy foo bar baz :  asdf quux ",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("coolguy", Source),
    ?assertEqual("foo", Verb),
    ?assertEqual(["bar","baz","  asdf quux "], Params).

parse_8_test() ->
    Input = ":coolguy PRIVMSG bar :lol :) ",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("coolguy", Source),
    ?assertEqual("PRIVMSG", Verb),
    ?assertEqual(["bar","lol :) "], Params).

parse_9_test() ->
    Input = ":coolguy foo bar baz :",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("coolguy", Source),
    ?assertEqual("foo", Verb),
    ?assertEqual(["bar","baz",[]], Params).

parse_10_test() ->
    Input = ":coolguy foo bar baz :  ",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("coolguy", Source),
    ?assertEqual("foo", Verb),
    ?assertEqual(["bar","baz","  "], Params).

% Skipping test 11 (parsing of tags not supported)

% Skipping test 12 (parsing of tags not supported)

% Skipping test 13 (parsing of tags not supported)

parse_14_test() ->
    Input = ":src JOIN #chan",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("src", Source),
    ?assertEqual("JOIN", Verb),
    ?assertEqual(["#chan"], Params).

parse_15_test() ->
    Input = ":src JOIN :#chan",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("src", Source),
    ?assertEqual("JOIN", Verb),
    ?assertEqual(["#chan"], Params).

parse_16_test() ->
    Input = ":src AWAY",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("src", Source),
    ?assertEqual("AWAY", Verb),
    ?assertEqual([], Params).

parse_17_test() ->
    Input = ":src AWAY ",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("src", Source),
    ?assertEqual("AWAY", Verb),
    ?assertEqual([], Params).

parse_18_test() ->
    Input = ":cool\tguy foo bar baz",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("cool	guy", Source),
    ?assertEqual("foo", Verb),
    ?assertEqual(["bar","baz"], Params).

parse_19_test() ->
    Input = [58,99,111,111,108,103,117,121,33,97,103,64,110,101,116,3,53,119,
             3,111,114,107,46,97,100,109,105,110,32,80,82,73,86,77,83,71,32,
             102,111,111,32,58,98,97,114,32,98,97,122],
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("coolguy!ag@net5work.admin", Source),
    ?assertEqual("PRIVMSG", Verb),
    ?assertEqual(["foo","bar baz"], Params).

parse_20_test() ->
    Input = [58,99,111,111,108,103,117,121,33,126,97,103,64,110,2,101,116,3,
             48,53,119,15,111,114,107,46,97,100,109,105,110,32,80,82,73,86,77,
             83,71,32,102,111,111,32,58,98,97,114,32,98,97,122],
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("coolguy!~ag@net05work.admin", Source),
    ?assertEqual("PRIVMSG", Verb),
    ?assertEqual(["foo","bar baz"], Params).

% Skipping test 21 (parsing of tags not supported)

parse_22_test() ->
    Input = ":irc.example.com COMMAND param1 param2 :param3 param3",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("irc.example.com", Source),
    ?assertEqual("COMMAND", Verb),
    ?assertEqual(["param1","param2","param3 param3"], Params).

% Skipping test 23 (parsing of tags not supported)

parse_24_test() ->
    Input = "COMMAND",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("", Source),
    ?assertEqual("COMMAND", Verb),
    ?assertEqual([], Params).

% Skipping test 25 (parsing of tags not supported)

parse_26_test() ->
    Input = ":gravel.mozilla.org 432  #momo :Erroneous Nickname: Illegal characters",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("gravel.mozilla.org", Source),
    ?assertEqual("432", Verb),
    ?assertEqual(["#momo","Erroneous Nickname: Illegal characters"], Params).

parse_27_test() ->
    Input = ":gravel.mozilla.org MODE #tckk +n ",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("gravel.mozilla.org", Source),
    ?assertEqual("MODE", Verb),
    ?assertEqual(["#tckk","+n"], Params).

parse_28_test() ->
    Input = ":services.esper.net MODE #foo-bar +o foobar  ",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("services.esper.net", Source),
    ?assertEqual("MODE", Verb),
    ?assertEqual(["#foo-bar","+o","foobar"], Params).

% Skipping test 29 (parsing of tags not supported)

% Skipping test 30 (parsing of tags not supported)

% Skipping test 31 (parsing of tags not supported)

% Skipping test 32 (parsing of tags not supported)

% Skipping test 33 (parsing of tags not supported)

parse_34_test() ->
    Input = ":SomeOp MODE #channel :+i",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("SomeOp", Source),
    ?assertEqual("MODE", Verb),
    ?assertEqual(["#channel","+i"], Params).

parse_35_test() ->
    Input = ":SomeOp MODE #channel +oo SomeUser :AnotherUser",
    {Source, Verb, Params} = irc_parser:parse_irc(Input),
    ?assertEqual("SomeOp", Source),
    ?assertEqual("MODE", Verb),
    ?assertEqual(["#channel","+oo","SomeUser","AnotherUser"], Params).

