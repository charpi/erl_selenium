%%% Copyright (c) 2008-2009 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.

%% @author Nicolas Charpentier <open_source@charpi.net> [http://charpi.net]
%% @copyright 2008 Nicolas Charpentier
-module (selenium_session_tests).

-include_lib("eunit/include/eunit.hrl").

-export([default_server_test /1]).
-export([google_test /1]).
-export([keypress_test /1]).
-export([type_very_long_text_test /1]).
-export([utf8_test /1]).
-export([i18n_test /1]).

fast_test_() ->
    test_generator: launch_close (?MODULE).

default_server_test (Session) ->
    Start_url = "http://charpi.net/erl_selenium_test/tests/html/test_click_page1.html",
    Session: open (Start_url),
    
    {ok, "Click here for next page" ++ _Rest} = Session: get_text ("link"),
    
    {ok, StringLinks} = Session: get_all_links (),
    true = length (StringLinks) > 3,
    "linkToAnchorOnThisPage" = lists: nth (4, StringLinks),
    
    Session: click ( "link"),
    Session: wait_for_page_to_load ( "5000"),
    Session: click ( "previousPage"),
    Session: wait_for_page_to_load ( "5000"),
    ok.

google_test (Session) ->
    Session: open ( "http://www.google.com/webhp"),
    Session: wait_for_page_to_load ( "10000"),
    Session: type ( "q", "hello world"),
    Session: wait_for_page_to_load ( "10000"),
    Session: click ( "btnG"),
    Session: wait_for_page_to_load ( "10000"),
    {ok,"hello world" ++ _} = Session: get_title (),
    ok.

keypress_test (Session) ->
    InputId = "ac4",
    UpdateId = "ac4update",
    Ajax_url = "http://charpi.net/erl_selenium_test/tests/html/ajax/ajax_autocompleter2_test.html",
    Session: open ( Ajax_url),
    Session: key_press ( InputId, "74"),
    receive after 500 -> ok end,
    Session: key_press ( InputId, "97"),
    Session: key_press ( InputId, "110"),
    receive after 500 -> ok end,
    {ok, "Jane Agnews"} = Session: get_text ( UpdateId),
    Session: key_press( InputId, "13"),
    receive after 500 -> ok end,
    {ok, "Jane Agnews"} = Session: get_value ( InputId),
    ok.

type_very_long_text_test (Session) ->
    LongText = lists:duplicate (50000, $z), 
    Start_url = "http://charpi.net/erl_selenium_test/tests/html/test_rich_text.html",
    Session: open ( Start_url),
    Session: type ( "richtext", LongText),
    {ok, LongText} = Session: get_value ( "richtext"),
    ok.

utf8_test (Session) ->    
    Start_url = "http://charpi.net/erl_selenium_test/tests/html/test_editable.html",
    Session: open ( Start_url),
    Session: wait_for_page_to_load ("5000"),
    Object = "normal_text",
    Swedish = "Företag",
    String = [32479,19968,30721],
    Unicode = {unicode, binary_to_list(unicode:characters_to_binary(String))},
    Inputs = ["foo", Swedish, Unicode],
    F = fun(T) -> assert_set_value(T, Session, Object) end,
    lists: foreach (F, Inputs),
    ok.

assert_set_value({Encoding, Text}, Session, Object) ->
    Session:type(Object, Text),
    Unicode = binary_to_list(unicode:characters_to_binary(Text, Encoding, utf8)),
    {ok, Result} = Session:get_value(Object),
    {Text, Unicode} = {Text, Result};
assert_set_value(Text, Session, Object) ->
    assert_set_value({latin1, Text}, Session, Object).


i18n_test (Session) ->
    Start_url = "http://charpi.net/erl_selenium_test/tests/html/test_i18n.html",
    Session: open ( Start_url),
    Datas = [
	     {"romance", [252,246,228,220,214,196,32,231,232,233,32,191,241,32,232,224,249,242]},
	     {"korean", [50676,50640]},
	     {"chinese", [20013,25991]},
	     {"japanese", [12414,12407]},
	     {"dangerous", "&%?\\+|,%*"}],
    
    Test = fun({Id,Data}) ->
		   UTF8 = xmerl_ucs:to_utf8(Data),
		   Result = Session: is_text_present ({unicode,UTF8}),
		   {ok, "true"} = Result,
		   {ok, UTF8} = Session: get_text (Id)
	   end,
    lists:foreach(Test, Datas),
    ok.
