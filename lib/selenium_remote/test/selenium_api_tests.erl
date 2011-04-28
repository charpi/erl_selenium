%%% Copyright (c) 2008-2009 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.

%% @author Nicolas Charpentier <open_source@charpi.net> [http://charpi.net]
%% @copyright 2008 Nicolas Charpentier
-module (selenium_api_tests).

-include_lib("eunit/include/eunit.hrl").

-export([default_server_test /1]).
-export([google_test /1]).
-export([keypress_test /1]).
-export([type_very_long_text_test /1]).
-export([utf8_test /1]).
-export([i18n_test /1]).

fast_test_() ->
    test_generator: start_stop (?MODULE).

default_server_test (Session) ->
    Start_url = "/selenium-server/tests/html/test_click_page1.html",
    selenium_api:open(Session, Start_url),

    {ok, "Click here for next page" ++ _Rest} = selenium_api:get_text (Session,"link"),

    {ok, StringLinks} = selenium_api: get_all_links (Session),
    true = length (StringLinks) > 3,
    "linkToAnchorOnThisPage" = lists: nth (4, StringLinks),

    selenium_api: click (Session, "link"),
    selenium_api: wait_for_page_to_load (Session, "5000"),
    selenium_api: click (Session, "previousPage"),
    selenium_api: wait_for_page_to_load (Session, "5000"),
    ok.

google_test (Session) ->
    selenium_api: open (Session, "http://www.google.com/webhp"),
    selenium_api: wait_for_page_to_load (Session, "10000"),
    selenium_api: type (Session, "q", "hello world"),
    selenium_api: wait_for_page_to_load (Session, "10000"),
    selenium_api: click (Session, "btnG"),
    selenium_api: wait_for_page_to_load (Session, "10000"),
    {ok,"hello world" ++ _} = selenium_api: get_title (Session),
    ok.

keypress_test (Session) ->
    InputId = "ac4",
    UpdateId = "ac4update",
    Ajax_url = "http://localhost:4444/selenium-server/tests/html/ajax/ajax_autocompleter2_test.html",
    selenium_api: open (Session, Ajax_url),
    selenium_api: key_press (Session, InputId, "74"),
    receive after 500 -> ok end,
    selenium_api: key_press (Session, InputId, "97"),
    selenium_api: key_press (Session, InputId, "110"),
    receive after 500 -> ok end,
    {ok, "Jane Agnews"} = selenium_api: get_text (Session, UpdateId),
    selenium_api: key_press(Session, InputId, "13"),
    receive after 500 -> ok end,
    {ok, "Jane Agnews"} = selenium_api: get_value (Session, InputId),
    ok.

type_very_long_text_test (Session) ->
    Start_url = "/selenium-server/tests/html/test_rich_text.html",
    LongText = lists: duplicate (50000, $z), 
    selenium_api: open (Session, Start_url),
    selenium_api: type (Session, "richtext", LongText),
    {ok, LongText} = selenium_api: get_value (Session, "richtext"),
    ok.

utf8_test (Session) ->    
    Start_url = "/selenium-server/tests/html/test_editable.html",
    selenium_api: open (Session, Start_url),
    selenium_api: wait_for_page_to_load(Session, "5000"),
    Object = "normal_text",
    Swedish = "Företag",
    String = [32479,19968,30721],
    Unicode = {unicode, binary_to_list(unicode:characters_to_binary(String))},
    Inputs = ["foo", Swedish, Unicode],
    F = fun(T) -> assert_set_value(T, Session, Object) end,
    lists: foreach (F, Inputs),
    ok.

assert_set_value({Encoding, Text}, Session, Object) ->
    selenium: cmd (Session, type, [Object, Text]),
    Unicode = binary_to_list(unicode:characters_to_binary(Text, Encoding, utf8)),
    {ok, Result} = selenium_api: get_value(Session, Object),
    {Text, Unicode} = {Text, Result};
assert_set_value(Text, Session, Object) ->
    assert_set_value({latin1, Text}, Session, Object).

i18n_test (Session) ->
    Start_url = "/selenium-server/tests/html/test_i18n.html",
    selenium_api: open (Session, Start_url),
    Datas = [
	     {"romance", [252,246,228,220,214,196,32,231,232,233,32,191,241,32,232,224,249,242]},
	     {"korean", [50676,50640]},
	     {"chinese", [20013,25991]},
	     {"japanese", [12414,12407]},
	     {"dangerous", "&%?\\+|,%*"}],

    Test = fun({Id,Data}) ->
		   UTF8 = xmerl_ucs:to_utf8(Data),
		   Result = selenium_api: is_text_present (Session, {unicode,UTF8}),
		   {ok, "true"} = Result,
		   {ok, UTF8} = selenium_api: get_text (Session, Id)
	   end,
    lists:foreach(Test, Datas),
    ok.
