%%% Copyright (c) 2008-2009 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.

%% @author Nicolas Charpentier <open_source@charpi.net> [http://charpi.net]
%% @copyright 2008 Nicolas Charpentier
-module (selenium_session_tests).

-include_lib("eunit/include/eunit.hrl").

-export ([fast_tests/0]).

fast_tests () ->
    [fun default_server_test/1,
     fun i18n_test/1,
     fun utf8_test/1,
     fun type_very_long_text_test/1,
     fun keypress_test/1,
     fun google_test/1].

default_server_test (Session) ->
    Start_url = "/selenium-server/tests/html/test_click_page1.html",
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
    Session: type ( "q", "hello world"),
    Session: click ( "btnG"),
    Session: wait_for_page_to_load ( "5000"),
    {ok,"hello world - Google Search"} = Session: get_title (),
    ok.

keypress_test (Session) ->
    InputId = "ac4",
    UpdateId = "ac4update",
    Ajax_url = "http://localhost:4444/selenium-server/tests/html/ajax/ajax_autocompleter2_test.html",
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
    Start_url = "/selenium-server/tests/html/test_rich_text.html",
    Session: open ( Start_url),
    Session: type ( "richtext", LongText),
    {ok, LongText} = Session: get_value ( "richtext"),
    ok.

utf8_test (Session) ->    
    Start_url = "/selenium-server/tests/html/test_editable.html",
    Session: open ( Start_url),
    Session: wait_for_page_to_load ("5000"),
    Object = "normal_text",
    String = [85,110,105,99,111,100,101,32479,19968,30721],
    Test = fun(Text) ->
		   Session: type (Object, Text),
		   {ok, Text} = Session: get_value (Object)
	   end,
    Inputs = ["foo", xmerl_ucs: to_utf8 (String)],
    lists: foreach (Test, Inputs),
    ok.

i18n_test (Session) ->
    Start_url = "/selenium-server/tests/html/test_i18n.html",
    Session: open ( Start_url),
    Datas = [
	     {"romance", [252,246,228,220,214,196,32,231,232,233,32,191,241,32,232,224,249,242]},
	     {"korean", [50676,50640]},
	     {"chinese", [20013,25991]},
	     {"japanese", [12414,12407]},
	     {"dangerous", "&%?\\+|,%*"}],
    
    
    Test = fun({Id,Data}) ->
		   UTF8 = xmerl_ucs:to_utf8(Data),
		   Result = Session: is_text_present ( UTF8),
		   {ok, "true"} = Result,
		   {ok, UTF8} = Session: get_text ( Id)
	   end,
    lists:foreach(Test, Datas),
    ok.
