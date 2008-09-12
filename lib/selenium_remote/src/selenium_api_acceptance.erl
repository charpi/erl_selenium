%%% Copyright (c) 2008 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.
-module (selenium_api_acceptance).

-compile ([export_all]).

-define (HOST,"localhost").
-define (PORT,4444).
-define (COMMAND,"*firefox\ /usr/lib/firefox/firefox-2-bin").

tests () ->
    default_server_test(),
    google_test(),
    keypress_test(),
    type_very_long_text_test(),
    utf8_test(),
    i18n_test(),
%%     high_level_test(),
    ok.


default_server_test () ->
    URL = "http://localhost:4444",
    Session = selenium: start (?HOST,
			       ?PORT,
			       ?COMMAND,
			       URL),
    Start_url = "/selenium-server/tests/html/test_click_page1.html",
    selenium_api:open(Session, Start_url),

    {ok, "Click here for next page" ++ _Rest} = selenium_api:get_text (Session,"link"),
    
    {ok, StringLinks} = selenium_api: get_all_links (Session),
    true = length (StringLinks) > 3,
    "linkToAnchorOnThisPage" = lists: nth (4, StringLinks),
    
    selenium_api: click (Session, "link"),
    selenium_api: wait_for_page_to_load (Session, "5000"),
    %%    Head ++ "/selenium-server/tests/html/test_click_page2.html" = selenium:cmd(get_location,Session),
    selenium_api: click (Session, "previousPage"),
    selenium_api: wait_for_page_to_load (Session, "5000"),
    %%    Head ++ "/selenium-server/tests/html/test_click_page1.html" = selenium:cmd(get_location,Session),
    selenium: stop (Session),
    ok.

google_test () ->
    URL = "http://www.google.com/webhp",
    Session = selenium: start (?HOST,
			       ?PORT,
			       ?COMMAND,
			       URL),
    selenium_api: open (Session, "http://www.google.com/webhp"),
    selenium_api: type (Session, "q", "hello world"),
    selenium_api: click (Session, "btnG"),
    selenium_api: wait_for_page_to_load (Session, "5000"),
    {ok,"hello world - Google Search"} = selenium_api: get_title (Session),
    selenium: stop (Session),
    ok.

keypress_test () ->
    InputId = "ac4",
    UpdateId = "ac4update",
    URL = "http://www.irian.at",
    Session = selenium: start (?HOST,
			       ?PORT,
			       ?COMMAND,
			       URL),
    Ajax_url = "http://www.irian.at/selenium-server/tests/html/ajax/ajax_autocompleter2_test.html",
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
    selenium: stop (Session).

type_very_long_text_test () ->
    URL = "http://localhost:4444",
    Session = selenium: start (?HOST,
			       ?PORT,
			       ?COMMAND,
			       URL),
    Start_url = "/selenium-server/tests/html/test_rich_text.html",
    LongText = lists: duplicate (50000, $z), 
    selenium_api: open (Session, Start_url),
    selenium_api: type (Session, "richtext", LongText),
    {ok, LongText} = selenium_api: get_value (Session, "richtext"),
    selenium: stop (Session).

utf8_test () ->    
    URL = "http://localhost:4444",
    Session = selenium: start (?HOST,
			       ?PORT,
			       ?COMMAND,
			       URL),
    Start_url = "/selenium-server/tests/html/test_editable.html",
    selenium_api: open (Session, Start_url),
    selenium_api: wait_for_page_to_load(Session, "5000"),
    Object = "normal_text",
    String = [85,110,105,99,111,100,101,32479,19968,30721],
    Test = fun(Text) ->
		   selenium_api: type (Session, Object, Text),
		   {ok, Text} = selenium_api: get_value (Session,Object)
	   end,
    Inputs = ["foo", xmerl_ucs: to_utf8 (String)],
    lists: foreach (Test, Inputs),
    selenium: stop (Session).

i18n_test () ->
    URL = "http://localhost:4444",
    Start_url = "/selenium-server/tests/html/test_i18n.html",
    Session = selenium: start (?HOST,
				?PORT,
				?COMMAND,
				URL),
    selenium_api: open (Session, Start_url),
    Datas = [
	     {"romance", [252,246,228,220,214,196,32,231,232,233,32,191,241,32,232,224,249,242]},
	     {"korean", [50676,50640]},
	     {"chinese", [20013,25991]},
	     {"japanese", [12414,12407]},
	     {"dangerous", "&%?\\+|,%*"}],
    
    
    Test = fun({Id,Data}) ->
		   UTF8 = xmerl_ucs:to_utf8(Data),
		   Result = selenium_api: is_text_present (Session, UTF8),
		   {ok, "true"} = Result,
		   {ok, UTF8} = selenium_api: get_text (Session, Id)
	   end,
    lists:foreach(Test, Datas),
    selenium: stop (Session).


%% high_level_test () ->
%%     Config = selenium_config (),
%%     Commands = commands (),
%%     Results = selenium: run (Config, Commands),
%%     URL = "/selenium-server/tests/html/test_click_page1.html",
%%     [{{open, [URL]},{not_tested, {ok, none}}},
%%      {{getText, ["link"], _}, {ok, "OK"}},
%%      {{{array, getAllLinks}, [], _}, {ok, "OK"}},
%%      {{click, ["link"]}, {not_tested, {ok, none}}},
%%      {{wait_for_page_to_load, ["5000"]}, {not_tested, {ok, none}}}] = Results.


%% commands () ->
%%     [open (),
%%      get_text (),
%%      get_all_links (),
%%      click (),
%%      wait_for_page_to_load ()].

%% open () ->
%%     {open, ["/selenium-server/tests/html/test_click_page1.html"]}.

%% get_text () ->
%%     {getText, ["link"], fun(X) -> "Click here for next page" ++ _Rest = X end}.

%% get_all_links () ->
%%     {{array, getAllLinks}, [], fun(X) -> true = 3 < length(X),
%% 				"linkToAnchorOnThisPage" = lists:nth(4,X)
%% 		      end}.
%% click () ->
%%     {click, ["link"]}.

%% wait_for_page_to_load () ->
%%     {wait_for_page_to_load, ["5000"]}.

%% selenium_config () ->
%%     URL = "http://localhost:4444",
%%     BrowserBinary = "/usr/lib/firefox/firefox-2-bin",
%%     [{server, {?HOST, ?PORT}},
%%      {browser, {"*firefox", BrowserBinary}},
%%      {url, URL}].
    
