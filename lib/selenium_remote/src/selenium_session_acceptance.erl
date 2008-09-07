%%% Copyright (c) 2008 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.
-module (selenium_session_acceptance).

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
    Session = selenium: launch_session (?HOST,
					?PORT,
					?COMMAND,
					URL),
    Start_url = "/selenium-server/tests/html/test_click_page1.html",
    Session: open (Start_url),
    
    {ok, "Click here for next page" ++ _Rest} = Session: getText ("link"),
    
    {ok, StringLinks} = Session: getAllLinks (),
    true = length (StringLinks) > 3,
    "linkToAnchorOnThisPage" = lists: nth (4, StringLinks),
    
    Session: click ( "link"),
    Session: waitForPageToLoad ( "5000"),
    %%    Head ++ "/selenium-server/tests/html/test_click_page2.html" = selenium:cmd(get_location,Session),
    Session: click ( "previousPage"),
    Session: waitForPageToLoad ( "5000"),
    %%    Head ++ "/selenium-server/tests/html/test_click_page1.html" = selenium:cmd(get_location,Session),
    Session: stop_session (),
    ok.

google_test () ->
    URL = "http://www.google.com/webhp",
    Session = selenium: launch_session (?HOST,
					?PORT,
					?COMMAND,
					URL),
    Session: open ( "http://www.google.com/webhp"),
    Session: type ( "q", "hello world"),
    Session: click ( "btnG"),
    Session: waitForPageToLoad ( "5000"),
    {ok,"hello world - Google Search"} = Session: getTitle (),
    Session: stop_session (),
    ok.

keypress_test () ->
    InputId = "ac4",
    UpdateId = "ac4update",
    URL = "http://www.irian.at",
    Session = selenium: launch_session (?HOST,
					?PORT,
					?COMMAND,
					URL),
    Ajax_url = "http://www.irian.at/selenium-server/tests/html/ajax/ajax_autocompleter2_test.html",
    Session: open ( Ajax_url),
    Session: keyPress ( InputId, "74"),
    receive after 500 -> ok end,
    Session: keyPress ( InputId, "97"),
    Session: keyPress ( InputId, "110"),
    receive after 500 -> ok end,
    {ok, "Jane Agnews"} = Session: getText ( UpdateId),
    Session: keyPress( InputId, "13"),
    receive after 500 -> ok end,
    {ok, "Jane Agnews"} = Session: getValue ( InputId),
    Session: stop_session ().

type_very_long_text_test () ->
    URL = "http://localhost:4444",
    Session = selenium: launch_session (?HOST,
					?PORT,
					?COMMAND,
					URL),
    Start_url = "/selenium-server/tests/html/test_rich_text.html",
    LongText = lists:duplicate (50000, $z), 
    Session: open ( Start_url),
    Session: type ( "richtext", LongText),
    {ok, LongText} = Session: getValue ( "richtext"),
    Session: stop_session ().

utf8_test () ->    
    URL = "http://localhost:4444",
    Session = selenium: launch_session (?HOST,
					?PORT,
					?COMMAND,
					URL),
    Start_url = "/selenium-server/tests/html/test_editable.html",
    Session: open ( Start_url),
    Session: waitForPageToLoad ("5000"),
    Object = "normal_text",
    String = [85,110,105,99,111,100,101,32479,19968,30721],
    Test = fun(Text) ->
		   Session: type (Object, Text),
		   {ok, Text} = Session: getValue (Object)
	   end,
    Inputs = ["foo", xmerl_ucs: to_utf8 (String)],
    lists: foreach (Test, Inputs),
    Session: stop_session ().

i18n_test () ->
    URL = "http://localhost:4444",
    Start_url = "/selenium-server/tests/html/test_i18n.html",
    Session = selenium: launch_session (?HOST,
					?PORT,
					?COMMAND,
					URL),
    Session: open ( Start_url),
    Datas = [
	     {"romance", [252,246,228,220,214,196,32,231,232,233,32,191,241,32,232,224,249,242]},
	     {"korean", [50676,50640]},
	     {"chinese", [20013,25991]},
	     {"japanese", [12414,12407]},
	     {"dangerous", "&%?\\+|,%*"}],
    
    
    Test = fun({Id,Data}) ->
		   UTF8 = xmerl_ucs:to_utf8(Data),
		   Result = Session: isTextPresent ( UTF8),
		   {ok, "true"} = Result,
		   {ok, UTF8} = Session: getText ( Id)
	   end,
    lists:foreach(Test, Datas),
    Session: stop_session ().


%% high_level_test () ->
%%     Config = selenium_config (),
%%     Commands = commands (),
%%     Results = selenium: run (Config, Commands),
%%     URL = "/selenium-server/tests/html/test_click_page1.html",
%%     [{{open, [URL]},{not_tested, {ok, none}}},
%%      {{getText, ["link"], _}, {ok, "OK"}},
%%      {{{array, getAllLinks}, [], _}, {ok, "OK"}},
%%      {{click, ["link"]}, {not_tested, {ok, none}}},
%%      {{waitForPageToLoad, ["5000"]}, {not_tested, {ok, none}}}] = Results.


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
%%     {waitForPageToLoad, ["5000"]}.

%% selenium_config () ->
%%     URL = "http://localhost:4444",
%%     BrowserBinary = "/usr/lib/firefox/firefox-2-bin",
%%     [{server, {?HOST, ?PORT}},
%%      {browser, {"*firefox", BrowserBinary}},
%%      {url, URL}].
    
