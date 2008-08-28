%%% Copyright (c) 2007-2008 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.
-module (selenium_acceptance).

-compile ([export_all]).

-define (HOST,"192.168.0.5").
-define (PORT,4444).
-define (COMMAND,"*firefox\ /usr/lib/firefox/firefox-2-bin").

tests () ->
    start_session_test(),
    default_server_test(),
    google_test(),
    keypress_test(),
    high_level_test(),
    ok.

start_session_test () ->
    URL = "http://localhost:4444",
    Session = selenium: start (?HOST,
			       ?PORT,
			       ?COMMAND,
			       URL),
    %% After the start all commands sent throught the session must contain
    %% the same sessionid. This sessionid was returned by the server as a 
    %% reply to a command.
    selenium: stop (Session),
    ok.

default_server_test () ->
    URL = "http://localhost:4444",
    Session = selenium: start (?HOST,
			       ?PORT,
			       ?COMMAND,
			       URL),
    Start_url = "/selenium-server/tests/html/test_click_page1.html",
    selenium:cmd (Session, open, [Start_url]),
    {ok, "Click here for next page" ++ _Rest} = selenium: cmd (Session, getText,
    ["link"]),
    
    {ok, StringLinks} = selenium: cmd (Session, getAllLinks),
    true = length (StringLinks) > 3,
    "linkToAnchorOnThisPage" = lists: nth (4, StringLinks),
    
    selenium: cmd (Session, click, ["link"]),
    selenium: cmd (Session, waitForPageToLoad, ["5000"]),
    %%    Head ++ "/selenium-server/tests/html/test_click_page2.html" = selenium:cmd(get_location,Session),
    selenium: cmd (Session, click, ["previousPage"]),
    selenium: cmd (Session, waitForPageToLoad, ["5000"]),
%%    Head ++ "/selenium-server/tests/html/test_click_page1.html" = selenium:cmd(get_location,Session),
    selenium: stop (Session),
    ok.

google_test () ->
    URL = "http://www.google.com/webhp",
    Session = selenium: start (?HOST,
			       ?PORT,
			       ?COMMAND,
			       URL),
    selenium: cmd (Session, open, ["http://www.google.com/webhp"]),
    selenium: cmd (Session, type, ["q", "hello world"]),
    selenium: cmd (Session, click, ["btnG"]),
    selenium: cmd (Session, waitForPageToLoad, ["5000"]),
    {ok,"hello world - Google Search"} = selenium: cmd (Session, getTitle),
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
    selenium: cmd (Session, open, [Ajax_url]),
    selenium: cmd (Session, keyPress, [InputId, "74"]),
    receive after 500 -> ok end,
    selenium: cmd (Session, keyPress, [InputId, "97"]),
    selenium: cmd (Session, keyPress, [InputId, "110"]),
    receive after 500 -> ok end,
    {ok, "Jane Agnews"} = selenium: cmd (Session, getText, [UpdateId]),
    selenium: cmd (Session, keyPress, [InputId, "13"]),
    receive after 500 -> ok end,
    {ok, "Jane Agnews"} = selenium: cmd (Session, getValue, [InputId]),
    selenium: stop (Session).

high_level_test () ->
    Config = selenium_config (),
    Commands = commands (),
    Results = selenium: run (Config, Commands),
    [{{open, ["/selenium-server/tests/html/test_click_page1.html"]},
      {not_tested, {ok, none}}},
     {{getText, ["link"], _}, {ok, "OK"}},
     {{getAllLinks, [], _}, {ok, "OK"}},
     {{click, ["link"]}, {not_tested, {ok, none}}},
     {{waitForPageToLoad, ["5000"]}, {not_tested, {ok, none}}}] = Results.


commands () ->
    [open (),
     get_text (),
     get_all_links (),
     click (),
     wait_for_page_to_load ()].

open () ->
    {open, ["/selenium-server/tests/html/test_click_page1.html"]}.

get_text () ->
    {getText, ["link"], fun(X) -> "Click here for next page" ++ _Rest = X end}.

get_all_links () ->
    {getAllLinks, [], fun(X) -> true = 3 < length(X),
				"linkToAnchorOnThisPage" = lists:nth(4,X)
		      end}.
click () ->
    {click, ["link"]}.

wait_for_page_to_load () ->
    {waitForPageToLoad, ["5000"]}.

selenium_config () ->
    URL = "http://localhost:4444",
    BrowserBinary = "/usr/lib/firefox/firefox-2-bin",
    [{server, {?HOST, ?PORT}},
     {browser, {"*firefox", BrowserBinary}},
     {url, URL}].
    
