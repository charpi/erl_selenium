%%% Copyright (c) 2007-2009 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.

%% @author Nicolas Charpentier <open_source@charpi.net> [http://charpi.net]
%% @copyright 2008 Nicolas Charpentier
-module (selenium_tests).

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
    selenium:cmd (Session, open, [Start_url]),
    {ok, "Click here for next page" ++ _Rest} = selenium: cmd (Session, getText,
                                                               ["link"]),

    {ok, StringLinks} = selenium: cmd_array (Session, getAllLinks),
    true = length (StringLinks) > 3,
    "linkToAnchorOnThisPage" = lists: nth (4, StringLinks),

    selenium: cmd (Session, click, ["link"]),
    selenium: cmd (Session, waitForPageToLoad, ["5000"]),
    %%    Head ++ "/selenium-server/tests/html/test_click_page2.html" = selenium:cmd(get_location,Session),
    selenium: cmd (Session, click, ["previousPage"]),
    selenium: cmd (Session, waitForPageToLoad, ["5000"]),
    %%    Head ++ "/selenium-server/tests/html/test_click_page1.html" = selenium:cmd(get_location,Session),
    ok.

google_test (Session) ->
    selenium: cmd (Session, open, ["http://www.google.com/webhp"]),
    selenium: cmd (Session, type, ["q", "hello world"]),
    selenium: cmd (Session, click, ["btnG"]),
    selenium: cmd (Session, waitForPageToLoad, ["5000"]),
    {ok,"hello world - Google Search"} = selenium: cmd (Session, getTitle),
    ok.

keypress_test (Session) ->
    InputId = "ac4",
    UpdateId = "ac4update",
    Ajax_url = "http://localhost:4444/selenium-server/tests/html/ajax/ajax_autocompleter2_test.html",
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
    ok.

type_very_long_text_test (Session) ->
    Start_url = "/selenium-server/tests/html/test_rich_text.html",
    LongText = lists:duplicate (50000, $z), 
    selenium: cmd (Session, open, [Start_url]),
    selenium: cmd (Session, type, ["richtext", LongText]),
    {ok, LongText} = selenium: cmd (Session, getValue, ["richtext"]),
    ok.

utf8_test (Session) ->    
    Start_url = "/selenium-server/tests/html/test_editable.html",
    selenium: cmd (Session, open, [Start_url]),
    selenium: cmd (Session, waitForPageToLoad, []),
    Object = "normal_text",
    String = [85,110,105,99,111,100,101,32479,19968,30721],
    Test = fun(Text) ->
                   selenium: cmd (Session, type, [Object, Text]),
                   {ok, Text} = selenium: cmd (Session, getValue, [Object])
           end,
    Inputs = ["foo", xmerl_ucs: to_utf8 (String)],
    lists: foreach (Test, Inputs),
    ok.

i18n_test (Session) ->
    Start_url = "/selenium-server/tests/html/test_i18n.html",
    selenium: cmd (Session, open, [Start_url]),
    Datas = [
	     {"romance", [252,246,228,220,214,196,32,231,232,233,32,191,241,32,232,224,249,242]},
	     {"korean", [50676,50640]},
	     {"chinese", [20013,25991]},
	     {"japanese", [12414,12407]},
	     {"dangerous", "&%?\\+|,%*"}],


    Test = fun({Id,Data}) ->
		   UTF8 = xmerl_ucs:to_utf8(Data),
		   Result = selenium: cmd (Session, isTextPresent, [UTF8]),
		   {is_text_present, {ok, "true"}} = {is_text_present, Result},
		   {get_text, {ok, UTF8}} = {get_text, selenium: cmd(Session, getText, [Id])}
	   end,
    lists:foreach(Test, Datas),
    ok.

command_to_string_test () ->
    Test_fun = fun ({Expected, Command}) ->
                       Result = selenium: command_to_string (Command),
                       assert_commands (Expected,Result)
               end,
    Test_datas = [simple_command(), 
                  command_with_params (),
                  command_with_strange_params ()],
    lists:map (Test_fun, Test_datas),
    ok.

simple_command () ->
    {"cmd="++command_string (),{command (), []}}.

command_with_params () ->
    {"cmd="++command_string () ++ "&1=first&2=second",{command (),params ()}}.

command_with_strange_params () ->
    {"cmd=open&1=http%3A%2F%2Fgoogle.com&2=hello+world",{open, strange_params ()}}.


build_request_without_id_test () ->
    Test_fun = fun({Expected, {Host, Port, Command}}) ->
                       Result = selenium:build_request_without_id (Host, Port,Command),
                       assert_commands (Expected,Result)
               end,
    Parameters = {host(), port(), {open, strange_params()}},
    Body = "cmd=open&1=http%3A%2F%2Fgoogle.com&2=hello+world",
    Expected_request = {url(),Body},
    Test_datas = [{Expected_request,Parameters}],
    lists:map (Test_fun, Test_datas),
    ok.

build_request_test () ->
    Test_fun = fun({Expected, {Host, Port, Command}}) ->
                       Result = selenium:build_request(Host, Port, id(), Command),
                       assert_commands(Expected,Result)
               end,
    Parameters = {host(), port(), {open, strange_params()}},
    Body = "cmd=open&1=http%3A%2F%2Fgoogle.com&2=hello+world&sessionId=666",
    Expected_request = {url(),Body},
    lists:map (Test_fun, [{Expected_request, Parameters}]),
    ok.

parse_body_test () ->
    Test = fun({Expected, Input}) ->
                   {Input,Expected} = {Input,selenium:parse_body (standard, Input)}
           end,
    lists:map (Test,
               [{{failed,"toto"},"toto"},
                {{ok,none},"OK"},
                {{ok,[]},"OK,"},
                {{ok,1} ,"OK,01"},
                {{ok,-1} ,"OK,-01"},
                {{ok,109} ,"OK,109"},
                {{ok,10} ,"OK,10"},
                {{ok,-20} ,"OK,-20"},
                {{ok,0.123} ,"OK,0.123"},
                {{ok,10.05} ,"OK,10.05"},
                {{ok,-10.02} ,"OK,-10.02"},
                {{ok,"myString"} ,"OK,myString"},
                {{ok,"comma, test"} ,"OK,comma\\\, test"}, %% need to
                %% escape $\ to ensure that it's interpreted.
                {{ok,"dog,cat"} ,"OK,dog,cat"}
               ]),

    {ok, ["dog","cat"]} = selenium:parse_body (array, "OK,dog,cat"),
    {ok, ["comma, test"," other line"]} = selenium:parse_body (array, "OK,comma\\\, test, other line"),
    ok.

start_session_test () ->
    URL = "http://localhost:4444",
    io:format("Test~n"),
    Session = selenium: start (?HOST,
                               ?PORT,
                               ?COMMAND,
                               URL),
    selenium: stop (Session),
    ok.

high_level_test () ->
    Config = selenium_config (),
    Commands = commands (),
    Results = selenium: run (Config, Commands),
    URL = "/selenium-server/tests/html/test_click_page1.html",
    [{{open, [URL]},{not_tested, {ok, none}}},
     {{getText, ["link"], _}, {ok, "OK"}},
     {{{array, getAllLinks}, [], _}, {ok, "OK"}},
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
    {{array, getAllLinks}, [], fun(X) -> true = 3 < length(X),
					 "linkToAnchorOnThisPage" = lists:nth(4,X)
			       end}.
click () ->
    {click, ["link"]}.

wait_for_page_to_load () ->
    {waitForPageToLoad, ["5000"]}.

selenium_config () ->
    URL = "http://localhost:4444",
    [{server, {?HOST, ?PORT}},
     {browser, ?COMMAND},
     {url, URL}].



id () ->
    "666".

host () ->
    "host".

port () ->
    4444.

url () ->
    "http://"++host()++":"++integer_to_list(port())++"/selenium-server/driver/?".

command () ->
    myCommand.

command_string () ->
    atom_to_list(command ()).

params () ->
    ["first","second"].

strange_params () ->
    ["http://google.com","hello world"].

assert_commands (Expected, Result) ->
    Expected = Result.
