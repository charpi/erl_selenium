-module(webdriver_remote_tests).

-include_lib("eunit/include/eunit.hrl").

-export([session_capabilities/2]).
-export([invalid_uri_navigate/2]).
-export([navigate/2]).
-export([async_script/2]).
-export([implicit_wait/2]).
-export([get_window_handle/2]).
-export([get_window_handles/2]).
-export([back_forward/2]).
-export([execute/2]).
-export([screenshot/2]).
-export([engines/2]).
-export([active_engine/2]).
-export([frame/2]).
-export([frame_by_index/2]).
-export([invalid_frame/2]).
-export([default_frame/2]).
-export([window/2]).
-export([delete_window/2]).
-export([find_element_by_xpath/2]).
-export([element_not_found_by_xpath/2]).
-export([elements_by_xpath/2]).
-export([get_attribute/2]).
-export([get_implicit_attribute/2]).
-export([speed/2]).
-export([cookies/2]).
-export([add_cookie/2]).

session_test_() ->
     Tests = [fun correct_session/0,
 	     fun invalid_session/0,
 	     fun firefox_session/0,
 	     fun htmlunit_session/0
%% 	     fun chrome_session/0
 	    ],
     [{timeout, 60, [T]} || T <- Tests].

%%  api_test_() ->
%%      Default_browser = firefox,
%%      [{setup,
%%        fun() -> setup_session(Default_browser) end,
%%        fun close_session/1,
%%        api_tests(Default_browser)}].


complete_api_test_() ->
    Browsers = ?WEBDRIVER_BROWSERS,
    [{setup,
      fun() -> setup_session(B) end, 
      fun close_session/1,
      api_tests(B)} || B <- Browsers].

isolated_api_test_() ->
    Browsers = ?WEBDRIVER_BROWSERS,
    [{foreach,
      fun() -> setup_session(B) end, 
      fun close_session/1,
      [isolated_tests(B)] } || B <- Browsers].

isolated_tests(Browser) ->
    Tests = [window,
	     delete_window],
    fun(X) ->
	    [{timeout, 120, 
	      {lists:flatten(io_lib:format("~s with ~s",[T,Browser])), fun() -> ?MODULE:T(Browser,X) end } 
	     } || T <- Tests]
    end.
    
		     
api_tests(Browser) ->
    Tests = [
	     session_capabilities,
 	     invalid_uri_navigate,
 	     navigate,
 	     async_script,
 	     implicit_wait,
 	     get_window_handle,
 	     get_window_handles,
 	     back_forward,
	     execute,
	     screenshot,
	     engines,
	     active_engine,
	     frame,
	     frame_by_index,
	     invalid_frame,
	     default_frame,
	     find_element_by_xpath,
	     element_not_found_by_xpath,
	     elements_by_xpath,
	     get_attribute,
	     get_implicit_attribute,
	     speed,
	     add_cookie,
	     cookies
	    ],
    fun(X) ->
	    [{timeout, 120, 
	      {lists:flatten(io_lib:format("~s with ~s",[T,Browser])), fun() -> ?MODULE:T(Browser,X) end } 
	     } || T <- Tests]
    end.

setup_session(Browser) ->
    {ok, Session} = webdriver_remote:session(?HOST,?PORT,[{browserName, Browser}, {javascriptEnabled, true}, 
							  {version, <<"">>}, {platform, 'ANY'}]),
    Session.

close_session(Session) ->    
    {ok, no_content} = webdriver_remote:quit(Session).

session_capabilities(_, Session) ->
    %% Warning this is wrong 
    {error, _} = webdriver_remote:session(Session),
    ok.

invalid_uri_navigate(_, Session) ->
    {error, _} = webdriver_remote:get(Session, "www.google.com").

navigate(_, Session) ->
    URL = "http://charpi.net/blog/",
    URL_b = list_to_binary(URL),
    {ok, no_content} = webdriver_remote:get(Session, URL),
    {ok, URL_b} = webdriver_remote:get_current_url(Session),
    {ok, no_content} = webdriver_remote:get(Session, URL_b),
    {ok, URL_b} = webdriver_remote:get_current_url(Session),
    ok.

async_script(_, Session) ->
    {ok, no_content} = webdriver_remote:timeout(Session, async_script, 1000),
    ok.

implicit_wait(_, Session) ->
    {ok, no_content} = webdriver_remote:timeout(Session, implicit_wait, 1000),
    ok.

get_window_handle(_, Session) ->
    {ok, Binary} = webdriver_remote:get_window_handle(Session),
    true = is_binary(Binary),
    ok.

get_window_handles(_, Session) ->
    {ok, List} = webdriver_remote:get_window_handles(Session),
    true = is_list(List),
    ok.

back_forward(_, Session) ->
    {ok, no_content} = webdriver_remote:get(Session, "http://charpi.net/blog"),
    {ok, URL_1} = webdriver_remote:get_current_url(Session),
    {ok, no_content} = webdriver_remote:get(Session, "http://www.google.com"),
    {ok, URL_2} = webdriver_remote:get_current_url(Session),
    {ok, no_content} = webdriver_remote:back(Session),
    ?assertEqual({ok, URL_1}, webdriver_remote:get_current_url(Session)),
    {ok, no_content} = webdriver_remote:forward(Session),
    ?assertEqual({ok, URL_2}, webdriver_remote:get_current_url(Session)),
    ok.
    
execute(Browser, Session) ->
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "xhtmlTest")),
    sleep(Browser),
    Expected = {ok, <<"XHTML Test Page">>},
    Result = webdriver_remote:execute(Session, "return document.title;"),
    assert_command("execute", Expected, Result),
    ok.

screenshot(firefox, Session) ->
    {ok, Bin} = webdriver_remote:screenshot(Session),
    true = is_binary(Bin);
screenshot(_, Session) ->
    {error, {13,_}} = webdriver_remote:screenshot(Session),
    ok.

engines(firefox, Session) ->
    {error,{13,_}} = webdriver_remote:available_engines(Session);
engines(htmlunit, Session) ->
    {error, {13,_}} = webdriver_remote:available_engines(Session);
engines(_, Session) ->
    {ok, List} = webdriver_remote:available_engines(Session),
    true = is_list(List).

active_engine(firefox, Session) ->
    {error,{13,_}} = webdriver_remote:available_engines(Session);
active_engine(htmlunit, Session) ->
    {error, {13,_}} = webdriver_remote:available_engines(Session).


frame(Browser, Session) ->
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "frameset")),
    sleep(Browser),
    {ok, no_content} = webdriver_remote:frame(Session, "third"),
    ok.

frame_by_index(Browser, Session) ->
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "frameset")),
    sleep(Browser),
    {ok, no_content} = webdriver_remote:frame(Session, 3),
    ok.

invalid_frame(Browser, Session) ->
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "frameset")),
    sleep(Browser),
    {error, {8,_}} = webdriver_remote:frame(Session, "invalid_frame_name"),
    ok.

default_frame(Browser, Session) ->
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "frameset")),
    sleep(Browser),
    {ok, no_content} = webdriver_remote:frame(Session, null),
    ok.

window(Browser, Session) ->
    Title1 = <<"XHTML Test Page">>,
    Title2 = <<"We Arrive Here">>,
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "xhtmlTest")),
    sleep(Browser),
    {ok, [Element]} = webdriver_remote:find_elements(Session, <<"link text">>, "Open new window"),
    {ok, no_content} = webdriver_remote:click(Session, Element),
    {ok, Title1} = webdriver_remote:title(Session),
    {ok, no_content} = webdriver_remote:switch_to_window(Session, "result"),
    {ok, Title2} = webdriver_remote:title(Session),
    ok.

delete_window(Browser, Session) ->
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "xhtmlTest")),
    sleep(Browser),
    {ok, [Element]} = webdriver_remote:find_elements(Session, <<"link text">>, "Open new window"),
    {ok, no_content} = webdriver_remote:click(Session, Element),
    {ok, no_content} = webdriver_remote:switch_to_window(Session, "result"),
    {ok, Title1} = webdriver_remote:title(Session),
    {ok, no_content} = webdriver_remote:switch_to_window(Session, "result"),
    {ok, _} = webdriver_remote:title(Session),
    {ok, no_content} = webdriver_remote:delete_window(Session),
    {ok, Title1} = webdriver_remote:title(Session),
    ok.

find_element_by_xpath(_, Session) ->
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "simpleTest")),
    timer:sleep(10000),
    {ok, [Id]} = webdriver_remote:find_elements(Session, xpath, "//h1"),
    {ok, <<"Heading">>} = webdriver_remote:text(Session, Id),
    ok.

element_not_found_by_xpath(_, Session) ->
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "simpleTest")),
    timer:sleep(10000),
    {error, {7,_}} = webdriver_remote:find_elements(Session, xpath, "//h4"),
    ok.

elements_by_xpath(htmlunit, Session) ->
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "nestedElements")),
    sleep(htmlunit),
    {ok, Elements} = webdriver_remote:find_elements(Session, xpath, "//option"),
    {48,Elements} = {length(Elements), Elements},
    {ok , <<>>} = webdriver_remote:get_attribute(Session, hd(Elements), "value");
elements_by_xpath(Browser, Session) ->
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "nestedElements")),
    sleep(Browser),
    {ok, Elements} = webdriver_remote:find_elements(Session, xpath, "//option"),
    {48,Elements} = {length(Elements), Elements},
    {ok , <<"One">>} = webdriver_remote:get_attribute(Session, hd(Elements), "value").


get_attribute(Browser, Session) ->
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "xhtmlTest")),
    sleep(Browser),
    {ok, [Element|_]} = webdriver_remote:find_elements(Session, id, "id1"),
    Expected = list_to_binary(test_page(Session,"xhtmlTest") ++ "#"),
    {ok, Result} = webdriver_remote:get_attribute(Session, Element, "href"),
    Expected = Result.

get_implicit_attribute(Browser, Session) ->
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "nestedElements")),
    sleep(Browser),
    {ok, [A,B,C,D|_]} = webdriver_remote:find_elements(Session, xpath, "//option"),
    Tmp = [webdriver_remote:get_attribute(Session, Element, "index") || Element <- [A,B,C,D]],
    {_,Indexes} = lists:unzip(Tmp),
    RefIndexes = lists:seq(0,3),
    [X=list_to_binary(integer_to_list(Z)) || {X,Z} <- lists:zip(Indexes, RefIndexes)].
    
speed(_Browser, Session) ->
%%     {?LINE,{ok, 'SLOW'}} = {?LINE,webdriver_remote:speed(Session)},
%%     {?LINE,{ok, no_content}} = {?LINE,webdriver_remote:speed(Session, 'SLOW')},
%%     {?LINE,{ok, 'SLOW'}} = {?LINE,webdriver_remote:speed(Session)},
%%     {?LINE,{ok, no_content}} = {?LINE,webdriver_remote:speed(Session, 'MEDIUM')},
%%     {?LINE,{ok, 'MEDIUM'}} = {?LINE,webdriver_remote:speed(Session)},
%%     {?LINE,{ok, no_content}} = {?LINE,webdriver_remote:speed(Session, 'FAST')},
%%     {?LINE,{ok, 'FAST'}} = {?LINE,webdriver_remote:speed(Session)},
    {error, []} = webdriver_remote:speed(Session),
    {error, []} = webdriver_remote:speed(Session, 'FAST'),
    ok.

cookies(Browser, Session) ->
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "nestedElements")),
    sleep(Browser),
    {ok, Before} = webdriver_remote:cookies(Session),
    Cookies = [{<<"a">>, <<"v1">>},
	       {<<"b">>, <<"v2">>}
	      ],
    Options = [{domain, <<"charpi.net">>},
	       {path, <<"/">>},
	       {secure, false}],
    [{ok, no_content} = webdriver_remote:add_cookie(Session, N, V, Options) || {N,V} <- Cookies],
    {ok, After} = webdriver_remote:cookies(Session),
    ?assertEqual(length(Before) + 2 , length(After)),
    [{ok, no_content} = webdriver_remote:delete_cookie(Session, N) || {N,_} <- Cookies],
    {ok, Before} = webdriver_remote:cookies(Session),

    {ok, no_content} = webdriver_remote:delete_cookies(Session),
    {ok, []} = webdriver_remote:cookies(Session),
    ok.

add_cookie(Browser, Session) ->
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "nestedElements")),
    Options = [{domain, <<"charpi.net">>},
	       {path, <<"/">>},
	       {secure, false}],
    {ok, no_content} = webdriver_remote:add_cookie(Session, <<"test">>, <<"value">>, Options).

correct_session() ->
    {ok, Session} = webdriver_remote:session(?HOST,?PORT,[{browserName, firefox}]),
    {?HOST, ?PORT, Id} = Session,
    true = is_list(Id),
    true = is_integer(list_to_integer(Id)),
    {ok, no_content} = webdriver_remote:quit(Session),
    ok.

invalid_session() ->
    %% Strange we can start a session with an invalid browser
    %% {error, _} = webdriver_remote:session(?HOST,?PORT,[{browserName, dummy}]),
    {ok, Session} = webdriver_remote:session(?HOST,?PORT,[{browserName, dummy}]),
    {?HOST, ?PORT, Id} = Session,
    true = is_list(Id),
    true = is_integer(list_to_integer(Id)),
    ok.

firefox_session() ->
    {ok, Session} = webdriver_remote:session(?HOST,?PORT,[{browserName, firefox}]),
    {ok, no_content} = webdriver_remote:quit(Session),
    ok.

htmlunit_session() ->
    {ok, Session} = webdriver_remote:session(?HOST,?PORT,[{browserName, htmlunit}, {version, <<"">>}, {platform, 'ANY'}]),
    {ok, no_content} = webdriver_remote:quit(Session),
    ok.
    
%% chrome_session() ->
%%     {ok, Session} = webdriver_remote:session(?HOST,?PORT,[{browserName, chrome}, {version, <<"">>}, {platform, 'ANY'}]),
%%     {ok, no_content} = webdriver_remote:quit(Session),
%%     ok.

test_page({_Host,_Port,_}, Page) ->
    "http://charpi.net/erl_selenium_test/tests/html/" ++ Page ++ ".html".

assert_command(Name, Expected, Result) ->
    case Result of
	Expected -> ok;
	{ok, _Other} -> exit({bad_result, Expected, Result});
	{error, {13, Message}} = E -> 
	    case proplists:get_value(<<"screen">>, Message) of
		undefined -> 
		    ok;
		Screen ->
		    file:write_file("/tmp/" ++ Name ++ ".jpg", base64:decode(Screen))
	    end,
	    exit(E);
	X -> X
    end.


%% sleep(htmlunit) ->
%%     ok;
sleep(_) ->
    timer:sleep(5000).
