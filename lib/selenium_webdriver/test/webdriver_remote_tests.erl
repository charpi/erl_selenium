-module(webdriver_remote_tests).

-include_lib("eunit/include/eunit.hrl").

-export([invalid_uri_navigate/1]).
-export([navigate/1]).
-export([async_script/1]).
-export([implicit_wait/1]).
-export([get_window_handle/1]).
-export([get_window_handles/1]).
-export([back_forward/1]).
-export([execute/1]).

session_test_() ->
     Tests = [fun correct_session/0,
 	     fun invalid_session/0,
 	     fun session_capabilities/0,
 	     fun firefox_session/0,
 	     fun htmlunit_session/0,
%% 	     fun chrome_session/0
 	    ],
     [{timeout, 60, [T]} || T <- Tests].

api_test_() ->
     Default_browser = firefox,
       [{setup,
 	fun() -> setup_session(Default_browser) end,
 	fun close_session/1,
 	api_tests(Default_browser)}].


complete_api_test_() ->
      Browsers = [firefox, htmlunit, iphone],
      [{setup,
        fun() -> setup_session(B) end, 
        fun close_session/1,
        api_tests(B)} || B <- Browsers].
		     
api_tests(Browser) ->
    Tests = [
 	     invalid_uri_navigate,
 	     navigate,
 	     async_script,
 	     implicit_wait,
 	     get_window_handle,
 	     get_window_handles,
 	     back_forward,
	     execute
	    ],
    fun(X) ->
	    [{timeout, 120, 
	      {lists:flatten(io_lib:format("~s with ~s",[T,Browser])), fun() -> ?MODULE:T(X) end } 
	     } || T <- Tests]
    end.

setup_session(Browser) ->
    {ok, Session} = webdriver_remote:session(?HOST,?PORT,[{browserName, Browser}, {javascriptEnabled, true}, 
							  {version, <<"">>}, {platform, 'ANY'}]),
    Session.

close_session(Session) ->    
    {ok, no_content} = webdriver_remote:quit(Session).

invalid_uri_navigate(Session) ->
    {error, _} = webdriver_remote:get(Session, "www.google.com").

navigate(Session) ->
    URL = "http://charpi.net/blog/",
    URL_b = list_to_binary(URL),
    {ok, no_content} = webdriver_remote:get(Session, URL),
    {ok, URL_b} = webdriver_remote:get_current_url(Session),
    {ok, no_content} = webdriver_remote:get(Session, URL_b),
    {ok, URL_b} = webdriver_remote:get_current_url(Session),
    ok.

async_script(Session) ->
    {ok, no_content} = webdriver_remote:timeout(Session, async_script, 1000),
    ok.

implicit_wait(Session) ->
    {ok, no_content} = webdriver_remote:timeout(Session, implicit_wait, 1000),
    ok.

get_window_handle(Session) ->
    {ok, Binary} = webdriver_remote:get_window_handle(Session),
    true = is_binary(Binary),
    ok.

get_window_handles(Session) ->
    {ok, List} = webdriver_remote:get_window_handles(Session),
    true = is_list(List),
    ok.

back_forward(Session) ->
    {ok, no_content} = webdriver_remote:get(Session, "http://charpi.net/blog"),
    {ok, URL_1} = webdriver_remote:get_current_url(Session),
    {ok, no_content} = webdriver_remote:get(Session, "http://www.google.com"),
    {ok, URL_2} = webdriver_remote:get_current_url(Session),
    {ok, no_content} = webdriver_remote:back(Session),
    ?assertEqual({ok, URL_1}, webdriver_remote:get_current_url(Session)),
    {ok, no_content} = webdriver_remote:forward(Session),
    ?assertEqual({ok, URL_2}, webdriver_remote:get_current_url(Session)),
    ok.
    
execute(Session) ->
    {ok, no_content} = webdriver_remote:get(Session, test_page(Session, "xhtmlTest")),
    timer:sleep(10000),
    Expected = {ok, <<"XHTML Test Page">>},
    Result = webdriver_remote:execute(Session, "return document.title;"),
    assert_command("execute", Expected, Result),
    ok.

correct_session() ->
    {ok, Session} = webdriver_remote:session(?HOST,?PORT,[{browserName, firefox}]),
    {?HOST, ?PORT, Id} = Session,
    true = is_list(Id),
    true = is_integer(list_to_integer(Id)),
    {ok, no_content} = webdriver_remote:quit(Session),
    ok.

invalid_session() ->
    {error, _} = webdriver_remote:session(?HOST,?PORT,[{browserName, dummy}]),
    ok.

firefox_session() ->
    {ok, Session} = webdriver_remote:session(?HOST,?PORT,[{browserName, firefox}]),
    {ok, no_content} = webdriver_remote:quit(Session),
    ok.

htmlunit_session() ->
    {ok, Session} = webdriver_remote:session(?HOST,?PORT,[{browserName, htmlunit}, {version, <<"">>}, {platform, 'ANY'}]),
    {ok, no_content} = webdriver_remote:quit(Session),
    ok.
    
chrome_session() ->
    {ok, Session} = webdriver_remote:session(?HOST,?PORT,[{browserName, chrome}, {version, <<"">>}, {platform, 'ANY'}]),
    {ok, no_content} = webdriver_remote:quit(Session),
    ok.

session_capabilities() ->
    {ok, Session} = webdriver_remote:session(?HOST,?PORT,[{browserName, htmlunit}, {version, <<"">>}, {platform, 'ANY'}]),
    %% Warning this is wrong 
    {error, _} = webdriver_remote:session(Session),
    ok.

test_page({_Host,Port,_}, Page) ->
    "http://" ++ "localhost" ++ ":" ++ integer_to_list(Port) ++ "/selenium-server/tests/html/" ++ Page ++ ".html".

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
