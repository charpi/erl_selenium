%%% Copyright (c) 2009 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.

%% @author Nicolas Charpentier <open_source@charpi.net> [http://charpi.net]
%% @copyright 2009 Nicolas Charpentier
-module(test_generator).

-export ([api_tests/0]).
-export ([start_session/0]).
-export ([launch_session/0]).
-export ([selenium_tests/0]).
-export ([session_tests/0]).

start_session () ->
    URL = "http://localhost:4444",
    selenium: start (?HOST, ?PORT, ?COMMAND, URL).

launch_session () ->
    URL = "http://localhost:4444",
    selenium: launch_session (?HOST, ?PORT, ?COMMAND, URL).

close_session (Session) ->
    Session: stop_session ().

stop_session (Session) ->
    selenium: stop (Session).

selenium_tests () ->
    [{setup, fun start_session/0, 
      fun stop_session/1,
      generate_tests_fun(selenium_tests)}].

api_tests () ->
    [{setup, fun start_session/0, 
      fun stop_session/1,
      generate_tests_fun(selenium_api_tests)}].

session_tests () ->
    [{setup, fun launch_session/0, 
      fun close_session/1,
      generate_tests_fun(selenium_session_tests)}].

generate_tests_fun(Module) ->
    fun (X) ->
	    Tests = Module: fast_tests (),
	    [generate_test (T,X) || T <- Tests]
    end.

generate_test (T, X) ->
    {timeout, 60, fun() -> T(X) end}.
