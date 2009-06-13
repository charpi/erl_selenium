%%% Copyright (c) 2009 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.

%% @author Nicolas Charpentier <open_source@charpi.net> [http://charpi.net]
%% @copyright 2009 Nicolas Charpentier
-module(test_generator).

-export([start_stop /1]).
-export([launch_close /1]).

-define (URL, "http://localhost:4444").

start_stop(Module) ->
    [{setup, fun start_session/0, fun stop_session/1, 
      fast_tests(Module)}].

start_session () ->
    selenium: start (?HOST, ?PORT, ?COMMAND, ?URL).

stop_session (Session) ->
    selenium: stop (Session).

launch_close(Module) ->
    [{setup, fun launch_session/0, fun close_session/1, 
      fast_tests(Module)}].

launch_session () ->
    selenium: launch_session (?HOST, ?PORT, ?COMMAND, ?URL).

close_session (Session) ->
    Session: stop_session ().

fast_tests (Module) ->
    Tests = [default_server_test,
	     i18n_test,
	     utf8_test,
	     type_very_long_text_test,
	     keypress_test,
	     google_test],
    fun (X) ->
	    [{timeout, 60, fun() -> Module:T(X) end} || T <- Tests]
    end.
