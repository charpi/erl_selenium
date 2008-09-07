%%% Copyright (c) 2007,2008 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.
-module(selenium_ut).

-compile([export_all]).

tests () ->
    command_to_string_test (),
    build_request_without_id_test (),
    build_request_test (),
    parse_body_test (),
    ok.

command_to_string_test () ->
    Test_fun = fun ({Expected, Command}) ->
		       Result = selenium: command_to_string (Command),
		       assert_commands (Expected,Result)
	       end,
    Test_datas = [simple_command(), 
		  command_with_params (),
		  command_with_strange_params ()],
    lists:map (Test_fun, Test_datas).

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
    lists:map (Test_fun, Test_datas).


build_request_test () ->
    Test_fun = fun({Expected, {Host, Port, Command}}) ->
		      Result = selenium:build_request(Host, Port, id(), Command),
		      assert_commands(Expected,Result)
	      end,
    Parameters = {host(), port(), {open, strange_params()}},
    Body = "cmd=open&1=http%3A%2F%2Fgoogle.com&2=hello+world&sessionId=666",
    Expected_request = {url(),Body},
    lists:map (Test_fun, [{Expected_request, Parameters}]).

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
