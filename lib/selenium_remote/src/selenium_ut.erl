%%% Copyright (c) 2007 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.
-module(selenium_ut).

-compile([export_all]).

commands_test() ->
    lists:map(fun({Expected, Command}) ->
			Result = selenium:command_to_string(Command),
			assert_commands(Expected,Result)
		end,
		[{"cmd="++command_string(),{command(), []}},
		 {"cmd="++command_string()++"&1=first&2=second",{command(), params()}},
		 {"cmd=open&1=http%3A%2F%2Fgoogle.com&2=hello+world",{open, strange_params()}}		 
		]).

build_request_without_id_test() ->
    lists:map(fun({Expected, Host, Port, Command}) ->
		      Result = selenium:build_request_without_id(Host, Port,Command),
		      assert_commands(Expected,Result)
	      end,
	      [{url()++"cmd=open&1=http%3A%2F%2Fgoogle.com&2=hello+world",
	       host(), port(), {open, strange_params()}}]).

build_request_test() ->
    lists:map(fun({Expected, Host, Port, Command}) ->
		      Result = selenium:build_request(Host, Port, id(), Command),
		      assert_commands(Expected,Result)
	      end,
	      [{url()++"cmd=open&1=http%3A%2F%2Fgoogle.com&2=hello+world&sessionId=666",
	       host(), port(), {open, strange_params()}}]).

parse_body_test() ->
    {failed,"toto"} = selenium:parse_body("toto"),
    {ok,none} = selenium:parse_body("OK"),
    {ok,[]} = selenium:parse_body("OK,"),
    
    {ok,"01"} = selenium:parse_body("OK,01"),
    {ok,"-01"} = selenium:parse_body("OK,-01"),
    {ok,109} = selenium:parse_body("OK,109"),
    {ok,10} = selenium:parse_body("OK,10"),
    {ok,-20} = selenium:parse_body("OK,-20"),

    {ok,0.123} = selenium:parse_body("OK,0.123"),
    {ok,10.05} = selenium:parse_body("OK,10.05"),
    {ok,-10.02} = selenium:parse_body("OK,-10.02"),


    {ok,"myString"} = selenium:parse_body("OK,myString"),
    {ok,"comma, test"} = selenium:parse_body("OK,comma\\\, test"), %% need to
    %% escape $\ to ensure that it's interpreted.
    {ok,["dog","cat"]} = selenium:parse_body("OK,dog,cat").

id() ->
    666.

host() ->
    "host".

port() ->
    4444.

url() ->
    "http://"++host()++":"++integer_to_list(port())++"/selenium-server/driver/?".

command() ->
    myCommand.

command_string() ->
    atom_to_list(command()).

params() ->
    ["first","second"].
    
strange_params() ->
    ["http://google.com","hello world"].

assert_commands(Expected, Result) ->
    Expected = Result.
