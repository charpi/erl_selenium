%%% Copyright (c) 2007 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.
-module (selenium).

-export ([start/4, stop/1]).
-export ([cmd/2, cmd/3]).
-export ([run/2]).

-export ([build_request_without_id/3]).
-export ([build_request/4]).
-export ([parse_body/1]).
-export ([command_to_string/1]).


start (Host, Port, Command, URL) ->
    Get_new_browser = {getNewBrowserSession, [Command, URL]},
    RequestUrl = build_request_without_id (Host, Port, Get_new_browser),
    {ok, Result} = send_request (RequestUrl),
    {Host, Port, Result}.

stop (Session) ->
    {ok, Result} = cmd (Session, testComplete, []),
    Result.

cmd (Session, Command) ->
    cmd (Session, Command, []).

cmd ({Host, Port, Id}, Command, Params) when is_list (Params) ->    
    Request = build_request (Host, Port, Id, {Command, Params}),
    Result = send_request (Request),
    Result.

run(Config, Commands) when is_list (Config) ->
    Session = launch_session (Config),
    Excecute = fun (Command) -> run_command(Session, Command) end,
    Results = lists: map(Excecute, Commands),
    selenium:stop(Session),
    Results.

launch_session (Config) ->
    Config_value = dict: from_list (Config),
    {ok, {Host, Port}} = dict: find (server, Config_value),
    {ok, {Browser, Browser_binary}} = dict: find (browser, Config_value),
    {ok, URL} = dict: find (url, Config_value),
    Browser_command = Browser ++ " " ++ Browser_binary,
    selenium: start (Host, Port, Browser_command, URL).

run_command (Session, {Key,Params,Expected}) when is_function (Expected)->
    Command = {Key, Params, Expected},
    Command_result = selenium: cmd (Session, Key, Params),
    interpret_result (Command, Command_result, Expected);
run_command (Session, Command) ->
    {Key, Params} = Command,
    Result = selenium: cmd (Session, Key, Params),
    {Command, {not_tested, Result}}.

interpret_result (_, {ok, Result}, Expected) ->
    case catch Expected (Result) of
	{'EXIT', {Reason, _Stack}} ->
	    {test_error, error_message (Reason, Result)};
	_Good ->
	    {ok,"OK"}
    end;
interpret_result (Command, X, _) ->
    {Command, {cmd_error, X}}.

error_message (Reason, Result) ->
    String = io_lib: format ("FAILED: ~w:~w", [Reason, Result]),
    lists: flatten (String).

build_request_without_id (Host, Port, Command) when is_integer(Port) ->
    Command_string = command_to_string (Command),
    Driver_url = "/selenium-server/driver/?",
    Port_value = integer_to_list (Port),
    "http://" ++ Host ++ ":" ++ Port_value ++ Driver_url ++ Command_string.

build_request (Host, Port, Id, Command) ->
    Base_url = build_request_without_id (Host,Port,Command),
    Base_url ++ "&sessionId=" ++ integer_to_list (Id).

command_to_string ({Command, Parameters}) when is_atom (Command), 
					       is_list (Parameters)->
    Build_parameter = fun (X, {Index,Acc}) ->
			      {Index+1,
			       Acc ++ "&" ++ integer_to_list (Index)
				  ++ "=" ++ encode_url_params (X)}
			 end,
    {_, ParamsString} = lists: foldl (Build_parameter, {1,""}, Parameters),
    "cmd="++ atom_to_list (Command) ++ ParamsString.

send_request (RequestUrl) ->
    {ok, {{_,200,_}, _, Body}} = http: request (RequestUrl),
    parse_body(Body).

parse_body ("OK") ->
    {ok, none};
parse_body ("OK," ++ Rest) ->
    {ok, parse_body_content (Rest)};
parse_body (X) ->
    {failed, X}.

parse_body_content ([H|_] = Body) when H >= $1, H =< $9 ->
    parse_integer (Body);
parse_body_content ([H,Second |_] = Body) when H == $- , 
Second >= $1, 
Second=< $9 ->
    parse_integer (Body);
parse_body_content ([H,Second |_] = Body) when H == $0 , Second == $. ->
    parse_float (Body);
parse_body_content (Rest) ->
    parse_string (Rest).

parse_integer (Rest) ->
    parse_integer (Rest,[]).

parse_integer ([], Acc) ->
    list_to_integer (lists: reverse (Acc));
parse_integer ([$.|Rest], Acc) ->
    parse_float (lists: reverse (Acc) ++ [$.|Rest]);
parse_integer ([H|Rest],Acc) ->
    parse_integer (Rest,[H|Acc]).

parse_float (Rest) ->
    list_to_float (Rest).

parse_string(String) ->
    parse_string(String,[]).
parse_string([], Acc) ->
    reverse_accumulator(Acc);
parse_string([$,|Rest],Acc) ->
    parse_string(Rest,[[]|transform_accumulator(Acc)]);
parse_string([$\\,$\\|Rest],Acc) ->
    parse_string(Rest,accumulate($\\,Acc));
parse_string([$\\,$,|Rest],Acc) ->
    parse_string(Rest,accumulate($,,Acc));
parse_string([$\\,A|Rest],Acc) ->
    parse_string(Rest,accumulate(A,Acc));
parse_string([H|Rest],Acc) ->
    parse_string(Rest,accumulate(H,Acc)).

accumulate(Value,[H|T]) when is_list(H) -> 
    [[Value|H]|T];
accumulate(Value, Acc) ->
    [Value|Acc].

reverse_accumulator([H|_]=Acc) when is_list(H) ->
    lists:reverse(lists:map(fun(X) ->
		      lists:reverse(X)
	      end,
	      Acc));
reverse_accumulator(Acc) ->
    lists:reverse(Acc).

transform_accumulator([H|T]) when is_list(H) ->
    [H|T];
transform_accumulator(Acc) ->
    [Acc].

encode_url_params(Str) when list(Str) ->
    encode_url_params(lists:reverse(Str), []).

encode_url_params([32 | T], Acc) ->
    encode_url_params(T, [$+ | Acc]);
encode_url_params([X | T], Acc) when X >= $0, X =< $9 ->
    encode_url_params(T, [X | Acc]);
encode_url_params([X | T], Acc) when X >= $a, X =< $z ->
    encode_url_params(T, [X | Acc]);
encode_url_params([X | T], Acc) when X >= $A, X =< $Z ->
    encode_url_params(T, [X | Acc]);
encode_url_params([X | T], Acc) when X == $-; X == $_; X == $. ->
    encode_url_params(T, [X | Acc]);
encode_url_params([X | T], Acc) ->
    encode_url_params(T, [$%, 
			  dec_to_hex(X bsr 4), 
			  dec_to_hex(X band 16#0f) | Acc]);
encode_url_params([], Acc) ->
    Acc.

dec_to_hex(N) when N<10 -> N+$0;
dec_to_hex(N) -> N+$A-10.
