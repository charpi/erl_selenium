%%% Copyright (c) 2007,2008 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.

%% @author Nicolas Charpentier <open_source@charpi.net> [http://charpi.net]
%% @copyright 2007,2008 Nicolas Charpentier

-module (selenium).

-export ([launch_session/4]).

-export ([start/4, stop/1]).
-export ([cmd/2, cmd/3]).
-export ([cmd_array/2, cmd_array/3]).
-export ([run/2]).

-export ([build_request_without_id/3]).
-export ([build_request/4]).
-export ([parse_body/2]).
-export ([command_to_string/1]).

%% @type selenium_session(). An abstract session
%% @type selenium_command_result(). {ok, none} | {ok, term()} | {failed, term()}.
%% @type module_instance(). Instance of a parametrized module

%% @doc Starts a selenium session which can be used with cmd/3 and cmd_array/3
%% functions.
%% @spec start(string(), integer(), string(), string()) -> selenium_session()
start (Host, Port, Command, URL) ->
    application: start (inets),
    Get_new_browser = {getNewBrowserSession, [Command, URL]},
    RequestUrl = build_request_without_id (Host, Port, Get_new_browser),
    Result = send_request (RequestUrl),
    {ok, Body} = parse_body(standard, Result),
    {Host, Port, normalize_session_id (Body)}.

%% @doc Returns a module selenium_session which can be used directly.
%% @spec launch_session(string(),integer(),string(),string()) ->
%% module_instance()
launch_session (Host, Port, Command, URL) ->
    S = start (Host, Port, Command, URL),
    try 
	selenium_session: new (S)
    catch E:R ->
	    stop (S),
	    exit ({E,R,erlang:get_stacktrace ()})
    end.

%% @doc Tells to the selenium server to close the session.
%% @spec stop(selenium_session()) -> selenium_command_result()
stop (Session) ->
    {ok, Result} = cmd (Session, testComplete, []),
    Result.

%% @doc Sends a command to the selenium server through a session. The result of
%% the command is not interpreted as an array
%% @spec cmd(selenium_session(), string()) -> selenium_command_result()
cmd (Session, Command) ->
    cmd (Session, Command, []).

%% @doc Sends a command to the selenium server through a session. The result of
%% the command is not interpreted as an array
%% @spec cmd(selenium_session(), string(), [string()]) -> selenium_command_result()
cmd (Session, Command, Params) ->
    cmd (Session, Command, Params, fun result_as_standard/1).

%% @doc Sends a command to the selenium server through a session. The result of
%% the command is interpreted as an array
%% @spec cmd_array(selenium_session(), string()) -> selenium_command_result()
cmd_array (Session, Command) ->
    cmd_array (Session, Command, []).

%% @doc Sends a command to the selenium server through a session. The result of
%% the command is interpreted as an array
%% @spec cmd_array(selenium_session(), string(), [string()]) -> selenium_command_result()
cmd_array (Session, Command, Params) ->
    cmd (Session, Command, Params, fun result_as_array/1).

run (Config, Commands) when is_list (Config) ->
    Session = launch_command_session (Config),
    Excecute = fun (Command) -> run_command (Session, Command) end,
    Results = lists: map (Excecute, Commands),
    selenium: stop (Session),
    Results.

cmd (Session, Command, Params, Fun) when is_list (Params) ->
    {Host, Port, Id} = Session,
    Request = build_request (Host, Port, Id, {Command, Params}),
    Result = send_request (Request),
    Fun(Result).

launch_command_session (Config) ->
    Config_value = dict: from_list (Config),
    {ok, {Host, Port}} = dict: find (server, Config_value),
    {ok, {Browser, Browser_binary}} = dict: find (browser, Config_value),
    {ok, URL} = dict: find (url, Config_value),
    Browser_command = Browser ++ " " ++ Browser_binary,
    selenium: start (Host, Port, Browser_command, URL).

run_command (Session, {Key, Params}) ->
    Command = {Key, Params},
    Function = function_to_call(Session, Key, Params),
    Command_result = Function (),
    interpret_result (Command, Command_result, not_tested);
run_command (Session, {Key,Params,Expected}) when is_function (Expected) ->
    Command = {Key, Params, Expected},
    Function = function_to_call(Session, Key, Params),
    Command_result = Function (),
    interpret_result (Command, Command_result, Expected).

function_to_call (Session, Key, Params) when is_atom (Key) ->
    fun () -> selenium: cmd (Session, Key, Params) end;
function_to_call (Session, {array, Key}, Params) ->
    fun () -> selenium: cmd_array (Session, Key, Params) end.

interpret_result (Command, {ok, Result}, not_tested) ->
    {Command, {not_tested, {ok, Result}}};
interpret_result (Command, {ok, Result}, Expected) ->
    case catch Expected (Result) of
        {'EXIT', {Reason, _Stack}} ->
            {test_error, error_message (Reason, Result)};
        _Good ->
            {Command, {ok,"OK"}}
    end;
interpret_result (Command, X, _) ->
    {Command, {cmd_error, X}}.

error_message (Reason, Result) ->
    String = io_lib: format ("FAILED: ~w:~w", [Reason, Result]),
    lists: flatten (String).


request_url (Host, Port) ->
    server_url (Host, Port) ++ "/?".

%% @private
build_request_without_id (Host, Port, Command)  ->
    {request_url (Host, Port),command_to_string (Command)}.

%% @private
build_request (Host, Port, Id, Command) ->
    {request_url (Host, Port), command_to_string (Command, Id)}.


%% @private
command_to_string ({Command, Parameters}) when is_atom (Command), 
                                               is_list (Parameters)->
    Build_parameter = fun (X, {Index,Acc}) ->
                              {Index+1,
                               Acc ++ "&" ++ integer_to_list (Index)
                               ++ "=" ++ encode_url_params(X)}
                      end,
    {_, ParamsString} = lists: foldl (Build_parameter, {1,""}, Parameters),
    "cmd="++ atom_to_list (Command) ++ ParamsString.

command_to_string (Command, Id) ->
    command_to_string(Command) ++ "&sessionId=" ++ Id.

server_url (Host, Port) when is_integer(Port) ->
    Driver_url = "/selenium-server/driver",
    Port_value = integer_to_list (Port),
    "http://" ++ Host ++ ":" ++ Port_value ++ Driver_url.

send_request ({Url, Body}) ->
    Content_type = "application/x-www-form-urlencoded; charset=utf-8",
    Request = {Url, [], Content_type, Body},
    Result = http: request (post, Request, [], []),
    {ok, {{_,200,_}, _, Response}} = Result,
    Response.

%% @private
parse_body (_, "OK") ->
    {ok, none};
parse_body (Type, "OK," ++ Rest) ->
    {ok, parse_body_value (Type, Rest)};
parse_body (_, X) ->
    {failed, X}.

parse_body_value (Type, [$-|T]) ->
    parse_number (Type, T, [$-],number);
parse_body_value (Type, [H|T]) when H >= $0 , H =<$9 ->
    parse_number (Type, T, [H],number);
parse_body_value (Type, H)->
    parse_string (Type, H).

parse_number (_, [],Acc,Type) ->
    case Type of 
	float ->
	    list_to_float (lists: reverse (Acc));
	_ ->
	    list_to_integer (lists: reverse (Acc))
    end;
parse_number (Type, [$.|T], Acc, number) ->
    parse_number (Type, T, [$.|Acc],  float);
parse_number (Type,  [H|T], Acc, number) when H >= $0 , H =<$9 ->
    parse_number (Type, T, [H|Acc],  number);
parse_number (Type,  [H|T], Acc, float) when H >= $0 , H =<$9 ->
    parse_number (Type, T, [H|Acc],  float);
parse_number (Type, Head, Acc, _) ->
    parse_string (Type, lists: reverse (Acc) ++ Head).

parse_string (Type, String) ->
    parse_string (Type,String,[]).

parse_string (Type, [], Acc) ->
    reverse_accumulator (Type, Acc);
parse_string (Type, [$,|Rest],Acc) ->
    parse_string (Type, Rest,[[]|transform_accumulator(Acc)]);
parse_string (Type, [$\\,$\\|Rest],Acc) ->
    parse_string (Type, Rest,accumulate($\\,Acc));
parse_string (Type, [$\\,$,|Rest],Acc) ->
    parse_string (Type, Rest,accumulate($,,Acc));
parse_string (Type, [H|Rest],Acc) ->
    parse_string (Type, Rest,accumulate(H,Acc)).

accumulate(Value,[H|T]) when is_list(H) -> 
    [[Value|H]|T];
accumulate(Value, Acc) ->
    [Value|Acc].

reverse_accumulator(array, [H|_]=Acc) when is_list(H) ->
    lists:reverse(lists:map(fun(X) ->
                                    lists:reverse(X)
                            end,
                            Acc));
reverse_accumulator(standard, [H|_]=Acc) when is_list(H) ->
    Values = lists:reverse(lists:map(fun(X) ->
                                    lists:reverse(X)
                            end,
                            Acc)),
    string_join (Values,",");
reverse_accumulator(_Type, Acc) ->
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

string_join (List, Sep) ->
    string_join (List, Sep, []).

string_join ([H], _, Acc) ->
    lists: flatten (lists: reverse ([H|Acc]));
string_join ([H|Tail], Sep, Acc) ->
    string_join (Tail, Sep, [Sep, H|Acc]).

normalize_session_id (SessionId) when is_list(SessionId) ->
    SessionId;
normalize_session_id (SessionId) ->
    integer_to_list(SessionId).

result_as_array (Result) ->
    parse_body (array, Result).

result_as_standard (Result) ->
    parse_body (standard, Result).

