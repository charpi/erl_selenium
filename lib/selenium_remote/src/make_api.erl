%%% Copyright (c) 2008 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.

%% @author Nicolas Charpentier <open_source@charpi.net> [http://charpi.net]
%% @copyright 2008 Nicolas Charpentier
%% @doc The main purpose of this module is to generate files from the selenium
%% api xml file.
%% It can generate either beam files or a documentation.
%% The beam file corresponds to selenium_api and to selenium_session.

-module (make_api).

-include_lib ("xmerl/include/xmerl.hrl").

-export ([api_to_binary /2]).
-export ([binary_to_file /3]).
-export ([api_to_html /1]).
-export ([html_to_file /3]).

%% @type module_type(). "api" | "session"

%% @doc Returns the binary format of the api file.
%% The parameter Type can be:
%% <ul><li>"api" to generate the binary version of selenium_api</li>
%% <li>"session" to generate the binary version of selenium_session</li></ul>.
%% The parameter API is the file path to the api xml file.
%% @spec api_to_binary(module_type(), [string()]) -> {string(), binary()}
api_to_binary (Type, API) ->
    Functions = extract_functions (API),
    {File_name, Forms} = functions_to_module (Type, Functions),
    {ok, _, Binary} = compile: forms(Forms, [report,debug_info]),
    {File_name, Binary}.

%% @doc Write a binary module on disk.
%% Output_dir is the destination directory.
%% File_name is the name of the beam file.
%% Binary is the bianry to write.
%% @spec binary_to_file(module_type(), [string()], string()) -> term()
binary_to_file (Type, API, Output_dir) ->
    {File_name, Binary} = make_api: api_to_binary (Type, API),
    Output_file = filename:join(Output_dir, File_name),
    {ok, File} = file: open(Output_file, [write,binary]),
    ok = file: write(File, Binary),
    file: close (File).

%% @doc Return the html documentation of the API. The html is returned with the
%% simplified xmerl format [{tag, attributes, children}].
%% @spec api_to_html ([string()]) -> [{atom(), [term()], [term()]}]
api_to_html (API) ->
    Functions = extract_functions (API),
        F = fun (Function) -> 
		{Name, Param, _, Comments} = function_information (Function),
		Params_html = [{b,[],[camel_case_to_erlang(Name)]},{br,[],[]},
			       "Params:",{ul, [], [{li,[],[P]} || P <- Param]}],
		    [{tr,[],[{td,[],Params_html},{td,[{valign,"top"}], Comments}]}]
	    end,
    Sort = fun (A,B) -> sort_xml_element(A,B) end,
    Functions_html = lists: map (F, lists: sort(Sort, Functions)),
    [{table,[],lists: flatten (Functions_html)}].

%% @doc Generate the html file.
%% Output_dir is the destination directory.
%% File_name is the name of the beam file.
%% HTML is the html in the simplified xmerl format.
%% @spec html_to_file(string(), string(), [string()]) -> term()
html_to_file (Output_dir, File_name, API) ->
    HTML = make_api: api_to_html (API),
    Bytes = xmerl:export_simple(HTML, xmerl_html),
    {ok, File} = file: open (filename:join(Output_dir, File_name), [write]),
    ok = file: write (File, Bytes),
    file: close (File).

extract_functions (APIs) when is_list(APIs) ->
    lists:concat([do_extract_functions(A) || A <- APIs]).

do_extract_functions (API) ->
    {Root, []} = xmerl_scan: file(API, []),
    xmerl_xpath: string("function", Root).

function_information (Function) ->    
    Name = extract_attribute (name, Function#xmlElement.attributes),
    Parameters = extract_parameters (xmerl_xpath: string ("param", Function)),
    Return = extract_return (xmerl_xpath: string ("return", Function)),
    Comments = extract_comments (xmerl_xpath: string ("comment", Function),[]),
    {Name, Parameters, Return, Comments}.

extract_comments ([],Acc) ->
    lists: reverse(Acc);
extract_comments ([#xmlElement{content = [#xmlText{value= A}]}|T], Acc) ->
    extract_comments (T,[A|Acc]);
extract_comments ([_|T], Acc) ->
    extract_comments (T,Acc).

extract_return([]) ->
    default;
extract_return([#xmlElement{attributes=Attributes}]) ->
    Value = extract_attribute (type, Attributes),
    case hd(lists: reverse (Value)) of
	$\] -> array;
	_ -> default
    end.

extract_parameters (Parameters) ->
    extract_parameters (Parameters, []).

extract_parameters ([], Acc) ->
    lists: reverse (Acc);
extract_parameters ([#xmlElement{attributes=Attributes}|Tail],Acc) ->
    extract_parameters (Tail, [extract_attribute (name, Attributes)|Acc]).

extract_attribute (Type, []) ->
    exit ({no_attribute, Type});
extract_attribute (Type, [#xmlAttribute{name=Type, value=Value}|_]) ->
    Value;
extract_attribute (Type, [_|Tail]) ->
    extract_attribute(Type,Tail).

functions_to_module ("api", Functions) ->
    Fun = fun(Function) -> function_to_api_form(Function) end,
    Forms = lists: map (Fun, Functions),
    {"selenium_api.beam",full_api_forms (Forms)};
functions_to_module ("session" , Functions) ->
    Fun = fun(Function) -> function_to_session_form(Function) end,
    Forms = lists: map (Fun, Functions),
    {"selenium_session.beam",full_session_forms (Forms)}.

%% Generate an export attribute for all functions in Forms.
generate_export_all(Line, Forms) ->
  {attribute, Line, export, generate_export_all2(Forms, [])}.

generate_export_all2([], Exports) ->
  lists:reverse(Exports);
generate_export_all2([{function,_Line,Name,Arity,_Repr} | Forms], Exports) ->
  generate_export_all2(Forms, [{Name,Arity} | Exports]);
generate_export_all2([_ | Forms], Exports) ->
  generate_export_all2(Forms, Exports).

full_api_forms (Functions) ->
    Line = 1,
    [{attribute, Line, file, {"./selenium_api.erl", 1}},
     {attribute, Line, module, selenium_api},
     generate_export_all(Line, Functions)]
    ++ Functions.

full_session_forms (Functions) ->
    Line = 1,
    Stop = stop_session_form (Line),
    AllFunctions = Functions ++ Stop,
    [{attribute, Line, file, {"./selenium_session.erl", 1}},
     {attribute, Line, module, {selenium_session, ['Session']}},
     generate_export_all(Line, AllFunctions)]
    ++ AllFunctions.

function_to_api_form (Function) ->
    Line = 3,
    {Name, Param, Return, _} = function_information (Function),
    Param_as_variable = [{var, Line, list_to_atom (P)} || P <- Param],
    Name_as_atom = list_to_atom (Name),

    Parameters = [{var, Line, 'Session'}] ++ Param_as_variable,

    Selenium_cmd_call_form = selenium_cmd_call_form (Return,
						     Name_as_atom,
						     Param_as_variable),
						     
    api_function_form (Name, Selenium_cmd_call_form, Parameters).


function_to_session_form (Function) ->
    Line = 3,
    {Name, Param, Return, _} = function_information (Function),

    Name_as_atom = list_to_atom (Name),
    Param_as_variable = [{var, Line, list_to_atom (P)} || P <- Param],
    
    Selenium_cmd_call_form = selenium_cmd_call_form (Return,
						     Name_as_atom,
						     Param_as_variable),
    
    api_function_form (Name, Selenium_cmd_call_form, Param_as_variable).

api_function_form (Name, Selenium_cmd_call_form, Parameters) ->
    Name_as_erlang_atom = camel_case_to_erlang_atom (Name),
    Line = 3,
    Clauses = [{clause, Line, Parameters, [], Selenium_cmd_call_form}],
    {function, Line, Name_as_erlang_atom, length (Parameters), Clauses}.

stop_session_form (Line) ->
    Instructions = [{call, Line,
		     {remote, Line,
		      {atom, Line, selenium},
		      {atom, Line, stop}},
		     [{var, Line, 'Session'}]
		    }],
    Clauses = [{clause, Line, [], [], Instructions}],
    [{function, Line, stop_session, 0, Clauses}].

put_parameter_in_call (Parameters) ->
    list_to_tuple (transform_list_to_cons (Parameters)).

transform_list_to_cons ([]) ->
    [nil, 3];
transform_list_to_cons ([H|T]) ->
    [cons, 3, H, put_parameter_in_call (T)].

parameter_list_form ([]) ->
    [{nil,3}];
parameter_list_form (Variable_forms) ->
    [put_parameter_in_call (Variable_forms)].


selenium_cmd_call_form (Return, API_function, Variable_forms) ->
    Line = 3,
    Selenium_function = return_to_selenium_cmd (Return),
    Additional_parameters = parameter_list_form (Variable_forms),
    [{call, Line, {remote,Line,
		   {atom, Line, selenium},
		   {atom, Line, Selenium_function}},
      [{var, Line, 'Session'},
       {atom, Line, API_function} | 
       Additional_parameters
      ]
     }].

return_to_selenium_cmd (default) ->
    cmd;
return_to_selenium_cmd (array) ->
    cmd_array.



sort_xml_element (A, B) ->
    Name_a = extract_attribute (name, A#xmlElement.attributes),
    Name_b = extract_attribute (name, B#xmlElement.attributes),
    Name_a < Name_b.

camel_case_to_erlang_atom (Name) ->
    list_to_atom(camel_case_to_erlang (Name, [])).

camel_case_to_erlang (Name) ->
    camel_case_to_erlang (Name, []).

camel_case_to_erlang ([], Acc) ->
    lists: reverse (Acc);
camel_case_to_erlang ([H|Tail], Acc) when H >= $A, H =< $Z ->
    camel_case_to_erlang (Tail, [H+32,$_|Acc]);
camel_case_to_erlang ([H|Tail], Acc) ->
    camel_case_to_erlang (Tail, [H|Acc]).
