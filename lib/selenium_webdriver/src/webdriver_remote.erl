%%% Copyright(c) 2011 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.

%% @author Nicolas Charpentier <open_source@charpi.net> [http://charpi.net]
%% @copyright 2011 Nicolas Charpentier
-module(webdriver_remote).

-type(abstract_session() :: {string(), integer(), any()}).
-type(command_result() :: {ok, term()}).

-type(capabilities() :: [{browserName, firefox|internet_explorer|htmlunit|iphone|chrome} |
			 {version, string()} |
			 {javascriptEnabled, true|false} |
			 {platform, 'WINDOWS'|'XP'|'VISTA'|'MAC'|'LINUX'|'UNIX'|'ANY'}]).

-export([session/3]).
-export([session/1]).
-export([quit/1]).
-export([get/2]).
-export([get_current_url/1]).
-export([timeout/3]).
-export([get_window_handle/1]).
-export([get_window_handles/1]).
-export([forward/1]).
-export([back/1]).
-export([refresh/1]).
-export([execute/2]).
-export([screenshot/1]).
-export([available_engines/1]).
-export([active_engine/1]).
-export([frame/2]).
-export([find_elements/3]).
-export([get_attribute/3]).
-export([click/2]).
-export([text/2]).
-export([title/1]).
-export([switch_to_window/2]).
-export([delete_window/1]).
-export([speed/1]).
-export([speed/2]).

-define(CONTENT_TYPE,"application/json;charset=UTF-8").

-spec session(string(), integer(), capabilities()) -> {ok, abstract_session()}.
session(Host, Port, Capabilities) ->
    application:start(inets),
    Result = post(path({Host,Port,undefined}), to_json([{desiredCapabilities, {struct, Capabilities}}])),
    case Result of
	{ok, {302, H}} ->
	    Location = proplists:get_value("location", H),
%%	    io:format(user,"Location ~p ~n",[Location]),
	    Id = hd(lists:reverse(string:tokens(Location,"/"))),
	    {ok, {Host, Port, Id}};
	E -> E
    end.

session(Session) ->
    request(get, path(Session), []).

-spec quit(abstract_session()) -> command_result().
quit(Session) ->
    request(delete, path(Session), []).
    
-spec get(abstract_session(), string() | binary()) -> command_result().
get(Session, Destination) when is_list(Destination) ->
    get(Session, list_to_binary(Destination));
get(Session, Destination) when is_binary(Destination) ->
    post(path(Session, "url"), to_json([{url, Destination}])).

-spec get_current_url(abstract_session()) -> command_result().
get_current_url(Session) ->
    request(get, path(Session, "url"), []).

-spec(timeout(abstract_session(), async_script | implicit_wait, integer()) ->
	     command_result()).
timeout(Session, Timeout, Delay) when Timeout == async_script;
				      Timeout == implicit_wait,
				      is_integer(Delay) ->
    Body = to_json([{ms, Delay}]),
    post(path(Session,"timeouts/" ++ atom_to_list(Timeout)), Body).

-spec(get_window_handle(abstract_session()) -> command_result()).
get_window_handle(Session) ->
    request(get, path(Session, "window_handle"), []).
	     
-spec(get_window_handles(abstract_session()) -> command_result()).
get_window_handles(Session) ->
    request(get, path(Session, "window_handles"), []).

-spec(forward(abstract_session()) -> command_result()).
forward(Session) ->
    post(path(Session,"forward"), " ").

-spec(back(abstract_session()) -> command_result()).
back(Session) ->
    post(path(Session,"back"), " ").
    
-spec(refresh(abstract_session()) -> command_result()).
refresh(Session) ->
    post(path(Session, "refresh") , " ").

-spec(execute(abstract_session(), string()) -> command_result()).	     
execute(Session, Script) ->    
    post(path(Session, "execute"), 
 	 to_json([{<<"script">>, list_to_binary(Script)},
		  {<<"args">>, []}])).

-spec(screenshot(abstract_session()) -> command_result()).
screenshot(Session) ->
   request(get, path(Session, "screenshot"), []).

available_engines(Session) ->
    request(get, path(Session, "ime/available_engines"), []).

active_engine(Session) ->
    request(get, path(Session, "ime/active_engine"), []).

-spec(frame(abstract_session(), string()| number() | null) ->
	     command_result()).
frame(Session, Frame)  ->
    Body = case Frame of
	       L when is_list(L) ->
		   list_to_binary(L);
	       I when is_integer(I) ->
		   I;
	       null ->
		   null
	   end,
    post(path(Session, "frame"), 
	 to_json([{<<"id">>, Body}])).

-spec(find_elements(abstract_session(), atom()|binary(), string()) -> command_result()).
find_elements(Session, Using, Value) ->
    Res = post(path(Session, "elements"), 
	       to_json([{<<"using">>, Using},
			{<<"value">>, list_to_binary(Value)}])),
    case Res of
	{ok , []} ->
	    {error, {7, no_such_element}};
	{ok, List} ->
	    {ok, [webelement_id(E) || E <- List] };
	Other ->
	    io:format(user,"~p~n",[Other]),
	    Other
    end.

-spec(get_attribute(abstract_session(), string()|binary(), string()) ->
	     command_result()).
get_attribute(Session, Id, Value) when is_binary(Id) ->
    get_attribute(Session, binary_to_list(Id), Value);
get_attribute(Session, Id, Value) ->
    request(get, path(Session, "element/" ++ Id ++ "/attribute/" ++ Value),
	    []).


click(Session, Id) when is_binary(Id) ->
    click(Session, binary_to_list(Id));
click(Session, Id) ->
    post(path(Session, "element/" ++ Id ++ "/click"), " ").

text(Session, Id) when is_binary(Id) ->
    text(Session, binary_to_list(Id));
text(Session, Id) ->
    request(get, path(Session, "element/" ++ Id ++ "/text"), []).

title(Session) ->
    request(get, path(Session, "title"), []).

-spec(switch_to_window(abstract_session(), string()) ->
	     command_result()).
switch_to_window(Session, Name) ->
    post(path(Session,"window"), to_json([{<<"name">>, list_to_binary(Name)}])).

-spec(delete_window(abstract_session()) ->
	     command_result()).
delete_window(Session) ->
    request(delete,path(Session,"window"),[]).

-spec(speed(abstract_session()) -> command_result()).
speed(Session) ->
    request(get, path(Session,"speed"), []).

-spec(speed(abstract_session(), 'SLOW' | 'MEDIUM' | 'FAST') -> command_result()).
speed(Session, Speed) ->
    post(path(Session,"speed"), to_json([{<<"speed">>,Speed}])).

webelement_id({struct, [{<<"ELEMENT">>, Id}]}) ->
    Id.

post(Path,Body) ->
    request(post, Path, Body).

request(Method, Path, Body) ->
    %%    io:format(user,"~p ~p ~p~n",[Method, Url ++ Path, Body]),
    case httpc:request(Method, req(Path, Body), [], []) of
	{error,_} = E -> 
	    E;
	{ok, {{_,204,_}, _,[]}} ->
	    {ok, no_content};
	{ok, {{_,302,_}, H, _B}} ->
	    {ok,{302,H}};
	{ok, {{_,200,_}, H, B}} ->
	    decode_response(decode_body(H,B));
	{ok, {{_,_Code,_Reason}, H, B}} ->
	    decode_response(decode_body(H,B))
    end.

path({Host, Port, Id}) ->
    Base = "http://" ++ Host ++ ":" ++ integer_to_list(Port) ++"/wd/hub/session",
    case Id of
	undefined -> Base;
	_ -> Base ++ "/" ++ Id
    end.

path(Session, Tail) ->
    path(Session) ++ "/" ++ Tail.

req(Path,[]) -> 
    {Path, []};
req(Path,Body) ->
    {Path, [], ?CONTENT_TYPE, Body}.
	    
to_json(Proplist) ->
    Res = mochijson2:encode({struct, Proplist}),
    iolist_to_binary(Res).

from_json(List) ->
%%    io:format(user,"~p~n",[List]),
    mochijson2:decode(list_to_binary(List)).

decode_body(Headers, Body) ->
    case proplists:get_value("content-type",Headers) of
	undefined ->
	    Body;
	"application/json;charset=UTF-8" ->
	    {struct, Object} = from_json(string:strip(Body,both,0)),
	    Object;
	_ -> 
	    Body
    end.

decode_response(List) ->
    case proplists:get_value(<<"status">>, List) of
	0 ->
	    {ok, proplists:get_value(<<"value">>, List)};
	Other -> 
	    case lists:member(Other,status_codes()) of
		true ->
		    {struct, Message} = proplists:get_value(<<"value">>, List),
		    {error, {Other, Message}};
		_ ->
		    {error, List}
	    end
    end.


status_codes() ->
    [7,8,9,10,11,12,13,15,17,19,23,24,25,28].
