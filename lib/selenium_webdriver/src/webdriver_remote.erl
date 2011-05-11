%%% Copyright(c) 2011 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.

%% @author Nicolas Charpentier <open_source@charpi.net> [http://charpi.net]
%% @copyright 2011 Nicolas Charpentier
-module(webdriver_remote).

-type(abstract_session() :: {string(), integer(), any()}).
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

-spec quit(abstract_session()) -> {ok, no_content}.
quit(Session) ->
    request(delete, path(Session), []).
    
-spec get(abstract_session(), string() | binary()) -> {ok, no_content}.
get(Session, Destination) when is_list(Destination) ->
    get(Session, list_to_binary(Destination));
get(Session, Destination) when is_binary(Destination) ->
    post(path(Session, "url"), to_json([{url, Destination}])).

-spec get_current_url(abstract_session()) -> {ok, no_content}.
get_current_url(Session) ->
    request(get, path(Session, "url"), []).

-spec(timeout(abstract_session(), async_script | implicit_wait, integer()) ->
	     {ok, no_content}).
timeout(Session, Timeout, Delay) when Timeout == async_script;
				      Timeout == implicit_wait,
				      is_integer(Delay) ->
    Body = to_json([{ms, Delay}]),
    post(path(Session,"timeouts/" ++ atom_to_list(Timeout)), Body).

-spec(get_window_handle(abstract_session()) -> {ok, term()}).
get_window_handle(Session) ->
    request(get, path(Session, "window_handle"), []).
	     
-spec(get_window_handles(abstract_session()) -> {ok, term()}).
get_window_handles(Session) ->
    request(get, path(Session, "window_handles"), []).

-spec(forward(abstract_session()) -> {ok, term()}).
forward(Session) ->
    post(path(Session,"forward"), " ").

-spec(back(abstract_session()) -> {ok, term()}).
back(Session) ->
    post(path(Session,"back"), " ").
    
-spec(refresh(abstract_session()) -> {ok, term()}).
refresh(Session) ->
    post(path(Session, "refresh") , " ").

-spec(execute(abstract_session(), string()) -> {ok, term()}).	     
execute(Session, Script) ->    
    post(path(Session, "execute"), 
 	 to_json([{<<"script">>, list_to_binary(Script)},
		  {<<"args">>, []}])).

screenshot(Session) ->
   request(get, path(Session, "screenshot"), []).

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
	13 -> 
	    {struct, Message} = proplists:get_value(<<"value">>, List),
	    {error, {13, Message}};
	_  ->
	    {error, List}
    end.
