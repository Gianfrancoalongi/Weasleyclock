%%%-------------------------------------------------------------------
%%% @author Gianfranco Alongi <zenon@zentop>
%%% @copyright (C) 2011, Gianfranco Alongi
%%% Created : 31 Aug 2011 by Gianfranco Alongi <zenon@zentop>
%%%-------------------------------------------------------------------
-module(wc_websvc).
-behaviour(gen_server).
-define(STD_PORT,55300).

%% API
-export([start_link/0,
	 new_data/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

new_data(Data) ->
    ets:insert(data_storage,{now(),{[],Data}}),
    ok.
    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ets:new(data_storage,[public,named_table,ordered_set]),
    WebSvcPort = case application:get_env(wc_websvc,port) of
		     undefined -> ?STD_PORT;
		     {ok,P} -> P
		 end,		     
    misultin:start_link([{port, WebSvcPort}, 
			 {loop, fun(Req) -> handle_http(Req, WebSvcPort) end}, 
			 {ws_loop, fun(Ws) -> handle_websocket(Ws) end}]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_http(Req, Port) -> 
    Args = Req:parse_qs(),
    case Req:resource([lowercase, urldecode]) of
	["all"] -> 
	    All = wc_backend:all_users_and_paths(),
	    Req:ok("<html>"++All++"</html>");
	["addpath"] ->
	    Id = proplists:get_value("id",Args),
	    Name = proplists:get_value("name",Args),
	    PointPairs = string:tokens(proplists:get_value("points",Args),","),
	    Path = wc_backend:pointpairs_to_path(Id,Name,PointPairs),
	    wc_backend:add_path(Id,Path),
	    Req:ok("Nice");
	["removepath"] ->
	    Id = proplists:get_value("id",Args),
	    Name = proplists:get_value("name",Args),
	    wc_backend:remove_path(Id,Name),
	    Req:ok("Nice");
	["getpaths"] ->
	    Id = proplists:get_value("id",Args),
	    Res = wc_backend:get_paths(Id),
	    Req:ok([{"Content-Type","text/html"}],[Res]);
	["filtered"] ->
	    _Ids = proplists:get_value("ids",Args),
	    Req:ok("Service not supported yet.");
	X ->
	    io:format("Request ~p~n",[X]),
	    PrivPath = code:priv_dir(wc_websvc),
	    Page = filename:join([PrivPath,"content.html"]),
	    {ok,Bin} = file:read_file(Page),
	    Places = wc_backend:places_string(),
	    Users = wc_backend:users_string(),
	    BaseContent = erlang:binary_to_list(Bin),
	    {ok,ExposedIp} = application:get_env(wc_websvc,exposed_ip),
	    Replaced = replace([{"#PORT",integer_to_list(Port)},
				{"#IP",ExposedIp},
				{"#PLACES",Places},
				{"#USERS",Users}],BaseContent),
	    Req:ok([{"Content-Type", "text/html"}],[Replaced])  
    end.

handle_websocket(Ws) ->
    receive
	{browser, Data} ->
	    Sent = ["received '", Data, "'"],
	    Ws:send(Sent),
	    handle_websocket(Ws);
	_Ignore ->    
	    handle_websocket(Ws)
    after 1000 ->
	    ets:foldl(
	     fun({K,{Seen,V}},_) ->
			      ID = {misultin_ws:get(peer_addr,Ws),
				    misultin_ws:get(socket,Ws)},
			      case lists:member(ID,Seen) of
				  true -> ignore;
				  false ->
				      io:format("Web_svc is sending ~p~n",[V]),
				      Ws:send(V),
				      ets:insert(data_storage,{K,{[ID|Seen],V}})
			      end
		      end,[],data_storage),
	    handle_websocket(Ws)
    end.

replace([],Str) -> Str;
replace([{Pattern,ReplaceWith}|X],Str) -> 
    Res = re:replace(Str, Pattern, ReplaceWith,[{return,list}]),
    replace(X,Res).
    
