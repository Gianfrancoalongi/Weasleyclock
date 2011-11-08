%%%-------------------------------------------------------------------
%%% @author Gianfranco Alongi <zenon@zentop>
%%% @copyright (C) 2011, Gianfranco Alongi
%%% Created : 30 Aug 2011 by Gianfranco Alongi <zenon@zentop>
%%%-------------------------------------------------------------------
-module(wc_backend).
-behaviour(gen_server).
-include("wc_backend.hrl").

%% API
-export([start_link/1]).
-export([add_path/2]).
-export([places_string/0,
	 users_string/0]).
-export([all_users_and_paths/0]).
-export([remove_path/2]).
-export([get_paths/1]).
-export([pointpairs_to_path/3]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {lsock}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).  

places_string() ->
    All = ets:foldl(
	    fun({_,Paths}, Acc) ->
		    Acc ++ ["\""++Path#path.id++"\"" || Path <- Paths]
	    end,
	    [],
	    paths),
    Sorted = lists:usort(All),
    string:join(Sorted,",\n").
 
users_string() ->
    All = ets:foldl(
	    fun({Id,_}, Acc) ->
		    ["\""++Id++"\""|Acc]
	    end,
	    [],
	    paths), 
    Sorted = lists:usort(All),
    string:join(Sorted,",\n").

all_users_and_paths() ->
    ets:foldl(
      fun({Id,Paths},Acc) ->
	      Str = paths_to_pointpairs(Paths),
	      Acc++"<h3>"++Id++"</h3></br>"++Str
      end,
      "",
      paths).

add_path(Id,Path) -> 
    case ets:lookup(paths,Id) of
	[] ->
	    ets:insert(paths,{Id,[Path]});	
	[{Id,Paths}] ->
	    ets:insert(paths,{Id,[Path|Paths]})
    end,
    io:format("Added path ~p for user ~p ~n",[Path,Id]),
    ok.

remove_path(Id,Name) ->
    case ets:lookup(paths,Id) of
	[] ->
	    ok;
	[{Id,Paths}] ->
	    ets:insert(paths,{Id,[P||P <- Paths,P#path.id =/= Name]})
    end,
    io:format("Removed path ~p for user ~p ~n",[Name,Id]),
    ok.

get_paths(Id) ->
    Paths = case ets:lookup(paths,Id) of
		[] -> 
		    [];
		[{Id,PathList}] ->
		    PathList
	    end,
    Res = paths_to_pointpairs(Paths),
    io:format("Returning paths ~p for user ~p ~n",[Res,Id]),
    Res.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([LSock]) ->    
    {ok,#state{lsock = LSock},0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp,AccSock,Data}, State) ->    
    io:format(" ~p ======> Received ~p FROM ~p ~n",[self(),Data,AccSock]),
    Point = in_to_point(Data -- "\n\r"),
    Prediction = prediction(Point),
    wc_websvc:new_data(Point#point.id ++ "," ++ Prediction),
    {noreply, State};
handle_info({tcp_closed,Sock},State) -> 
    io:format(" ~p ======> Closed socket ~p~n",[self(),Sock]),
    {noreply,State};    
handle_info(timeout,State) -> 
    io:format(" ~p ======> Waiting to accept a connection on 55200 with active true~n",[self()]),
    {ok,_Sock} = gen_tcp:accept(State#state.lsock),
    io:format(" ~p ======> Accepted connection on 55200~n",[self()]),
    wc_backend_sup:start_child(),
    {noreply,State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

in_to_point(Data) ->
    [Id|Toks] = string:tokens(Data,","),
    PList = lists:foldl(fun(Tok,Acc) -> 
				Elem = list_to_tuple(string:tokens(Tok,"=")),
				[Elem|Acc] 
			end,[],Toks),
    #point{id = Id,
	   time = list_to_integer(proplists:get_value("mTime",PList)),
           latitude = list_to_float(proplists:get_value("mLatitude",PList)),
           longitude = list_to_float(proplists:get_value("mLongitude",PList))}.

prediction(Point) ->
    Vector = case ets:lookup(data_points,Point#point.id) of
		 [] -> [Point];
		 [{_,List}] -> [Point|List]
	     end,
    Paths = case ets:lookup(paths,Point#point.id) of
		[{_,P}] -> P;
		[] -> []
	    end,
    predict(Vector,Paths).

predict(_,[]) -> "Lost";
predict(Vector,Paths) ->
    Dist_Path = lists:foldl(
		  fun(Path,Acc) ->
			  Dist = vector_to_path_dist(Vector,Path),
			  [{Dist,Path}|Acc]
		  end,
		  [],Paths),
    [{_,T}|_] = lists:sort(Dist_Path),
    T#path.id.

vector_to_path_dist(Vector,#path{points = Points} = _) ->
    LV = length(Vector),
    LP = length(Points),
    {AdjV,AdjP} = case LV > LP of
		      true ->
			  {lists:sublist(Vector,LP),Points};
		      false ->
			  {Vector,lists:sublist(Points,LV)}
		  end,
    lists:sum([point_distance(V,P) || {V,P} <- lists:zip(AdjV,AdjP)]).

point_distance(A,B) ->
    #point{latitude = Vlat, longitude = Vlong} = A,
    #point{latitude = Plat, longitude = Plong} = B,
    Xd = (Vlat - Plat)*(Vlat - Plat),
    Yd = (Vlong - Plong)*(Vlong - Plong),
    math:sqrt( Xd + Yd ).

pointpairs_to_path(Id,Name,PointPairs) ->
    #path{id = Name,
	  points = [begin
			[Lat,Long] = string:tokens(PointPair,":"),
			#point{id = Id,
			       time = undefined,
			       latitude = list_to_float(Lat), 
			       longitude = list_to_float(Long)
			      }
		    end || PointPair <- PointPairs]}.

paths_to_pointpairs(Paths) ->
    lists:foldl(
      fun(Path,Acc) ->
	      Points = Path#path.points,
	      PointsStr = points_to_pointpairs(Points),
	      Acc++"</br>"++
		  Path#path.id++";"++PointsStr
      end,"",Paths).
    

points_to_pointpairs(PointsList) ->
    lists:foldl(
      fun(Point,Acc) ->
	      Acc++","++
		  float_to_list(Point#point.latitude)++":"++
		  float_to_list(Point#point.longitude)
      end,"",PointsList).


