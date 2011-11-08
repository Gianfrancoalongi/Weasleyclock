-module(wc_backend_app).

-behaviour(application).
-define(STD_PORT,55200).
%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Port = case application:get_env(wc_backend,port) of
	       undefined -> ?STD_PORT;
	       {ok,ConfPort} -> ConfPort
	   end,
    {ok,LSock} = gen_tcp:listen(Port,[{active,true},{reuseaddr,true}]),
    io:format("Started listening on 55200 with reuseaddr = true~n",[]),
    wc_backend_sup:start_link(LSock).

stop(_State) ->
    ok.
