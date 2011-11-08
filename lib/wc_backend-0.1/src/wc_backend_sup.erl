-module(wc_backend_sup).
-behaviour(supervisor).

%% API
-export([start_link/1,
	 start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type,Arg), {I, {I, start_link, [Arg]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(LSock) ->
    ets:new(data_points,[public,named_table]),
    ets:new(paths,[public,named_table]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LSock]).

start_child() ->
    supervisor:start_child(?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([LSock]) ->
    Children = [?CHILD(wc_backend,worker,LSock)],
    {ok,{{simple_one_for_one, 10, 10}, Children}}.
