%%
%%  @doc Application Supervisor. 
%%

-module(cowtracks_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_all, 20, 3600}, [
        {cowtracks_server, {cowtracks_server, start_link, []}, 
         permanent, 5000, worker, [cowtracks_server]}
    ]}}.
