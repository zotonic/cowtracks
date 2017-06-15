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

%%
%% TODO: initialize the ets tables, and start process which periodically checks the tables.
%%

init([]) ->
    ignore.


