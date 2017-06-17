%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% %% @hidden
%%

-module(cowtracks_test).

-include_lib("eunit/include/eunit.hrl").

-export([
    init/2
]).

start_stop_test() ->
    ?assertEqual(ok, start()),
    ?assertEqual(ok, stop([])),
    ok.


%%
%% Helpers
%%

start() ->
    application:set_env(cowtracks, handler, ?MODULE),
    application:set_env(cowtracks, args, []),
    application:start(cowtracks).
 
stop(_) ->
    application:stop(cowtracks).

%
% Cowtrack handler callback.
%

init(_Pid, _Args) ->
    {ok, 1000, []}.



