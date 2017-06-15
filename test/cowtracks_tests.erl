%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% %% @hidden
%%

-module(cowtracks_tests).

-include_lib("eunit/include/eunit.hrl").

start_stop_test_() ->
    {"The server can be started, stopped and has a registered name",
     {setup,
      fun start/0,
      fun stop/1,
      fun is_registered/1}}.

is_registered(Pid) ->
    ok.

%%
%% Helpers
%%
start() ->
    {ok, Pid} = cowtracks_server:start_link(),
    Pid.
 
stop(_) ->
    cowtracks_server:stop().

