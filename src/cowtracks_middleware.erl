%
%
%

-module(cowtracks_middleware).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl").

-behaviour(cowboy_middleware).

-export([
    execute/2
]).

-export([
    execute_middleware/4,
    resume_middleware/6
]).


%% @doc Call cowmachine to handle the request with the given controller.
-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req}
    when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(#{cowtracks_ref := Ref}=Req, #{cowtracks_middleware := Middlewares}=Env) ->
    execute_middleware(Ref, Req, Env, Middlewares).

%%
%%
%%

execute_middleware(_, _, _, []) ->
	ok; %% @todo Maybe error reason should differ here and there.
execute_middleware(Ref, Req, Env, [Middleware|Tail]) ->
    Start = t(),
    R = Middleware:execute(Req, Env),
    Duration = t() - Start,
    track(Ref, Middleware, R, Duration),
    handle_middleware(Ref, Env, R, Tail).

resume_middleware(Ref, Env, Tail, Module, Function, Args) ->
    handle_middleware(Ref, Env, apply(Module, Function, Args), Tail).

handle_middleware(Ref, _Env, {ok, Req, Env}, Middlewares) ->
	execute_middleware(Ref, Req, Env, Middlewares);
handle_middleware(Ref, Env, {suspend, Module, Function, Args} , Middlewares) ->
    proc_lib:hibernate(?MODULE, resume_middleware, [Ref, Env, Middlewares, Module, Function, Args]);
handle_middleware(_Ref, _Env, {stop, _Req}, _Middlewares) ->
    ok.

track(Ref, Middleware, {ok, Req, Env}, Duration) ->
    cowtracks:track(Ref, {middleware, Middleware, Req, Env, Duration});
track(_Ref, _Middleware, _, _Duration) ->
    ok.

t() -> erlang:monotonic_time(micro_seconds).
