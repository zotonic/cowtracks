%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @hidden

-module(cowtracks_server).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([
    start_link/0, start_link/2,
    stop/0,
    flush/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).

-record(state, {
    handler,
    timeout,
    handler_state
}).

start_link() ->
    {ok, Handler} = application:get_env(cowtracks, handler),
    {ok, Args} = application:get_env(cowtracks, args),
    start_link(Handler, Args).

start_link(Handler, Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Handler, Args], []).

%% @doc Stop the worker, all pending work will be lost.
stop() ->
    gen_server:call(?MODULE, stop).

flush() ->
    whereis(?MODULE) ! flush.

%%
%% gen_server callbacks
%%

init([Handler, Args]) ->
    ?MODULE = ets:new(?MODULE, [ordered_set, named_table, public,
            {keypos, 2},
            {write_concurrency, true}]),

    % ets:insert(?MODULE, #counter{name=next}),

    {ok, Timeout, HandlerState} = Handler:init(self(), Args),

    erlang:send_after(Timeout, self(), flush),

    {ok, #state{handler=Handler, timeout=Timeout, handler_state=HandlerState}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush, #state{handler=Handler, handler_state=HandlerState, timeout=Timeout}=State) ->
    % [#counter{value=Upto}] = ets:lookup(?MODULE, next),
    % flush(?MODULE, Upto, Handler, HandlerState),
    Handler:handle_flush_done(self(), HandlerState),
    erlang:send_after(Timeout, self(), flush),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

