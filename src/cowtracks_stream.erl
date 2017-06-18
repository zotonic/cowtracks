
-module(cowtracks_stream).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behavior(cowboy_stream).

-export([
    init/3,
    data/4,
    info/3,
    terminate/3,
    early_error/5
]).

-record(state, {
    next :: any(),

    request_ref :: reference(),
    start_time :: integer()
}).

-type state() :: #state{}.

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
         -> {cowboy_stream:commands(), {module(), state()} | undefined}.
init(StreamID, Req, Opts) ->
    StartTime = erlang:monotonic_time(micro_seconds),
    Ref = erlang:make_ref(),
    cowtracks:track(Ref, {start, StartTime}),
    Req1 = Req#{cowtracks_ref => Ref},

    {Commands, Next} = cowboy_stream:init(StreamID, Req1, Opts),
    {Commands, #state{next = Next, start_time = StartTime, request_ref = Ref}}.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), binary(), {Handler, State} | undefined)
        -> {cowboy_stream:commands(), {Handler, State} | undefined}
        when Handler::module(), State::state().
data(StreamID, IsFin, Data, #state{next = Next} = State) ->
    {Commands, Next1} = cowboy_stream:data(StreamID, IsFin, Data, Next),
    {Commands, State#state{next = Next1}}.


-spec info(cowboy_stream:streamid(), any(), {Handler, State} | undefined)
        -> {cowboy_stream:commands(), {Handler, State} | undefined}
        when Handler::module(), State::state().
info(StreamID, Response, #state{next = Next, request_ref = Ref} = State) ->
    case Response of
        {response, Status, Headers, _Body} -> track_response(Ref, Status, Headers);
        {headers, Status, Headers} -> track_headers(Ref, Status, Headers);
        _ -> ignore
    end,
    {Commands, Next1} = cowboy_stream:info(StreamID, Response, Next),
    {Commands, State#state{next = Next1}}.

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), {module(), state()} | undefined) -> ok.
terminate(StreamID, Reason, #state{request_ref = Ref, next = Next}) ->
    track_done(Ref),
    cowboy_stream:terminate(StreamID, Reason, Next).

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(), cowboy_stream:partial_req(), Resp, cowboy:opts())
        -> Resp when Resp::cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
    %% TODO: track the early error.
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).


%%
%% Helpers 
%%

track_response(Ref, Status, Headers) -> 
    cowtracks:track(Ref, {response, Status, Headers, t()}).

track_headers(Ref, Status, Headers) -> 
    cowtracks:track(Ref, {headers, Status, Headers, t()}).

track_done(Ref) ->
    cowtracks:track(Ref, {done, t()}).

t() -> 
    erlang:monotonic_time(micro_seconds).
