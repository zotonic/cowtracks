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
    StartTime = erlang:monotonic_time(microseconds),
    Ref = erlang:make_ref(),
    Req1 = Req#{cowtrack_ref => Ref},
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
info(StreamID, Response, #state{next = Next, start_time = StartTime, request_ref = Ref} = State) ->
    case Response of
        {response, Status, Headers, _Body} -> track_response(Status, Headers, Ref, StartTime);
        {headers, Status, Headers} -> track_headers(Status, Headers, Ref, StartTime);
        _ -> ignore
    end,
    {Commands, Next1} = cowboy_stream:info(StreamID, Response, Next),
    {Commands, State#state{next = Next1}}.

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), {module(), state()} | undefined) -> ok.
terminate(StreamID, Reason, #state{next = Next}) ->
    cowboy_stream:terminate(StreamID, Reason, Next).

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(), cowboy_stream:partial_req(), Resp, cowboy:opts())
        -> Resp when Resp::cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
    %% TODO: track the early error.
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).


%%
%% Track
%%

track_response(Status, Headers, Ref, StartTime) ->
    ok.

track_headers(Status, Headers, Ref, StartTime) ->
    ok.

