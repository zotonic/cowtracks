%%
%%

-module(cowtracks_handler).

-callback handle_track(Msg, Args) ->
    any()
    when Msg :: cowtracks:msg(), Args :: any().

-callback init(Pid, Args) ->
    {ok, Timeout, State}
    when Pid :: pid(), Args::any(), Timeout::integer(), State::any().

-callback handle_value(Pid, State) ->
    {keep, any()} | {delete, any()}
    when Pid::pid(), State::any().

-callback handle_flush_done(Pid, State) ->
    any()
    when Pid::pid(), State::any().