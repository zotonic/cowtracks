

-module(cowtracks).

-export([
    track/2
]).

track(Ref, What) ->
    cowtracks_server:push({Ref, What}).