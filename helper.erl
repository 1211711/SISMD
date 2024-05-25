-module(helper).
-export([get_process_alias/1]).

get_process_alias(Pid) ->
    RegisteredNames = registered(),
    case lists:filter(fun(Name) -> whereis(Name) =:= Pid end, RegisteredNames) of
        [] -> undefined;
        [Alias] -> Alias
    end.