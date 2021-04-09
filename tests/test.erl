-module(test).
-compile(export_all).
-compile(nowarn_export_all).

test() ->
    %% try call local function
    case catch lists:thing_to_list(?MODULE) of
        {'EXIT', Reason} ->
            io:format("~p~n", [Reason]);
        _ ->
            ok
    end,
    %% export it
    exporter:export(lists, thing_to_list, 1),
    %% call again
    lists:thing_to_list(?MODULE).
