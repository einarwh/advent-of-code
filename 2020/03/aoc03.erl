-module(aoc03).
-export([run1/0, run2/0]).

read(File) -> 
    {ok, Bin} = file:read_file(File),
    string:lexemes(binary_to_list(Bin), "\n").

check(Pos, Line) -> 
    case lists:nth(Pos + 1, Line) of 
        $# -> 1;
        _ -> 0
    end.

trees({R, L}, Items) -> 
    Indexed = lists:zip(lists:seq(0, length(Items) - 1), Items),
    Included = lists:filter(fun ({Ix, _}) -> (Ix rem L) == 0 end, Indexed),
    Values = lists:map(fun ({Ix, Line}) -> check((R * Ix) rem (length(Line)), Line) end, Included),
    lists:sum(Values).

run(Configs) -> 
    Input = read("input.txt"),
    Results = lists:map(fun (C) -> trees(C, Input) end, Configs),
    lists:foldl(fun (X, Prod) -> X * Prod end, 1, Results).

run1() -> 
    run([{3, 1}]).

run2() -> 
    run([{1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2} ]).