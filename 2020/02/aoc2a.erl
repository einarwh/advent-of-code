-module(aoc02a).
-export([run/0]).

read(File) -> 
    {ok, Bin} = file:read_file(File),
    string:lexemes(binary_to_list(Bin), "\n").

count_char(Char, Pwd) -> 
    Matches = lists:filter(fun (C) -> C == Char end, Pwd),
    length(Matches).

check(Line) -> 
    Pattern = "^(\\d+)\\-(\\d+) (\\w): (\\w+)",
    Options = [{capture, all, list}],
    {match, Captured} = re:run(Line, Pattern, Options),
    [_, MinStr, MaxStr, CharStr, Pwd] = Captured,
    Matches = count_char(lists:nth(1, CharStr), Pwd),
    Min = list_to_integer(MinStr),
    Max = list_to_integer(MaxStr),
    (Min =< Matches) and (Matches =< Max).

run() -> 
    Input = read("input.txt"),
    Valid = lists:filter(fun (L) -> check(L) end, Input),
    length(Valid).