-module(aoc02b).
-export([run/0]).

read(File) -> 
    {ok, Bin} = file:read_file(File),
    string:lexemes(binary_to_list(Bin), "\n").

lookup(IxStr, Pwd) -> 
    lists:nth(list_to_integer(IxStr), Pwd).

check(Line) -> 
    Pattern = "^(\\d+)\\-(\\d+) (\\w): (\\w+)",
    Options = [{capture, all, list}],
    {match, Captured} = re:run(Line, Pattern, Options),
    [_, Ix1Str, Ix2Str, CharStr, Pwd] = Captured,
    Char = lists:nth(1, CharStr),
    Char1 = lookup(Ix1Str, Pwd),
    Char2 = lookup(Ix2Str, Pwd),
    (Char == Char1) xor (Char == Char2).

run() -> 
    Input = read("input.txt"),
    Valid = lists:filter(fun (L) -> check(L) end, Input),
    length(Valid).