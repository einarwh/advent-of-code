module Aoc05 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)

-- MAIN

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = updateModel
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample

type Row = Highlighted (List Int) | Plain (List Int)

type alias Model = 
  { dataSource : DataSource 
  , checkSorted : Bool 
  , rules : List (Int, Int)
  , updates : List (List Int)
  , rows : List Row 
  , sumOfMiddles : Int 
  , lastCommandText : String
  , counter : Int 
  , debug : String }

parseNumbers : String -> List Int 
parseNumbers line = 
  line |> String.split " " |> List.filterMap String.toInt

parseRule : String -> Maybe (Int, Int)
parseRule s =
  case s |> String.split "|" |> List.filterMap String.toInt of 
    [ before, after ] -> Just (before, after)
    _ -> Nothing

parseRules : String -> List (Int, Int)
parseRules s =
  s |> String.split "\n" |> List.filterMap parseRule 

parseUpdate : String -> List Int
parseUpdate s =
  s |> String.split "," |> List.filterMap String.toInt 

parseUpdates : String -> List (List Int)
parseUpdates s =
  s |> String.split "\n" |> List.map parseUpdate 

initUpdates : DataSource -> (List (Int, Int), List (List Int))
initUpdates dataSource = 
  let 
    sample = """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""
    input = """76|18
58|19
58|49
44|59
44|16
44|25
59|33
59|35
59|51
59|71
79|28
79|76
79|18
79|63
79|44
99|85
99|79
99|55
99|36
99|33
99|63
86|73
86|63
86|18
86|48
86|35
86|11
86|57
98|82
98|76
98|35
98|99
98|32
98|79
98|95
98|29
46|48
46|57
46|75
46|49
46|13
46|98
46|93
46|45
46|44
56|17
56|32
56|86
56|53
56|37
56|36
56|55
56|24
56|11
56|85
85|39
85|92
85|84
85|25
85|45
85|54
85|51
85|61
85|78
85|59
85|21
93|99
93|61
93|82
93|21
93|54
93|56
93|92
93|79
93|58
93|33
93|95
93|24
32|57
32|37
32|19
32|44
32|85
32|13
32|28
32|17
32|55
32|48
32|53
32|78
32|11
35|32
35|92
35|33
35|28
35|79
35|56
35|36
35|54
35|61
35|19
35|71
35|13
35|24
35|16
45|76
45|32
45|71
45|79
45|58
45|24
45|99
45|42
45|51
45|54
45|82
45|29
45|25
45|33
45|84
37|59
37|75
37|86
37|39
37|63
37|57
37|93
37|44
37|11
37|42
37|85
37|18
37|16
37|53
37|36
37|48
18|61
18|21
18|51
18|84
18|73
18|17
18|45
18|39
18|35
18|59
18|48
18|31
18|53
18|54
18|75
18|71
18|25
13|75
13|78
13|86
13|45
13|85
13|59
13|21
13|82
13|39
13|63
13|18
13|17
13|11
13|93
13|44
13|98
13|42
13|31
53|54
53|42
53|75
53|35
53|21
53|31
53|71
53|79
53|99
53|92
53|58
53|98
53|51
53|25
53|45
53|84
53|93
53|61
53|82
63|31
63|59
63|61
63|75
63|25
63|82
63|51
63|35
63|93
63|17
63|84
63|54
63|98
63|42
63|45
63|11
63|48
63|73
63|21
63|39
42|35
42|95
42|54
42|56
42|19
42|33
42|92
42|71
42|24
42|32
42|76
42|51
42|21
42|49
42|55
42|79
42|29
42|58
42|61
42|28
42|25
29|63
29|32
29|76
29|85
29|44
29|19
29|86
29|16
29|49
29|95
29|18
29|37
29|57
29|28
29|55
29|79
29|36
29|11
29|13
29|46
29|24
29|48
19|49
19|31
19|18
19|75
19|73
19|13
19|16
19|44
19|53
19|86
19|36
19|48
19|63
19|39
19|98
19|78
19|46
19|85
19|57
19|37
19|11
19|17
19|59
51|36
51|79
51|92
51|24
51|71
51|54
51|19
51|33
51|61
51|37
51|29
51|44
51|28
51|32
51|76
51|99
51|95
51|35
51|46
51|56
51|55
51|13
51|58
51|49
75|71
75|51
75|35
75|42
75|33
75|25
75|82
75|56
75|29
75|99
75|73
75|61
75|79
75|93
75|58
75|59
75|84
75|39
75|31
75|98
75|21
75|45
75|92
75|54
54|32
54|99
54|76
54|55
54|61
54|49
54|29
54|28
54|24
54|33
54|13
54|46
54|79
54|95
54|86
54|37
54|36
54|19
54|56
54|71
54|92
54|58
54|44
54|16
78|99
78|92
78|75
78|71
78|29
78|59
78|93
78|25
78|33
78|45
78|51
78|21
78|61
78|39
78|42
78|58
78|82
78|54
78|73
78|98
78|31
78|53
78|84
78|35
84|56
84|79
84|95
84|32
84|29
84|71
84|36
84|19
84|33
84|24
84|92
84|99
84|54
84|51
84|55
84|37
84|28
84|58
84|13
84|61
84|46
84|35
84|76
84|49
71|76
71|33
71|57
71|36
71|16
71|13
71|99
71|85
71|86
71|19
71|29
71|95
71|58
71|24
71|37
71|32
71|63
71|46
71|28
71|79
71|49
71|44
71|56
71|55
95|63
95|78
95|85
95|57
95|19
95|46
95|44
95|55
95|17
95|48
95|39
95|18
95|11
95|75
95|76
95|86
95|37
95|36
95|53
95|49
95|73
95|28
95|13
95|16
28|48
28|13
28|17
28|36
28|86
28|11
28|55
28|63
28|49
28|37
28|31
28|16
28|44
28|85
28|76
28|53
28|39
28|57
28|19
28|78
28|46
28|73
28|75
28|18
92|56
92|76
92|95
92|33
92|24
92|36
92|63
92|79
92|71
92|32
92|16
92|57
92|37
92|99
92|29
92|46
92|19
92|44
92|58
92|86
92|49
92|55
92|28
92|13
11|84
11|25
11|18
11|73
11|92
11|98
11|61
11|82
11|59
11|35
11|93
11|54
11|45
11|39
11|48
11|75
11|71
11|31
11|17
11|51
11|21
11|78
11|42
11|53
55|49
55|11
55|16
55|76
55|46
55|85
55|37
55|44
55|73
55|36
55|75
55|57
55|63
55|31
55|17
55|86
55|48
55|18
55|39
55|13
55|53
55|78
55|19
55|59
31|51
31|59
31|82
31|95
31|42
31|71
31|25
31|58
31|24
31|45
31|21
31|61
31|84
31|99
31|92
31|56
31|33
31|54
31|79
31|35
31|29
31|93
31|98
31|32
73|54
73|42
73|71
73|84
73|33
73|93
73|24
73|92
73|79
73|99
73|25
73|31
73|82
73|29
73|59
73|21
73|35
73|32
73|56
73|58
73|98
73|45
73|51
73|61
48|53
48|75
48|58
48|78
48|25
48|98
48|45
48|31
48|73
48|59
48|51
48|93
48|17
48|92
48|84
48|54
48|99
48|21
48|61
48|42
48|71
48|39
48|82
48|35
24|85
24|18
24|28
24|13
24|19
24|76
24|36
24|86
24|55
24|46
24|63
24|39
24|95
24|48
24|57
24|53
24|49
24|78
24|37
24|75
24|17
24|11
24|16
24|44
25|76
25|71
25|21
25|92
25|37
25|99
25|33
25|32
25|35
25|49
25|84
25|61
25|54
25|51
25|29
25|79
25|46
25|95
25|24
25|55
25|58
25|19
25|28
25|56
21|37
21|51
21|92
21|33
21|28
21|36
21|95
21|76
21|35
21|58
21|84
21|46
21|61
21|24
21|71
21|49
21|32
21|55
21|79
21|19
21|29
21|54
21|56
21|99
16|84
16|11
16|57
16|42
16|48
16|86
16|53
16|78
16|98
16|39
16|85
16|59
16|82
16|73
16|63
16|75
16|93
16|25
16|18
16|21
16|17
16|45
16|31
16|51
39|61
39|32
39|25
39|98
39|56
39|45
39|92
39|82
39|93
39|99
39|59
39|79
39|51
39|35
39|42
39|29
39|71
39|73
39|33
39|58
39|21
39|54
39|31
39|84
33|79
33|19
33|11
33|56
33|46
33|49
33|28
33|18
33|85
33|55
33|63
33|13
33|44
33|36
33|86
33|48
33|24
33|16
33|57
33|29
33|76
33|95
33|37
33|32
17|84
17|75
17|42
17|99
17|71
17|53
17|33
17|78
17|98
17|31
17|73
17|25
17|39
17|59
17|93
17|82
17|61
17|21
17|35
17|54
17|92
17|51
17|45
17|58
57|53
57|54
57|18
57|42
57|25
57|73
57|59
57|98
57|51
57|48
57|85
57|21
57|39
57|31
57|17
57|45
57|82
57|63
57|84
57|35
57|78
57|11
57|93
57|75
82|56
82|35
82|24
82|76
82|46
82|19
82|79
82|25
82|99
82|84
82|51
82|32
82|28
82|95
82|71
82|21
82|92
82|58
82|61
82|33
82|54
82|42
82|29
82|55
61|79
61|28
61|13
61|19
61|92
61|24
61|36
61|58
61|71
61|29
61|99
61|55
61|33
61|86
61|46
61|49
61|56
61|32
61|57
61|44
61|16
61|37
61|95
61|76
49|53
49|16
49|17
49|98
49|36
49|86
49|44
49|31
49|13
49|73
49|93
49|63
49|85
49|11
49|18
49|48
49|82
49|57
49|37
49|39
49|78
49|75
49|59
49|45
36|98
36|31
36|53
36|63
36|45
36|75
36|59
36|44
36|85
36|11
36|13
36|39
36|42
36|86
36|93
36|73
36|18
36|78
36|57
36|48
36|82
36|17
36|25
36|16
76|93
76|39
76|75
76|73
76|44
76|16
76|63
76|86
76|59
76|46
76|57
76|37
76|31
76|11
76|36
76|48
76|85
76|17
76|78
76|49
76|13
76|53
76|19
58|99
58|29
58|76
58|36
58|56
58|33
58|79
58|32
58|63
58|37
58|86
58|28
58|11
58|13
58|24
58|44
58|57
58|95
58|46
58|85
58|55
58|16
44|18
44|31
44|21
44|73
44|42
44|98
44|53
44|48
44|57
44|86
44|11
44|45
44|93
44|17
44|82
44|78
44|63
44|75
44|84
44|39
44|85
59|24
59|82
59|95
59|45
59|56
59|61
59|32
59|29
59|42
59|98
59|79
59|21
59|99
59|93
59|84
59|58
59|92
59|28
59|25
59|54
79|57
79|46
79|24
79|86
79|13
79|11
79|37
79|56
79|36
79|17
79|78
79|48
79|55
79|16
79|49
79|19
79|95
79|32
79|85
99|76
99|57
99|46
99|16
99|29
99|13
99|95
99|28
99|32
99|56
99|86
99|19
99|11
99|49
99|18
99|24
99|37
99|44
86|59
86|39
86|17
86|75
86|98
86|85
86|93
86|51
86|21
86|25
86|53
86|78
86|84
86|45
86|82
86|31
86|42
98|28
98|25
98|84
98|51
98|42
98|71
98|61
98|21
98|55
98|45
98|24
98|92
98|54
98|33
98|58
98|56
46|16
46|37
46|53
46|59
46|39
46|36
46|63
46|78
46|31
46|86
46|18
46|11
46|85
46|73
46|17
56|48
56|78
56|13
56|44
56|49
56|57
56|18
56|46
56|19
56|63
56|76
56|28
56|16
56|95
85|73
85|98
85|17
85|53
85|31
85|75
85|93
85|18
85|48
85|11
85|42
85|82
85|35
93|51
93|35
93|28
93|29
93|42
93|45
93|55
93|71
93|32
93|98
93|84
93|25
32|18
32|36
32|46
32|16
32|95
32|75
32|76
32|49
32|63
32|86
32|24
35|46
35|37
35|29
35|49
35|58
35|55
35|99
35|76
35|95
35|44
45|19
45|95
45|61
45|56
45|28
45|21
45|35
45|92
45|55
37|31
37|17
37|73
37|98
37|45
37|13
37|82
37|78
18|78
18|98
18|42
18|82
18|93
18|92
18|58
13|25
13|16
13|57
13|48
13|73
13|53
53|59
53|73
53|29
53|39
53|33
63|85
63|53
63|18
63|78
42|84
42|99
42|46
29|56
29|17
19|93

21,84,35,92,58,33,29,79,56,24,95,28,19,46,37
46,17,63,48,59,76,75,78,16,73,49,53,57,18,36
56,55,76,19,37,36,16,86,57,85,18,48,78
25,73,98,54,21,59,39,45,75,71,99,92,82,31,35,33,42,51,61,29,53,58,93
82,53,42,58,59,33,39,21,25,51,71,78,45
73,31,98,78,84,75,53,17,93,21,39,92,48,82,58
71,16,46,32,54,29,44
44,63,17,31,82,86,93,39,37
25,28,21,54,92,99,24,46,71,42,56,32,84,51,55
49,37,36,13,44,16,86,57,63,85,11,18,48,17,78,53,75,39,31,59,93,98,45
11,18,48,17,78,53,75,39,73,31,59,93,45,82,42,25,21,84,51,35,54,61,92
17,39,85,75,16,86,48,59,98,49,53,13,57,11,45,63,37
36,49,16,73,78,76,57,75,55,44,19
39,73,31,59,98,45,82,42,21,84,51,35,61,71,33,29,79
18,93,39,54,45,31,17,92,51,82,11
11,29,55,57,76,48,18
84,42,61,98,28,82,71,35,21,32,79,93,56,54,99,92,25,24,58,29,51
98,82,61,31,53,73,92,21,33,93,58,99,25,54,75,29,71,35,59,42,84
54,61,71,58,33,29,32,24,55
75,82,84,71,33
37,95,36,28,24,11,18,85,57,19,13,63,76,86,16,46,75
32,56,61,31,51,59,82,33,79,58,21,29,71,54,45,98,93,84,92,35,24
42,21,84,58,99,29,79,32,24,76,46
45,75,31,85,18,11,98,25,59,93,82,13,73,63,57,16,39
37,55,78,13,76,86,46,85,11,24,63,19,36,44,75,16,53
61,86,29,16,24
17,39,73,31,59,45,82,42,54,92,99
17,78,53,75,73,31,59,93,98,45,82,42,25,84,51,35,54,61,92,71,99
37,18,95,44,57
63,18,31,59,39,25,11,48,57,44,53,98,42
84,35,21,54,71,42,61,78,48,82,98,93,31,58,51
76,78,13,37,32,85,16,24,49,28,11,57,46,63,95,19,18,44,17
32,24,95,55,76,19,46,49,37,36,13,44,16,86,57,63,85,11,18,48,17,78,53
13,85,19,17,36,16,63,39,78,86,57,48,37,18,75,46,95
24,32,25,46,61,49,99,84,56,54,19,28,55
57,63,85,11,18,48,17,78,53,39,73,31,59,93,98,45,82,42,25,21,84,51,35
63,37,28,57,46,86,13,55,29,71,56,33,44,95,32,16,99,19,36
92,61,79,25,99,33,24,28,21,82,76,35,19,71,42
95,56,25,71,21,51,99,92,29,35,98,58,24,55,61
98,53,46,13,63,11,37,93,18
53,57,76,78,18,39,37,19,55,13,17,86,95
39,73,31,59,45,82,42,21,51,54,61,92,99,29,56
48,75,39,73,59,93,25,51,54,61,58
51,53,21,39,84,57,31,35,25,59,93,85,78
56,76,55,54,28,49,36,37,71,24,99,32,33,95,58,46,13,35,44
95,28,55,76,46,49,13,16,57,63,85,11,18,48,17,78,53,75,39
16,56,63,36,49,11,37,86,57,19,28,95,55,18,48
73,31,59,93,98,45,25,21,84,35,54,61,92,71,58,99,33,29,79,56,32
44,73,18,55,63,16,48,49,36,78,76,31,13
75,73,31,59,93,45,82,42,25,21,84,51,35,54,92,71,58,99,33,29,79
33,29,35,32,99,56,49,84,36,46,51,95,58,76,19,54,71,61,55
11,31,85,78,73,59,42,84,35,53,21
78,76,86,57,95,85,55,63,24,46,53,19,44,17,37,49,28,36,16,13,11
29,33,45,76,61,82,24
25,18,86,21,82,75,31,98,44,63,73,85,39,17,78,53,59,42,93,11,45,48,16
79,75,59,21,71,93,29,33,82,54,92,35,99,51,31,73,98,39,45,25,42,84,58
63,99,85,44,36,95,58,49,57
99,29,79,56,32,24,28,55,76,46,49,37,36,44,86,57,63,85,11
13,44,16,86,57,63,85,11,18,48,17,78,53,75,39,73,31,59,93,98,45,82,42
32,95,58,33,79,21,35,93,42,71,28,61,84,29,24,25,98,99,56,54,92
44,57,98,75,18,17,31,16,93,13,73,63,37,49,46
85,17,78,53,75,73,59,93,98,45,82,25,21,35,54
92,71,51,56,84,42,79,46,55,19,21
54,29,36,61,56,79,33,44,71
86,11,48,17,53,75,31,93,25,21,84
95,28,76,49,44,86,57,85,18
11,18,48,17,78,53,75,39,73,59,93,98,45,82,42,25,21,84,51,35,54,61,92
19,29,55,28,58,33,44,37,79,16,95,49,85,13,36
76,19,49,37,36,57,85,11,18,17,78,53,75,39,73
76,37,57,85,17,39,59
76,19,36,13,16,86,57,63,85,11,18,48,17,73,59
16,86,57,63,85,11,48,17,78,53,39,73,59,98,45,82,42,21,84
75,73,98,82,42,21,51,35,61,71,99,29,79
85,86,13,11,44,28,99,49,57,56,76,37,55,95,63
61,84,21,29,33,39,92,71,51,73,98,31,35,54,99,25,58,59,42,75,93,82,53
19,46,49,37,36,13,16,86,57,63,85,11,78,53,75,39,73,31,59
82,25,21,84,51,35,61,92,71,99,29,32,24,95,19
61,92,71,58,99,33,29,79,56,32,24,95,28,55,19,46,49,13,44,16,86
49,79,76,36,33,32,13,57,55,46,44,85,29,28,86,24,16
98,45,82,42,21,84,51,35,54,61,92,71,58,99,33,29,32,24,95,28,55
24,95,28,55,76,46,49,36,13,44,16,63,18,48,17,78,75
79,24,76,19,37,36,57,48,17
11,18,48,17,78,53,75,39,73,31,59,93,98,45,82,42,21,84,51,35,54,61,92
36,13,44,16,86,57,63,85,11,48,17,78,53,75,39,73,31,59,93,98,45,82,42
48,25,78,98,86
85,18,53,73,31,82,84
42,82,18,73,93,45,36
37,36,13,44,16,86,57,63,85,18,17,78,53,75,39,73,31,59,93,98,82
78,53,39,59,82,42,25,21,84,51,61,92,71,58,99
45,82,42,25,21,84,51,35,54,92,71,58,99,33,29,79,56,32,24,95,28,55,76
73,21,17,45,42,93,44,39,98,82,48,11,75,85,16,63,18,57,78,53,25
84,92,71,99,32
59,98,45,82,25,21,84,35,54,92,71,58,99,33,79,56,32,24,95
32,19,54,44,36,95,28,99,61,16,46,37,49,79,71
55,71,57,92,33,95,79
61,42,85,25,39,53,73
57,33,16,24,11,29,56,63,32,95,46,37,44,99,49
84,51,92,55,32,82,25,28,21,24,54,33,29,56,95,58,42,35,79,19,71
53,16,75,63,37,18,17,13,31,57,49
85,36,17,49,37,28,73,39,11,78,18,75,19,55,48,63,76
53,86,63,55,37,28,18,76,13,78,46,95,44,32,85
57,63,18,48,17,75,39,73,31,59,98,45,82,42,25,21,84,51,35
33,21,29,73,92,99,51,75,54,53,39
63,48,78,98,86,73,31,57,11,82,42,21,53,84,93,85,16
63,11,18,48,17,78,53,75,39,31,93,98,45,82,25,84,51,35,54
61,71,99,33,29,95,86
31,82,35,56,24,21,71,54,29
31,98,21,84,92,29,32
46,49,37,13,44,16,86,57,85,11,48,17,78,53,31,93,98
45,82,84,55,33,42,35,29,95,51,56,28,76,79,54,21,32
99,33,29,79,32,95,76,19,46,37,13,86,63,85,11
21,79,46,35,71,58,55,42,32,33,92,29,24,61,54,84,95,19,56
46,49,37,36,13,44,16,86,57,63,85,11,18,48,78,39,73,31,59,93,98
35,54,61,92,71,58,99,33,29,79,56,32,24,95,28,76,19,46,49,37,36,13,44
32,24,95,55,19,49,37,13,44,16,86,57,63,18,48
36,39,86,17,57,73,18,85,75,46,49,11,31,59,98,78,48
13,63,11,18,78,53,39,73,93,98,45,82,25
71,56,95,13,61,24,46,35,58,44,19,29,76
56,32,24,95,28,55,76,19,46,49,37,36,13,44,16,57,63,85,11,18,48,17,78
32,19,18,17,86,46,79,37,28
71,58,99,33,29,95,28,55,46,36,13,16,63
29,95,76,32,25,21,71
24,79,71,46,84,55,95,28,99
46,37,36,86,44,29,33,79,28,32,18,85,49,55,57
21,84,51,35,54,61,92,33,29,79,32,24,95,76,19,46,37
16,61,19,56,33,54,92
29,33,21,54,42,76,71,79,45,32,28,51,55,92,84
33,37,28,29,46,63,13,49,86,79,56,24,55,44,18,57,32,16,76,85,19,95,36
35,71,32,61,99,29,58
28,49,92,13,95,61,51,46,19,36,33,32,58,76,55,24,35,56,79
82,93,86,44,75,39,17,73,53,21,78,25,48
93,61,79,21,84,71,56,25,35,42,54,92,29,45,95,33,24,59,32,51,58
45,86,75,25,93,44,98,63,31,85,82
51,35,54,71,99,33,95,28,76,19,49
24,86,16,19,85,11,95,46,49,79,18,36,37,17,56,63,13,76,28,55,44
49,37,86,76,33,95,79,44,13,99,46,28,85,58,16,32,24
37,13,16,17,86,63,46,18,79,56,32
36,99,19,44,76,29,24,57,92,58,55,46,86
76,49,37,13,44,16,86,57,63,85,11,18,48,17,78,75,73,31,59
53,75,39,31,93,51,35,61,71,58,99,33,29
56,24,19,37,86,57,63,11,18
17,44,86,55,36
58,99,33,29,79,56,32,24,95,28,55,76,19,46,37,36,57,63,85
53,11,95,17,28,32,85,36,37,18,24,13,19,57,49,63,46,78,86,55,76
28,76,19,46,37,36,13,44,16,86,63,85,11,18,48,78,53,39,73
95,19,32,79,99,35,71,61,49,29,37,58,36,28,56,24,55,51,54
33,82,61,29,56,42,54,84,79,35,58
28,13,55,24,57,63,44,29,48
28,55,76,19,46,49,37,36,44,86,57,63,85,11,48,17,78,53,73
46,49,37,36,13,16,57,63,85,11,18,48,78,53,73,31,59,93,98
31,16,18,55,85,37,46,73,63
45,84,16,57,78,11,48,73,98,63,17,93,85
56,32,95,19,49,37,13,86,57,63,11,48,78
59,93,98,45,82,42,25,84,61,92,99,29,95
49,18,33,57,13,46,16,85,55,11,36,76,28,24,95,29,44,32,86
18,48,17,53,39,73,31,98,45,82,25,51,71
28,55,76,19,46,37,36,44,16,57,63,85,11,18,48,17,75,39,73
31,82,42,25,21,84,54,99,33,56,24
36,13,63,85,18,39,31,82,42
95,49,85,19,46,55,57,36,18,79,17,56,44,86,32
98,28,32,99,71,79,24,35,61,95,55,21,84
75,92,98,17,51,59,45,21,93,54,78,18,31,71,53,25,39,42,35,61,82,73,84
44,17,46,85,63,39,53,37,18,78,95,48,75,76,55,16,49
24,19,49,36,44,16,86,57,11,48,78,53,75
95,71,19,84,99,46,29,21,54,37,79,76,61,35,51
36,11,78,48,73,53,93,59,17,75,16,49,63,19,37,57,39
18,25,11,42,31,57,98,21,82,63,35,53,85,39,93,48,51,45,84,73,17,78,75
45,21,35,98,99,95,58,92,42,61,56,71,24,82,59,93,32
39,73,93,45,82,42,25,84,51,35,61,79,56
85,86,19,36,55,78,39,53,31,46,18
28,54,61,58,16,37,13,44,29,49,46
11,18,17,78,39,73,31,59,98,45,25,84,35,54,92
93,98,45,21,54,61,92,71,58,99,32,24,95
98,21,55,82,29,56,42,33,51,84,32,54,61,79,28,24,95,58,35,25,71,45,92
36,13,57,63,85,11,18,48,17,78,53,75,39,73,31,59,93,98,45,82,42
54,28,21,24,42,19,82,25,33,32,95,71,35
55,32,58,49,51,29,56,54,76,21,33,19,35,99,28,95,46
51,35,54,61,92,71,58,99,33,79,56,24,95,28,55,19,46,49,37,36,13
39,85,73,45,51,21,86
61,79,92,54,76,36,46,44,49,13,99,95,58,71,55,29,32
45,82,42,61,32,95,76
48,53,75,39,31,93,45,42,25,21,84,51,54,61,58
78,31,25,21,84,92,33
61,56,98,33,29,59,32,54,93,51,35,25,73,79,99
63,19,86,46,33,18,11
58,42,56,84,61,46,35
49,95,76,37,78,46,86,85,19,48,56,57,11,36,32
24,21,51,29,45,79,71,28,32,56,35,95,98,33,42,84,58
16,86,57,63,11,18,48,78,39,59,98,82,42,21,84
35,54,61,71,33,29,32,24,28,19,46,49,37,36,13
85,11,18,75,39,59,42,84,51,35,61
78,33,98,75,42,21,58,45,73,93,92,31,35,99,61
39,78,98,42,35,63,93,84,17
71,76,92,55,79,84,24,51,58,49,35,99,36,28,37
19,46,37,36,13,44,16,86,57,63,85,18,48,17,53,75,39,31,93
28,54,21,35,25,76,55,95,49,79,92,32,33,99,71,29,51
99,33,56,32,95,28,55,19,37,85,11
95,46,16,85,37,28,11,13,32,63,24,17,36,19,78,56,86,44,18
85,18,48,78,75,73,59,98,82,42,25,21,84,35,61
29,79,56,32,24,95,28,55,76,19,49,37,36,13,16,86,57,63,85,18,48
36,49,16,99,54,95,32
93,58,51,56,31,92,35,61,25,82,42,45,32
37,16,86,85,11,78,53,39,73,31,59,98,82""" 
    data = 
      case dataSource of 
        Input -> input 
        Sample -> sample
  in 
    case String.split "\n\n" data of 
      a :: b :: _ -> (parseRules a, parseUpdates b)
      _ -> ([], [])

init : () -> (Model, Cmd Msg)
init _ =
  let 
    dataSource = Input
    (rules, updates) = initUpdates dataSource
    rows = updates |> List.map Plain
    model = { sumOfMiddles = 0
            , rules = rules
            , updates = updates
            , rows = rows
            , lastCommandText = "press play to start"
            , dataSource = dataSource
            , checkSorted = True
            , counter = 0
            , debug = "" }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Clear | Sorted | Unsorted | UseSample | UseInput 

updateClear : Model -> Model
updateClear model =
  let 
    (rules, updates) = initUpdates model.dataSource
    rows = updates |> List.map Plain
  in 
    { model | rules = rules, updates = updates, rows = rows, sumOfMiddles = 0 } 

checkPage : List (Int, Int) -> List Int -> Int -> Bool 
checkPage rules pagesAfter page = 
  pagesAfter |> List.all (\p -> rules |> List.any (\(before, after) -> page == before && p == after))

isSorted : List (Int, Int) -> List Int -> Bool
isSorted rules update = 
  case update of 
    [] -> True 
    page :: pagesAfter -> 
      checkPage rules pagesAfter page && isSorted rules pagesAfter

findMiddle update = 
  let 
    ix = (List.length update) // 2
  in 
    case update |> List.drop ix of 
      [] -> 0 
      h :: _ -> h

updateSolveSorted : Model -> Model
updateSolveSorted model = 
  let 
    rows = model.updates |> List.map (\u -> if isSorted model.rules u then Highlighted u else Plain u)
    pick row = 
      case row of 
        Highlighted update -> Just update 
        Plain _ -> Nothing
    sorted = rows |> List.filterMap pick
    sumOfMiddles = sorted |> List.map findMiddle |> List.sum 
  in 
    { model | rows = rows, sumOfMiddles = sumOfMiddles }

comparePages : List (Int, Int) -> Int -> Int -> Order
comparePages rules page1 page2 =
  let
    foundRule = rules |> List.any (\(before, after) -> page1 == before && page2 == after)
  in 
    if foundRule then LT else GT

sortUpdate : List (Int, Int) -> List Int -> List Int 
sortUpdate rules update = 
  update |> List.sortWith (comparePages rules)

updateSolveUnsorted : Model -> Model
updateSolveUnsorted model =
  let 
    tag u = 
      if isSorted model.rules u then Plain u 
      else Highlighted (u |> List.sortWith (comparePages model.rules))
    rows = model.updates |> List.map tag
    pick row = 
      case row of 
        Highlighted update -> Just update 
        Plain _ -> Nothing
    highlighted = rows |> List.filterMap pick
    sumOfMiddles = highlighted |> List.map findMiddle |> List.sum 
  in 
    { model | rows = rows, sumOfMiddles = sumOfMiddles }

updateSolve : Model -> Model
updateSolve model = 
  if model.checkSorted then 
    updateSolveSorted model 
  else 
    updateSolveUnsorted model 

updateDataSource : DataSource -> Model -> Model
updateDataSource dataSource model = 
  let
    (rules, updates) = initUpdates dataSource
    rows = updates |> List.map Plain
  in
    { model | dataSource = dataSource, rules = rules, updates = updates, rows = rows, sumOfMiddles = 0 } 

updateToggleSorted : Model -> Model
updateToggleSorted model = 
  let
    (rules, updates) = initUpdates model.dataSource
    rows = updates |> List.map Plain
  in
    { model | rules = rules, updates = updates, rows = rows, sumOfMiddles = 0 } 

updateModel : Msg -> Model -> (Model, Cmd Msg)
updateModel msg model =
  case msg of
    Clear -> 
      (updateClear model, Cmd.none)
    Sorted -> 
      (updateSolveSorted model, Cmd.none)
    Unsorted -> 
      (updateSolveUnsorted model, Cmd.none)
    UseSample -> 
      (updateDataSource Sample model, Cmd.none)
    UseInput -> 
      (updateDataSource Input model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- VIEW

toPlainHtmlElement : List Int -> List (Html Msg) 
toPlainHtmlElement pages =
  [ pages |> List.map String.fromInt |> String.join " " |> Html.text, Html.br [] [] ]

toHighlightedHtmlElement : List Int -> List (Html Msg) 
toHighlightedHtmlElement numbers =
  let 
    str = numbers |> List.map String.fromInt |> String.join " "
    textElement = Html.text str 
    spanElement = Html.span [ Html.Attributes.style "background-color" "#AFE1AF" ] [ textElement ]
  in 
    [ spanElement, Html.br [] [] ]

toRowHtmlElement : Row -> List (Html Msg)  
toRowHtmlElement row = 
  case row of 
    Highlighted pages -> toHighlightedHtmlElement pages 
    Plain pages -> toPlainHtmlElement pages 

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2024 | Day 5: Print Queue"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    commandsStr = ""
    textFontSize = 
      case model.dataSource of 
        Input -> "14px"
        Sample -> "36px"
    elements = model.rows |> List.concatMap toRowHtmlElement
  in 
    Html.table 
      [ Html.Attributes.style "width" "1080px" 
      , Html.Attributes.style "font-family" "Courier New" ]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "10px"]
              [ Html.div [] [Html.text "Advent of Code 2024" ]
              , Html.div [] [Html.text "Day 5: Print Queue" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.text " ["
              , Html.a [ Html.Attributes.href "../../2024/"] [ Html.text "2024" ]
              , Html.text "] " 
              , Html.text " ["
              , Html.a [ Html.Attributes.href "../../2023/"] [ Html.text "2023" ]
              , Html.text "] "
              , Html.text " ["
              , Html.a [ Html.Attributes.href "../../2022/"] [ Html.text "2022" ]
              , Html.text "] "
              , Html.text " ["
              , Html.a [ Html.Attributes.href "../../2021/"] [ Html.text "2021" ]
              , Html.text "] "
              , Html.text " ["
              , Html.a [ Html.Attributes.href "../../2020/"] [ Html.text "2020" ]
              , Html.text "] "
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2024/day/5" ] 
                [ Html.text "https://adventofcode.com/2024/day/5" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "16px" ]
              [ 
                Html.input 
                [ Html.Attributes.type_ "radio", onClick UseInput, Html.Attributes.checked (model.dataSource == Input) ] 
                []
              , Html.label [] [ Html.text "Input" ]
              , Html.input 
                [ Html.Attributes.type_ "radio", onClick UseSample, Html.Attributes.checked (model.dataSource == Sample) ] 
                []
              , Html.label [] [ Html.text "Sample" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Sorted ] 
                [ Html.text "Sorted" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Unsorted ] 
                [ Html.text "Unsorted" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Clear ] 
                [ Html.text "Clear" ] 
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] [ Html.text (String.fromInt model.sumOfMiddles) ]
              , Html.div [] [ Html.text commandsStr ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" textFontSize
              , Html.Attributes.style "padding" "20px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] elements
              ] ] ]
