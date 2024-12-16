module Aoc02 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Html exposing (text)

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type Report = Unchecked (List Int) | Safe (List Int) | Dampened (List Int, Int) | Unsafe (List Int)

type alias Model = 
  { safeReports : Int 
  , useSample : Bool 
  , useDampener : Bool 
  , reports : List Report
  , lastCommandText : String
  , counter : Int 
  , debug : String }

parseNumbers : String -> List Int 
parseNumbers line = 
  line |> String.split " " |> List.filterMap String.toInt

-- let isSafe report = 
--     let diffs = report |> List.pairwise |> List.map (fun (a, b) -> a - b)
--     let safeIncreasing = diffs |> List.forall (fun d -> d >= 1 && d <= 3)
--     let safeDecreasing = diffs |> List.forall (fun d -> d >= -3 && d <= -1)
--     safeIncreasing || safeDecreasing

pairwise : List a -> List (a, a) 
pairwise lst = 
  case lst of 
    x :: y :: rest -> 
      (x, y) :: pairwise (y :: rest)
    _ -> []

isSafe : List Int -> Bool 
isSafe numbers = 
  let 
    diffs = numbers |> pairwise |> List.map (\(a, b) -> a - b)
    safeIncreasing = diffs |> List.all (\d -> d >= 1 && d <= 3)
    safeDecreasing = diffs |> List.all (\d -> d >= -3 && d <= -1)
  in 
    safeIncreasing || safeDecreasing

checkReport : Report -> Report 
checkReport report = 
  case report of 
    Unchecked numbers -> 
      if isSafe numbers then Safe numbers else Unsafe numbers
    _ -> report 

-- let permute report = 
--     let len = report |> List.length 
--     let indexedReport = report |> List.indexed
--     let keepDifferent i (ix, n) = if i = ix then None else Some n
--     [0 .. len - 1] |> List.map (fun i -> indexedReport |> List.choose (keepDifferent i))

permute : List Int -> List (List Int)
permute numbers = 
  let 
    len = numbers |> List.length 
    indexes = List.range 0 (len - 1)
    indexedNumbers = numbers |> List.indexedMap (\ix n -> (ix, n))
    keepDifferent i (ix, n) = if i == ix then Nothing else Just n 
  in 
    indexes |> List.map (\i -> indexedNumbers |> List.filterMap (\(ix, n) -> if i == ix then Nothing else Just n)) 

tryFindSafePermutation : Int -> List (List Int) -> Maybe Int 
tryFindSafePermutation ix permutations = 
  case permutations of 
    [] -> Nothing 
    p :: rest -> 
      if isSafe p then 
        Just ix 
      else 
        tryFindSafePermutation (ix + 1) rest

checkReportWithDampener : Report -> Report 
checkReportWithDampener report = 
  case report of 
    Unchecked numbers -> 
      if isSafe numbers then 
        Safe numbers 
      else 
        -- Create permutations.
        let
          permutations = permute numbers 
        in 
          case tryFindSafePermutation 0 permutations of 
            Nothing -> Unsafe numbers 
            Just ix -> Dampened (numbers, ix) 
    _ -> report 

initReports : Bool -> List Report
initReports useSample = 
  let 
    sample = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""
    input = """8 11 14 16 15
23 25 27 30 30
19 20 21 22 23 25 26 30
16 19 22 25 26 28 35
59 61 59 61 63
23 25 22 23 22
5 8 9 12 11 14 15 15
87 90 91 92 89 91 95
36 38 40 43 40 41 47
37 40 40 42 45 48 49 52
40 41 41 43 45 43
32 34 36 36 38 40 40
2 4 6 6 7 11
1 2 5 5 7 12
76 79 81 85 88
34 37 38 40 42 46 43
33 34 35 39 39
66 67 70 74 77 81
77 80 84 85 86 87 89 96
29 31 34 40 42 45 47 48
30 32 34 36 39 46 47 44
37 39 42 49 50 50
76 78 80 83 85 86 93 97
56 57 58 65 68 75
43 40 41 44 45 46 48 51
35 33 34 37 38 36
68 67 70 73 76 76
87 86 87 89 92 96
19 18 21 24 26 27 34
80 78 79 81 78 81
47 44 42 44 45 44
27 26 28 31 28 28
89 88 91 88 90 92 93 97
64 63 66 63 66 68 74
40 39 40 43 43 46 49
43 41 41 44 42
56 55 58 61 61 62 63 63
48 45 45 48 50 53 57
35 33 36 39 39 45
67 65 68 72 75 76 78 79
32 29 30 34 37 40 43 40
66 64 68 71 72 72
78 75 78 79 83 84 87 91
11 8 12 13 20
73 70 72 75 77 78 83 86
59 56 61 63 61
60 58 59 60 61 66 66
76 75 80 82 86
64 63 70 72 75 80
2 2 4 5 7 9
64 64 66 69 72 75 73
45 45 47 48 49 51 51
64 64 65 67 69 71 75
77 77 79 81 84 91
77 77 76 77 78 80
43 43 45 46 43 44 47 46
57 57 59 57 57
9 9 12 9 11 14 18
69 69 70 71 72 71 74 81
50 50 53 54 54 57
31 31 34 34 35 37 38 35
10 10 12 15 16 19 19 19
64 64 65 65 66 70
49 49 51 51 58
20 20 22 26 28
75 75 79 80 81 84 83
63 63 67 70 72 74 76 76
86 86 87 90 94 98
33 33 35 39 42 44 49
70 70 73 79 80
65 65 66 73 75 78 79 78
85 85 90 91 94 94
31 31 38 39 42 45 49
22 22 25 32 34 35 40
8 12 14 15 17 18 20 21
33 37 38 39 38
34 38 40 42 43 44 44
49 53 54 57 61
30 34 36 38 41 47
79 83 84 81 83
90 94 93 95 93
88 92 94 95 94 94
18 22 25 23 25 28 32
71 75 74 75 82
80 84 87 87 88 91 92
45 49 51 51 52 50
40 44 45 48 48 49 49
21 25 27 29 29 33
25 29 30 31 31 33 38
49 53 57 58 61
70 74 75 79 82 80
64 68 72 73 74 76 76
50 54 55 56 58 61 65 69
59 63 64 66 68 72 73 80
70 74 77 78 79 84 87
12 16 23 25 26 24
8 12 13 16 22 23 24 24
11 15 18 19 21 23 29 33
69 73 76 77 82 85 92
10 16 17 20 23
11 18 19 22 25 26 25
45 51 54 55 55
19 25 27 30 31 32 33 37
20 27 30 31 33 35 38 43
87 93 96 94 96
79 84 87 88 89 87 86
38 44 43 45 47 49 52 52
25 30 31 29 33
18 25 27 28 26 31
25 30 30 33 34
8 15 17 20 20 17
24 29 31 31 31
20 27 30 30 34
55 62 63 66 66 72
2 8 11 14 18 21 23 24
33 39 43 46 48 50 51 48
22 29 30 34 34
79 84 88 90 93 95 99
61 66 70 72 74 81
34 40 41 44 45 46 52 54
68 73 74 79 77
21 28 35 37 40 40
47 53 59 60 64
57 62 63 66 71 78
14 12 9 6 4 3 5
46 44 43 42 40 37 37
76 75 72 71 68 64
54 53 52 51 45
69 68 71 69 68
64 63 64 63 60 58 56 58
77 76 75 76 76
17 16 17 16 13 11 7
38 37 34 35 28
60 58 58 55 52
70 67 64 61 61 59 62
95 93 91 90 89 89 89
72 71 69 66 66 62
81 80 80 77 74 73 72 67
16 13 12 8 7 6 5
25 24 21 19 17 15 11 14
19 18 14 12 12
66 65 64 63 59 55
34 31 27 24 21 18 11
99 96 94 87 84
46 43 41 34 33 35
14 11 9 2 2
57 55 48 45 43 41 38 34
69 66 64 61 55 53 46
69 70 69 66 63 60 58
61 62 60 59 58 55 54 57
69 70 67 64 63 61 61
29 31 30 27 26 25 22 18
63 65 62 59 57 54 47
31 33 32 33 31 29
94 95 92 89 91 94
45 46 45 42 40 41 41
70 71 68 66 63 65 61
94 95 94 96 93 92 90 84
10 12 12 9 7 4 2
33 35 35 32 29 30
74 77 74 74 74
32 34 34 32 28
72 73 73 72 69 64
59 60 58 57 53 50 47
79 82 79 75 74 73 75
38 41 39 38 37 36 32 32
74 75 73 69 65
43 44 40 39 33
52 55 54 52 45 42 40 37
28 29 24 21 20 21
50 52 51 50 47 46 40 40
15 17 10 9 7 3
21 23 16 15 9
39 39 36 35 32
95 95 93 90 87 90
49 49 46 43 42 42
29 29 28 26 22
31 31 30 27 25 22 15
15 15 13 12 14 11 10
77 77 76 79 77 75 78
42 42 41 42 39 38 35 35
7 7 8 6 2
96 96 94 91 92 90 84
73 73 73 72 71
57 57 56 56 53 51 49 52
87 87 84 82 81 81 79 79
38 38 35 33 31 28 28 24
62 62 62 60 55
35 35 32 31 27 26
78 78 75 72 71 67 66 69
47 47 44 40 40
72 72 71 69 67 63 61 57
48 48 46 42 35
65 65 60 58 56 55 54
92 92 89 87 86 81 83
66 66 64 61 58 52 52
29 29 26 25 23 22 17 13
47 47 42 40 33
50 46 43 41 39 38 35 34
39 35 34 32 30 27 28
21 17 16 15 12 10 9 9
71 67 66 65 61
38 34 31 29 28 22
87 83 80 81 78
94 90 92 90 91
83 79 78 75 78 75 75
36 32 35 33 30 29 27 23
87 83 84 82 76
77 73 70 69 69 68 66 63
94 90 89 87 87 85 86
63 59 58 57 54 51 51 51
32 28 28 27 23
36 32 31 29 29 28 23
42 38 34 31 29 28
33 29 27 25 21 18 20
29 25 24 21 17 16 13 13
90 86 85 82 78 74
68 64 62 58 56 51
73 69 66 61 58 56
93 89 87 85 82 75 72 74
80 76 73 72 69 68 62 62
40 36 34 32 30 25 24 20
55 51 50 48 46 40 39 33
25 20 19 18 17 14
98 91 89 87 86 84 82 83
58 52 51 49 49
75 70 69 67 65 61
91 84 83 82 80 78 76 69
37 30 32 30 28 26 23 22
48 42 40 42 41 39 40
75 69 68 67 66 64 67 67
90 85 86 85 83 82 79 75
34 29 32 30 24
93 87 86 84 83 83 82
94 87 84 83 83 86
19 14 14 13 12 12
94 89 87 87 84 80
60 53 50 47 44 44 37
41 34 31 27 24 21
31 25 23 19 20
79 74 72 68 67 66 63 63
28 23 21 20 19 17 13 9
97 91 87 85 82 80 73
96 91 88 86 81 78 75 72
58 51 48 41 38 40
35 28 25 22 16 13 13
45 40 35 34 30
38 33 30 23 21 14
18 20 22 25 27 28 25
78 80 82 83 86 89 90 90
1 3 6 9 13
21 24 25 28 30 31 33 39
90 91 93 92 94
47 49 52 50 51 54 56 54
25 28 30 33 34 33 33
20 21 24 25 26 27 25 29
77 78 77 79 84
64 67 68 71 72 72 75
13 15 18 21 21 19
33 35 37 37 38 38
15 17 20 22 23 26 26 30
56 57 58 58 64
20 21 24 28 29 31
41 43 47 50 49
83 86 90 93 93
8 11 15 18 20 24
83 84 88 90 92 99
54 55 58 59 65 68
60 62 63 69 70 71 70
57 58 65 67 70 70
54 55 62 64 67 70 74
17 18 19 21 24 30 32 38
30 29 31 34 35 37 39
27 24 25 26 27 30 29
9 6 8 10 11 14 17 17
32 31 32 35 37 41
58 55 57 59 60 65
25 22 25 26 25 27 30 32
73 72 74 76 79 81 79 77
5 2 5 8 10 12 10 10
87 85 87 90 91 93 90 94
38 37 38 40 41 42 39 45
37 35 37 40 40 41
14 11 11 14 16 18 16
71 68 71 71 71
29 26 26 29 30 33 37
69 68 68 71 74 76 79 85
43 40 44 46 47 48 50
46 45 47 48 50 54 57 55
23 22 26 29 29
36 34 36 40 42 43 47
13 10 12 16 18 19 25
64 63 66 67 68 75 76 78
72 70 75 77 79 80 77
12 10 11 16 19 19
64 62 64 69 70 73 75 79
50 48 51 54 61 64 66 71
12 12 14 16 18
40 40 42 45 47 50 49
62 62 65 67 67
70 70 72 74 78
10 10 11 12 13 14 15 20
77 77 74 75 77
40 40 38 39 38
86 86 87 85 85
48 48 49 46 50
7 7 10 7 8 13
19 19 20 20 21 24
20 20 23 26 27 27 26
40 40 42 45 46 46 46
88 88 90 90 91 95
65 65 65 67 69 76
81 81 83 87 88 90
48 48 49 51 55 57 54
35 35 39 42 44 45 48 48
70 70 73 75 78 79 83 87
5 5 9 12 17
10 10 11 12 15 21 22
30 30 37 38 39 42 41
77 77 82 84 87 90 93 93
11 11 13 20 24
59 59 61 66 71
12 16 17 20 22 24 25
52 56 57 59 61 62 64 61
79 83 86 88 91 91
16 20 21 23 27
72 76 79 82 84 89
85 89 86 87 89
92 96 98 96 94
32 36 37 34 35 37 38 38
56 60 61 58 61 62 65 69
78 82 80 81 87
47 51 53 54 57 59 59 61
53 57 57 59 60 62 60
73 77 77 80 81 81
83 87 88 89 90 90 94
78 82 85 86 86 93
37 41 42 43 46 48 52 53
71 75 76 77 81 83 84 82
78 82 86 88 91 91
20 24 27 29 33 37
54 58 59 61 64 65 69 76
52 56 59 60 62 69 71
8 12 14 19 18
52 56 59 62 67 67
69 73 74 79 83
48 52 54 56 57 59 64 69
84 91 92 93 95
43 48 50 52 54 51
76 81 83 85 88 90 90
18 25 28 30 31 34 38
49 55 58 61 63 68
26 33 30 32 34 36 39
10 17 14 17 15
80 87 88 85 85
81 88 91 89 90 93 97
24 29 32 30 35
41 47 50 51 54 54 57 58
60 65 66 68 71 73 73 70
33 40 40 41 43 45 47 47
45 50 52 55 55 59
1 7 10 11 12 12 19
30 36 39 43 44
51 58 61 65 63
24 30 31 32 36 38 38
46 53 57 59 63
34 39 40 41 45 46 49 56
33 38 39 40 41 46 49
63 69 74 75 74
74 81 88 91 93 95 95
26 32 38 41 45
67 72 79 81 83 86 93
30 28 27 25 23 21 18 21
43 40 38 36 33 30 30
55 52 51 49 48 47 46 42
21 20 17 16 10
68 67 69 66 65
38 35 33 31 33 31 29 32
85 82 80 79 80 79 76 76
64 63 66 64 61 57
42 39 37 34 36 33 32 26
42 40 37 37 34 31 30
17 15 15 13 11 10 9 12
75 73 72 72 72
50 49 49 47 43
53 50 47 45 45 38
61 60 59 58 54 51
37 36 32 29 30
15 12 10 6 6
44 43 42 38 34
34 32 28 27 21
24 22 19 18 15 9 8
60 58 51 50 48 47 46 48
91 90 88 85 78 76 76
88 86 81 79 77 73
89 88 86 84 78 72
17 18 15 13 12 10
77 80 78 77 75 78
44 47 45 44 41 39 39
14 17 16 13 10 8 4
90 91 89 87 84 83 76
51 52 49 52 49 46 45
30 32 35 32 29 28 29
15 16 15 16 16
96 98 99 98 97 93
83 84 82 81 79 78 81 76
30 33 30 29 28 28 27
41 44 42 42 39 36 33 36
79 82 81 81 81
48 50 47 47 43
31 33 33 31 28 27 26 19
49 50 47 46 42 40 39
19 21 19 18 15 11 9 10
52 53 51 49 48 44 42 42
72 75 71 69 65
25 27 25 23 22 18 15 9
35 36 33 30 25 22 19
76 78 75 73 68 65 68
96 99 93 91 89 87 84 84
80 83 82 79 76 69 65
91 93 90 89 84 77
78 78 77 74 71
42 42 41 39 36 35 36
81 81 80 77 75 74 71 71
47 47 46 44 42 38
71 71 68 66 64 61 55
29 29 26 29 28 25
76 76 77 74 71 74
79 79 81 78 77 77
79 79 76 79 76 75 74 70
73 73 72 70 69 68 71 66
59 59 58 55 53 52 52 50
58 58 56 56 53 51 52
80 80 79 77 76 76 73 73
51 51 50 48 48 44
41 41 41 38 37 36 35 29
54 54 53 52 48 46 43 40
25 25 23 19 20
10 10 9 5 5
63 63 59 57 55 52 48
62 62 60 56 53 51 48 42
40 40 39 37 35 30 29
38 38 35 34 28 26 28
90 90 85 84 84
84 84 81 75 72 68
76 76 73 68 67 64 59
37 33 31 29 26 25 23
88 84 83 80 77 80
97 93 91 89 88 88
82 78 77 74 73 70 67 63
92 88 86 85 84 79
20 16 14 16 15
26 22 24 23 22 21 19 21
57 53 55 54 53 51 51
24 20 19 21 18 16 12
56 52 50 49 48 49 46 40
71 67 67 65 64 62 60
40 36 36 33 31 29 31
87 83 80 80 80
25 21 19 19 17 14 10
88 84 82 79 77 77 74 67
23 19 15 14 11 10
29 25 22 18 16 13 15
43 39 36 32 32
90 86 85 82 80 76 74 70
58 54 50 47 40
54 50 44 41 38 36
98 94 88 85 88
22 18 12 9 8 8
49 45 38 36 33 29
56 52 49 47 46 40 39 32
53 48 47 45 42 41 38
60 54 53 51 50 49 50
90 83 82 81 81
87 81 80 78 74
25 19 18 16 13 6
70 64 63 66 64 61
36 29 26 29 30
30 25 28 25 23 21 18 18
81 76 78 76 74 73 69
73 68 66 64 62 64 63 57
51 46 46 44 41
18 12 10 9 7 6 6 7
38 31 30 30 28 26 24 24
52 47 44 41 39 39 35
74 68 65 64 61 58 58 53
29 23 19 18 16 14 11
56 50 49 48 44 41 40 41
36 31 30 28 25 21 19 19
53 47 46 42 38
72 67 63 60 59 56 54 47
35 30 28 26 20 18
34 27 25 23 18 16 18
46 39 32 29 26 24 24
77 71 66 64 61 58 57 53
77 71 70 63 60 54
34 34 36 37 40 44 47 51
55 59 61 62 60
95 95 92 91 88 91
92 95 92 89 89 88 89
81 83 85 87 90 93 99
30 24 22 15 12
76 80 81 82 85 87 92 92
82 83 81 80 80
24 27 30 31 31 33 35 33
32 26 28 27 28
33 26 25 23 21 20 17
74 70 67 61 60 58 58
95 95 93 95 98
50 50 51 49 47 45 47
85 82 80 78 76 69
34 27 27 24 21 20 18 13
60 56 56 55 54 53 51 44
47 47 53 54 53
72 72 70 70 70
9 7 10 11 14 16 15 14
37 41 42 44 45 46 49 51
42 36 36 33 33
61 66 70 71 71
77 72 70 66 60
48 44 41 38 39 37 37
46 43 45 43 41 41
63 67 70 72 74 75 78 82
2 3 6 6 9 11 11
84 82 80 77 75 71 70 66
94 90 89 92 89 82
17 17 15 12 11 9 7 5
72 75 73 73 73
48 47 45 42 40 39 38 37
3 4 6 7 9
33 35 36 37 40 43 44 45
49 51 52 53 54
26 28 31 33 34 36
55 52 50 47 44
84 81 79 77 75 72 69 68
90 91 93 94 95 98
47 45 44 42 39
22 23 26 28 29 30 32 35
69 71 74 77 78 80 83
53 56 57 58 61 64 66
20 21 22 24 25 27 28 29
37 34 33 32 31 28
42 44 47 50 51
94 91 89 86 83 81
75 72 69 66 63 62
50 47 45 42 39 37
31 33 34 36 39 40
30 33 35 36 38 40
30 27 24 22 19 16 15
20 21 22 25 28 31 33
68 70 72 73 76 77 79
13 14 16 19 22
47 44 41 39 37 34 33 32
73 75 77 79 81 84
21 24 26 29 31
79 81 84 85 88
56 53 52 51 48 46
71 74 75 76 77
66 68 69 71 73 75
37 35 33 30 28 26 23 20
47 49 52 55 56 57
95 93 91 89 86 85 82
19 21 24 26 28 30
71 74 76 79 80 82 84 85
57 55 54 52 50
75 77 80 81 83 86
57 55 54 51 49 46
99 98 97 94 93 91
29 26 25 23 21 18
61 60 57 54 51 49 47
34 33 31 29 27 26
79 77 74 73 72
83 86 89 91 92 94
41 39 36 34 33
41 38 36 33 30 29 27 25
16 18 21 22 25 26
86 85 84 83 81 78 76 73
28 29 31 34 36 37
34 31 30 28 26 25
82 79 77 74 73 72 71
90 91 92 94 95 98
47 46 43 41 38 36 34
62 59 56 54 52
45 42 40 38 36 34 32
26 25 22 19 16 15 14
37 38 40 43 45 46
84 81 79 76 75 74 71
77 80 82 85 87 90 92 95
35 38 39 40 42 45 46 49
30 33 36 39 41 44
48 51 54 56 58 59 62
28 29 30 32 34 35 36 39
37 39 40 42 44 46
21 23 24 26 27 28 31 33
79 78 76 73 70 68 67 66
38 37 34 32 30 28 27
77 75 73 72 69 68 66
13 14 15 18 20 21
83 81 80 79 77 75
33 36 38 40 43 44 46 49
63 61 60 59 56 53 50 47
23 24 25 26 27 29 30
97 94 93 92 89
61 62 63 64 66 69
32 33 34 37 40
72 71 68 67 64 62 59
32 34 36 38 40 41 44 46
55 52 51 50 48 47 46 45
36 37 38 39 42
56 57 59 60 61 63 66
87 85 82 80 77 76
8 9 12 14 16
81 82 83 86 88 90 91 94
57 58 59 62 65
49 48 46 45 44 42 41
75 74 73 71 69
46 48 49 52 54
90 89 86 83 82 79 78
30 32 33 35 37 39 42
49 48 46 45 44 42
13 16 17 20 22 25 26 29
67 64 62 61 59
32 29 28 25 24 21 19
57 58 59 60 63 64
9 11 13 14 17 19 21 22
43 44 45 48 51 53
14 16 19 22 25 26 27 28
77 75 73 71 68 66 64 61
26 24 22 21 18 17 16
80 79 78 77 74 72 71
24 25 28 31 34 35 38
50 52 55 58 59 62
52 53 56 58 61 64 65 68
21 19 16 13 11 10 7 6
3 5 8 9 10 11
38 37 36 33 30 27 24
99 97 96 94 91 88 86
54 51 48 46 45 43 42
91 90 89 86 85 83 82 79
34 37 39 40 43 46 47
15 16 17 19 21 22 24
42 41 38 35 33 31 29 27
87 84 81 80 78 76
80 82 83 86 89
82 79 76 75 72 70 68 66
45 47 50 51 52 55 56
36 39 41 43 45 47 50
57 55 54 53 52 51
76 74 73 72 70 68 66
67 66 65 62 60
55 54 53 50 48 45
80 81 84 85 86 88 91
38 40 43 45 47 49 50 53
82 84 87 88 90
69 72 74 77 78 79 82
51 53 55 58 60
19 16 14 11 8 5 3
42 41 40 38 36
21 19 16 13 11 10
29 28 27 25 23
9 10 12 14 17
88 89 92 95 96
65 66 68 70 73 74 77 79
18 15 14 11 10
45 48 51 53 56
84 82 81 79 78 77
56 58 59 60 62 63 64 66
67 64 63 60 58 56 54 53
92 90 89 86 83 82 80 79
54 56 57 60 62 64 67 69
97 96 95 93 91 89
65 64 62 59 57 56 55 53
47 49 52 54 57 59 62
78 81 84 86 88 91 92 93
24 26 27 28 30 32
49 52 54 55 58 61
84 83 82 81 80
29 30 31 34 36 39 40
86 85 82 79 78
31 32 34 35 36
77 76 73 72 69 67 65
60 59 58 55 53 51 48 46
65 68 70 71 72
56 54 52 51 48 45
38 36 33 31 28 25 23
38 40 42 43 44
88 86 85 83 82 81 79 78
13 11 10 9 8
81 79 78 77 76
81 82 85 86 89
66 63 60 58 55
3 4 6 9 12 15 17 19
31 34 35 36 39
26 27 29 32 35 36 39 42
26 25 22 21 20 19 16
84 82 80 78 75
70 71 72 75 76 78 80 83
63 60 58 55 52
65 63 61 58 56 54
60 61 62 63 65
44 43 42 40 39 38
2 5 6 7 9 10 13
4 6 8 9 11 12 14
61 58 57 56 55
71 72 73 75 78
60 63 65 67 70 71 73
20 17 14 13 10
64 66 69 72 74 75 76 78
41 44 47 48 49 50 51 54
48 47 44 41 39 36
30 32 33 35 37 40 43
10 12 15 17 20 21 22 25
5 8 11 13 16 19 22 25
36 37 40 41 42
59 60 62 63 66
62 59 58 55 53 50 49
28 27 26 23 21
26 29 31 32 34 36
59 62 65 67 68
77 80 81 83 84 86 88 90
35 34 31 28 26
40 43 44 47 50 52
90 87 86 84 83 81 80
45 44 42 39 36
4 6 8 9 12
27 24 22 20 19 16 14 11
70 71 72 75 77 80
54 56 58 59 61 63 66
27 25 24 21 18 16
73 75 78 80 83 85 87
57 58 61 63 65 66 67
74 72 71 69 66 65 62
98 97 95 92 90
14 11 10 9 8 5
9 6 5 2 1
29 32 33 36 38 39
93 90 88 86 83 80 77
38 41 43 44 45 47 50 53
25 24 21 20 17
18 17 14 11 8 6 4 1
3 5 8 10 12
87 88 90 93 96
85 86 87 90 91 92 95
29 28 25 24 22 19
89 88 86 83 82 80 79 76
70 69 67 65 64 61
37 40 42 43 45 46 49 50
1 2 4 5 8 10
85 82 80 79 76 73 70 69
95 92 91 88 85 84 81 80
35 32 29 26 25 22
26 24 23 22 21 19 18 17
92 93 94 95 97
37 35 32 30 27
61 59 56 55 52 50 49 46
32 35 36 38 41 43
74 77 78 81 82
88 86 83 82 80
52 55 56 57 60
61 64 65 66 68 70 72 73
35 34 33 30 27 26
21 24 25 26 28 31 32 35
12 14 15 17 18 20 22 24
28 31 34 35 37
94 92 89 88 87 84 82
43 42 39 38 36 34 31 29
37 38 39 42 45 47 50 53
82 85 87 90 93 96 99
74 77 78 79 81 83
86 83 80 77 76 73
98 97 96 94 92 90
25 23 21 19 18 15
68 65 63 61 59
34 37 38 41 44 45
70 67 65 63 61 58 56 53
34 32 30 27 25
55 53 50 47 44 43
44 47 50 51 53
90 87 84 81 80 77 74
18 19 22 24 27
18 19 21 24 25 28 31 34
43 42 40 38 37 35 34
57 56 54 53 52 49 48 46
16 18 21 22 25
20 21 23 24 27
79 80 83 86 87 90
83 81 80 79 77 75 72
37 40 41 44 45 47 49
28 27 25 22 21
20 18 15 14 13 10 9
51 52 54 57 60 61 63 66
71 73 74 77 80 81 83 85
14 15 17 18 19 21
73 75 76 77 79 82
36 33 32 31 30 27 24 23
69 71 73 75 76 77 80 81
75 78 80 83 86 87
11 14 17 20 23
92 89 88 86 84
68 67 64 62 60 59 57 56
21 19 18 15 12 11 10 9
42 40 39 38 37
71 74 75 76 78 80 82
7 8 9 12 15 18
21 18 17 15 13 11
70 68 65 62 61 58 57
32 31 29 27 24 23 22
4 7 9 11 14
35 33 31 30 29 28 27 24
30 33 36 39 42 45 47
12 10 8 7 5
47 48 50 53 56 57 59
67 69 70 72 75 77 78
27 24 22 19 16 15 13 11
15 13 11 10 9
43 40 38 35 34 31
45 48 50 52 53
76 77 79 81 82 85
90 88 87 84 82 79 77 76
28 29 32 35 38 41 43 46
61 64 67 70 73 75 78
55 52 51 48 46 43 42
9 12 15 18 21 24 27
85 87 89 91 92 94 95 97
75 74 73 70 69 66
35 36 39 40 43
52 55 58 60 63
59 62 65 66 68 70 73
17 15 13 11 8 7 4
37 40 42 44 46
21 19 18 17 15
27 28 29 32 33 36
9 12 13 14 17 20 21 24
88 91 94 96 99
69 71 72 73 74 76 79
60 59 58 56 53 50
86 84 82 79 78 76 74 71
98 95 94 91 89
70 68 66 65 64 61
16 15 12 11 8 6 5
16 19 20 23 24 27 30 33
47 49 50 51 53 55 58
79 82 83 84 86 87 89
93 91 88 87 86 84
17 14 13 12 11 9 7
83 81 79 78 76 75
78 79 82 85 87 88
38 40 43 44 45 48 49
44 43 40 37 36 34
94 92 91 89 88
7 9 11 12 14
94 91 90 89 86
11 12 15 17 18 19 20
32 30 27 24 21 18
48 47 45 43 41
51 54 55 57 59 61 64
28 29 31 34 37 38 39 42
85 84 83 82 81 78 75
70 67 64 62 59
52 50 47 44 42
72 74 75 77 78 81 84
7 8 11 14 15 17 19
38 39 42 43 44
66 68 70 73 74 75 78 79
64 65 67 70 73
58 56 55 53 50 49
77 78 80 81 84
32 34 35 37 39
7 9 11 12 13 14
38 41 43 46 48 49 52
24 27 29 32 33 35 38 41
77 74 73 70 69 68 66
14 17 19 22 25 28 31 34
41 40 38 36 34 31 30
30 28 26 23 22 21 20
50 47 46 44 43 42
56 53 50 47 45 44 41
70 73 76 78 80 82
41 42 45 48 50 51 53 54
45 42 41 39 37 34 31 29
58 57 54 52 50
17 20 21 24 27
54 56 58 59 62
73 76 78 80 81
17 19 20 22 25 26 29
47 48 50 52 55 58
23 20 17 15 13 10 9
72 71 70 68 65 62 60 59
85 83 81 80 79
80 79 77 76 73 70 68
96 94 92 91 89
36 38 40 41 42 44
21 24 26 29 31 33 35
62 64 66 69 71
90 89 87 84 82 81 80 77
19 20 21 23 25 27
23 20 17 14 13 12 11 9
67 69 70 72 73 75 78 80
3 6 7 10 12
60 63 66 67 70 73 76 78
87 85 82 79 76
6 9 12 14 17 18 19
62 60 57 56 53 51
54 52 50 49 48
64 63 62 61 60 57 55
75 76 78 81 82 84
65 64 62 60 58 57 55
62 63 66 68 71 73
41 43 44 46 49 51 52
15 13 10 8 6
25 23 21 19 17 14 11 8
56 58 60 61 62 64 66 69
78 80 83 85 86 87 89
35 37 40 42 44 47 48
72 75 78 80 81 82 83
30 32 34 35 37
62 60 57 54 52 49 48 46
96 93 90 87 85 84 83
57 60 63 66 67 70
77 75 72 70 67
88 87 84 81 79
22 25 26 29 31 32 34
24 23 22 19 16 15
57 56 53 50 49
7 10 13 15 17 18 19 21
19 22 24 27 29
30 28 26 23 22 20 18 15
5 6 8 9 12
59 58 55 53 52 50 47 46
35 34 32 30 27 24 22
67 64 63 61 58 56 54
71 74 77 78 79
26 29 32 35 36 39
90 88 87 86 84
23 21 20 18 16 15 14
11 14 17 19 20 21 23
75 78 79 80 83
45 48 51 54 57 58 59 62
34 31 29 26 23 20 18 17
17 15 12 9 7 5
38 37 36 35 34 31 30 27
56 55 54 53 52 51 50 49
78 80 82 84 85
46 45 43 42 40 38 35
25 27 30 31 32 33 35
27 30 32 34 37 38
91 90 88 85 82 81 80
27 25 23 22 20 17 16
88 87 86 83 80
96 93 92 89 87 84 81
42 43 45 47 48 49 50
21 20 19 18 15 12 9
57 58 59 62 64 66 69
52 49 46 44 42
35 34 31 28 26 23 20 17
79 76 75 73 72 71 70 68
24 25 26 29 31 32 34
75 77 80 82 84 85 87 90
2 5 7 8 11 12 14
27 26 23 20 17 14 12
55 58 59 62 64 67 68 69
33 34 37 40 41 42
68 69 70 71 72
18 17 15 13 11 8 5 4
63 61 60 58 56 54 53 52
22 20 19 16 15 14
56 57 58 59 60 62
84 85 88 90 93 96
26 27 29 32 34 35
88 85 84 81 78 75 73 70
52 50 49 46 44 43 40
41 40 37 34 32 29 26 23
81 79 76 74 72
51 53 55 56 58 61 62
58 57 56 55 54
47 44 43 41 40
83 84 86 88 90 91 92 94
84 83 82 81 79 78
15 14 13 12 11 10 9
31 28 25 24 21 18
73 72 71 70 69
44 46 48 49 51 54 55 58
8 10 13 14 17 18
78 77 74 73 70
8 10 12 13 15
47 46 43 42 40
94 91 90 89 88 85 84 83
41 43 45 46 48
65 66 67 68 69 72 75
97 95 94 92 91 89
68 71 73 75 78
35 37 40 42 44
63 65 66 67 68 71 74 76
85 88 90 92 95 98
75 73 70 67 66
91 89 87 85 82 79 77 74
33 32 30 27 26 24 21 20
88 85 82 79 76 74 73 71
31 33 34 35 38 41 42 43
10 11 12 15 17 20 21""" 
    data = if useSample then sample else input 
  in 
    data |> String.split "\n" |> List.map (parseNumbers >> Unchecked)

init : () -> (Model, Cmd Msg)
init _ =
  let 
    reports = initReports False
    model = { safeReports = 0
            , reports = reports
            , lastCommandText = "press play to start"
            , useSample = False
            , useDampener = False
            , counter = 0
            , debug = "" }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Clear | Solve | ToggleDampener | ToggleSample

updateClear : Model -> Model
updateClear model = { model | reports = initReports model.useSample, safeReports = 0 } 

countAsSafe : Report -> Int 
countAsSafe report = 
  case report of 
    Safe _ -> 1 
    Dampened _ -> 1 
    Unchecked _ -> 0 
    Unsafe _ -> 0

countSafe : List Report -> Int 
countSafe reports = 
  reports |> List.map countAsSafe |> List.sum 

updateSolve : Model -> Model
updateSolve model = 
  let
    reports = 
      if model.useDampener then 
        model.reports |> List.map checkReportWithDampener
      else 
        model.reports |> List.map checkReport
    found = countSafe reports 
  in
    { model | reports = reports, safeReports = found }

updateToggleDampener : Model -> Model
updateToggleDampener model = 
  let
    useDampener = not model.useDampener
  in
    { model | useDampener = useDampener, reports = initReports model.useSample, safeReports = 0 } 

updateToggleSample : Model -> Model
updateToggleSample model = 
  let
    useSample = not model.useSample
  in
    { model | useSample = useSample, safeReports = 0, reports = initReports useSample } 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> 
      (updateClear model, Cmd.none)
    Solve -> 
      (updateSolve model, Cmd.none)
    ToggleDampener -> 
      (updateToggleDampener model, Cmd.none)
    ToggleSample -> 
      (updateToggleSample model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

toUncheckedHtmlElement : List Int -> List (Html Msg) 
toUncheckedHtmlElement numbers =
  [ numbers |> List.map String.fromInt |> String.join " " |> Html.text, Html.br [] [] ]

toUnsafeHtmlElement : List Int -> List (Html Msg) 
toUnsafeHtmlElement numbers =
  let 
    str = numbers |> List.map String.fromInt |> String.join " "
    textElement = Html.text str 
    spanElement = Html.span [ Html.Attributes.style "background-color" "#FAA0A0" ] [ textElement ]
  in 
    [ spanElement, Html.br [] [] ]

toSafeHtmlElement : List Int -> List (Html Msg) 
toSafeHtmlElement numbers =
  let 
    str = numbers |> List.map String.fromInt |> String.join " "
    textElement = Html.text str 
    spanElement = Html.span [ Html.Attributes.style "background-color" "#AFE1AF" ] [ textElement ]
  in 
    [ spanElement, Html.br [] [] ]

toDampenedHtmlElement : Int -> List Int -> List (Html Msg) 
toDampenedHtmlElement index numbers =
  let 
    before = numbers |> List.take index
    fromIndex = numbers |> List.drop index 
    dropped = fromIndex |> List.take 1 
    after = fromIndex |> List.drop 1 
    strBefore = before |> List.map String.fromInt |> String.join " " 
    textElementBefore = Html.text (String.append strBefore " ") 
    spanElementBefore = Html.span [ Html.Attributes.style "background-color" "#AFE1AF" ] [ textElementBefore ]
    strAfter = after |> List.map String.fromInt |> String.join " "
    textElementAfter = Html.text (String.append " " strAfter) 
    spanElementAfter = Html.span [ Html.Attributes.style "background-color" "#AFE1AF" ] [ textElementAfter ]
    strDropped = dropped |> List.map String.fromInt |> String.join " "
    textElementDropped = Html.text strDropped 
    spanElementDropped = 
      Html.span 
        [ Html.Attributes.style "background-color" "#AFE1AF"
        , Html.Attributes.style "color" "#808080"
        , Html.Attributes.style "text-decoration-line" "line-through"] 
        [ textElementDropped ]
    breakElement = Html.br [] []
  in 
    [ spanElementBefore, spanElementDropped, spanElementAfter, breakElement ]

toReportHtmlElement : Report -> List (Html Msg)  
toReportHtmlElement report = 
  case report of 
    Unchecked numbers -> toUncheckedHtmlElement numbers 
    Unsafe numbers -> toUnsafeHtmlElement numbers
    Safe numbers -> toSafeHtmlElement numbers
    Dampened (numbers, index) -> toDampenedHtmlElement index numbers

view : Model -> Html Msg
view model =
  let
    commandsStr = ""
    textFontSize = if model.useSample then "36px" else "14px"
    elements = model.reports |> List.concatMap toReportHtmlElement
  in 
    Html.table 
      [ 
        Html.Attributes.style "width" "1080px"
      , Html.Attributes.style "font-family" "Courier New"
      ]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "40px"
              , Html.Attributes.style "padding" "20px"]
              [ Html.div [] [Html.text "Advent of Code 2024" ]
              , Html.div [] [Html.text "Day 2: Red-Nosed Reports" ] ] ]
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
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Solve ] 
                [ Html.text "Solve" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Clear ] 
                [ Html.text "Clear" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick ToggleSample ] 
                [ Html.text (if model.useSample then "Input" else "Sample") ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleDampener, Html.Attributes.checked model.useDampener ] 
                []
              , Html.label [] [ Html.text " Use dampener" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding-top" "10px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] [ Html.text (String.fromInt model.safeReports) ]
              , Html.div [] [ Html.text commandsStr ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" textFontSize
              , Html.Attributes.style "padding" "10px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] elements
              ] ] ]
