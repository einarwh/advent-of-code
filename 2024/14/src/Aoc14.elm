module Aoc14 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Html exposing (text)
import Set exposing (Set)
import List.Extra exposing (gatherEquals)
import Time

defaultTickInterval : Float
defaultTickInterval = 100

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample | Bonus 

type alias Pos = (Int, Int)

type alias Robot = 
  { position : Pos 
  , velocity : Pos }

type alias Model = 
  { robots : List Robot 
  , width : Int 
  , height : Int 
  , findEasterEgg : Bool 
  , dataSource : DataSource
  , steps : Int 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , debug : String }

sample : String
sample = """p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"""

input : String
input = """p=52,66 v=37,34
p=0,87 v=41,-19
p=92,102 v=-26,86
p=7,12 v=-16,-17
p=85,34 v=81,38
p=80,28 v=88,-61
p=25,20 v=27,92
p=28,45 v=-80,66
p=72,86 v=47,-85
p=36,2 v=28,30
p=36,29 v=28,69
p=84,102 v=-26,86
p=52,73 v=68,-91
p=26,38 v=-68,-69
p=54,100 v=65,-70
p=99,77 v=-1,-42
p=50,59 v=-94,87
p=65,34 v=47,-96
p=87,100 v=-78,86
p=25,31 v=-62,7
p=78,8 v=67,-69
p=19,59 v=-28,9
p=85,61 v=-17,84
p=20,28 v=-25,13
p=25,21 v=-68,-50
p=34,30 v=9,32
p=91,15 v=-4,45
p=2,58 v=-60,56
p=52,89 v=-21,-79
p=59,11 v=-88,23
p=64,45 v=-91,72
p=42,102 v=-15,-92
p=64,56 v=99,-36
p=69,36 v=10,-15
p=22,74 v=-74,-13
p=74,32 v=-11,-15
p=49,37 v=-9,-48
p=31,55 v=70,-78
p=15,75 v=-65,-38
p=69,38 v=54,-69
p=32,36 v=93,68
p=36,96 v=67,23
p=57,96 v=-82,-67
p=6,88 v=48,30
p=13,67 v=76,87
p=32,34 v=-67,-87
p=87,41 v=-57,11
p=1,14 v=54,-99
p=88,51 v=34,50
p=73,80 v=38,56
p=19,95 v=88,27
p=80,71 v=-66,-82
p=79,30 v=84,54
p=24,22 v=91,-55
p=56,27 v=-27,1
p=69,80 v=4,46
p=50,78 v=-85,84
p=7,27 v=51,29
p=71,78 v=47,96
p=86,90 v=-23,58
p=31,37 v=6,19
p=47,31 v=-95,16
p=33,91 v=-89,80
p=36,64 v=-3,6
p=80,50 v=-38,25
p=54,84 v=28,21
p=77,26 v=58,81
p=43,43 v=68,56
p=60,95 v=-48,71
p=95,25 v=5,-57
p=80,95 v=-45,78
p=66,29 v=-20,82
p=99,76 v=-7,-91
p=81,31 v=-69,-88
p=76,57 v=-75,94
p=82,17 v=-66,-5
p=86,13 v=-13,21
p=12,94 v=-89,51
p=21,30 v=61,72
p=7,98 v=-35,-79
p=64,92 v=-41,27
p=16,54 v=-27,-53
p=69,56 v=-35,-67
p=51,84 v=-33,99
p=87,98 v=78,64
p=10,25 v=33,-75
p=57,12 v=13,42
p=9,98 v=-83,-23
p=97,66 v=32,-75
p=86,50 v=-29,22
p=43,0 v=18,21
p=90,23 v=29,-49
p=18,63 v=77,-51
p=61,58 v=-60,-44
p=69,94 v=-94,33
p=67,100 v=77,-8
p=12,50 v=30,47
p=57,96 v=-24,2
p=60,95 v=25,-51
p=9,4 v=3,-46
p=4,22 v=94,60
p=79,16 v=38,-93
p=62,57 v=78,-72
p=78,0 v=70,92
p=7,91 v=-93,-25
p=83,48 v=-57,34
p=73,35 v=-51,-90
p=5,32 v=94,-9
p=89,36 v=-23,44
p=36,33 v=-27,60
p=90,98 v=-66,2
p=81,9 v=93,58
p=27,101 v=92,36
p=72,39 v=96,-15
p=48,75 v=-77,-52
p=64,1 v=10,45
p=31,75 v=-31,-99
p=54,50 v=-92,-40
p=82,78 v=23,46
p=92,48 v=34,-65
p=91,54 v=20,-84
p=90,67 v=69,-88
p=72,35 v=-60,-40
p=18,76 v=55,-44
p=25,101 v=-92,-73
p=64,15 v=7,-77
p=42,42 v=-67,66
p=47,38 v=-95,44
p=54,46 v=-45,-19
p=25,0 v=37,-27
p=0,55 v=23,-62
p=41,66 v=43,-94
p=58,89 v=-61,11
p=41,90 v=-58,52
p=29,83 v=-37,2
p=50,45 v=16,6
p=59,6 v=65,-83
p=78,84 v=37,73
p=2,24 v=-58,-46
p=100,30 v=8,-96
p=13,55 v=64,65
p=84,83 v=78,-1
p=16,46 v=79,38
p=94,34 v=-1,75
p=36,82 v=-58,-85
p=4,52 v=25,-64
p=31,101 v=49,5
p=89,6 v=26,36
p=46,97 v=58,-64
p=65,5 v=74,55
p=24,37 v=-25,41
p=40,89 v=67,24
p=84,5 v=29,-48
p=26,94 v=81,98
p=51,8 v=-21,-84
p=10,82 v=-51,-32
p=56,89 v=50,-6
p=53,13 v=-58,68
p=47,36 v=-73,85
p=59,68 v=16,-60
p=1,83 v=54,-63
p=43,6 v=-67,-49
p=2,68 v=51,9
p=58,42 v=40,-75
p=10,71 v=-22,-26
p=17,79 v=-80,59
p=28,12 v=24,-11
p=1,100 v=26,84
p=58,99 v=16,67
p=54,8 v=-79,-92
p=69,26 v=81,-58
p=11,7 v=47,-41
p=87,15 v=-99,26
p=60,20 v=-40,-24
p=71,92 v=3,37
p=97,82 v=-41,40
p=54,12 v=-30,-27
p=32,17 v=61,82
p=54,95 v=-38,-16
p=42,26 v=-67,57
p=22,45 v=73,69
p=18,0 v=-22,-92
p=70,39 v=19,95
p=63,72 v=10,21
p=26,88 v=24,46
p=4,101 v=75,43
p=31,31 v=30,-12
p=71,10 v=43,-86
p=68,42 v=-2,94
p=90,76 v=-38,68
p=71,76 v=63,-13
p=35,76 v=-21,6
p=95,28 v=35,-77
p=22,7 v=-52,-8
p=23,56 v=-8,69
p=54,45 v=-76,-90
p=56,64 v=-91,-75
p=28,39 v=-41,77
p=42,51 v=-12,19
p=28,67 v=12,-44
p=86,52 v=75,-84
p=69,54 v=71,90
p=42,70 v=-52,-25
p=14,48 v=16,-96
p=1,65 v=-26,-72
p=80,34 v=-39,-6
p=78,15 v=41,-33
p=13,5 v=-7,64
p=28,80 v=-40,18
p=43,15 v=40,-8
p=98,68 v=-6,82
p=16,32 v=86,11
p=66,56 v=65,75
p=84,58 v=48,-33
p=65,22 v=-63,-84
p=93,52 v=-75,72
p=17,23 v=-68,-74
p=77,65 v=-8,-66
p=61,37 v=-42,69
p=86,8 v=66,-20
p=4,7 v=-44,70
p=15,82 v=9,-23
p=67,95 v=-95,57
p=5,24 v=69,4
p=58,96 v=99,46
p=38,21 v=50,56
p=60,76 v=-64,74
p=29,0 v=-55,-25
p=24,13 v=65,71
p=48,76 v=-67,37
p=19,39 v=-62,29
p=33,58 v=6,28
p=29,38 v=-98,-68
p=50,55 v=-55,65
p=5,77 v=91,71
p=99,38 v=-8,77
p=58,47 v=16,44
p=43,45 v=12,-31
p=21,102 v=45,-20
p=79,99 v=83,-46
p=52,56 v=83,16
p=56,27 v=74,-68
p=17,20 v=82,-77
p=62,8 v=13,95
p=58,24 v=-48,-2
p=86,68 v=-3,-37
p=2,24 v=63,-8
p=1,81 v=-41,-88
p=15,40 v=-25,66
p=77,97 v=-23,5
p=48,4 v=34,42
p=41,74 v=-3,-91
p=90,53 v=44,-76
p=50,42 v=-79,13
p=77,67 v=17,-53
p=69,3 v=-8,3
p=89,60 v=-8,19
p=15,93 v=54,-32
p=11,24 v=36,63
p=22,6 v=-52,79
p=11,101 v=-80,-79
p=79,87 v=-8,77
p=46,97 v=13,61
p=90,25 v=-45,7
p=46,95 v=13,-23
p=89,62 v=-72,-94
p=32,100 v=-49,58
p=58,30 v=68,-40
p=97,28 v=11,37
p=76,61 v=-88,-31
p=6,45 v=-7,38
p=79,34 v=-69,-90
p=96,76 v=57,-29
p=9,100 v=-7,55
p=59,23 v=50,43
p=19,7 v=33,-74
p=45,43 v=86,66
p=50,19 v=56,33
p=28,65 v=-9,-89
p=14,2 v=-74,-33
p=91,98 v=-50,99
p=93,80 v=94,-48
p=55,81 v=-7,-20
p=57,100 v=-30,-39
p=62,76 v=68,-38
p=15,20 v=-77,-90
p=37,33 v=89,60
p=18,101 v=-3,20
p=56,65 v=71,-60
p=26,26 v=-58,-99
p=33,75 v=-49,-91
p=10,71 v=-56,-34
p=25,83 v=39,-6
p=13,17 v=37,61
p=98,95 v=2,80
p=26,83 v=-49,-35
p=9,27 v=-72,46
p=27,73 v=-9,40
p=71,5 v=-94,-89
p=60,6 v=-21,30
p=10,68 v=-82,-31
p=87,45 v=-38,66
p=55,46 v=-68,-93
p=34,77 v=55,-63
p=5,51 v=38,31
p=3,65 v=94,-16
p=70,24 v=-48,10
p=89,78 v=-75,40
p=32,39 v=3,66
p=28,94 v=40,-36
p=91,38 v=-35,66
p=29,66 v=-94,-50
p=90,80 v=63,74
p=15,31 v=-95,78
p=50,19 v=7,-71
p=58,64 v=-5,68
p=16,64 v=65,-35
p=46,93 v=-59,-22
p=65,5 v=53,-33
p=10,83 v=84,47
p=23,12 v=49,-76
p=58,14 v=-36,-80
p=49,4 v=28,17
p=62,43 v=-39,56
p=61,11 v=-36,-52
p=60,72 v=71,87
p=44,56 v=-85,-76
p=37,88 v=-3,27
p=10,35 v=-90,-18
p=14,40 v=30,-34
p=22,95 v=27,95
p=32,92 v=-37,2
p=54,3 v=-54,-82
p=98,5 v=11,-24
p=39,76 v=37,90
p=69,15 v=47,45
p=56,40 v=-79,13
p=80,41 v=63,32
p=11,85 v=-7,74
p=24,45 v=46,-50
p=98,49 v=-38,-81
p=26,45 v=-26,-80
p=2,87 v=60,6
p=93,74 v=26,-16
p=24,70 v=-28,-85
p=59,21 v=-63,62
p=44,74 v=-7,14
p=75,94 v=47,-86
p=84,16 v=6,41
p=17,79 v=33,-44
p=5,72 v=18,58
p=96,11 v=8,17
p=34,48 v=62,14
p=17,36 v=-42,-64
p=87,55 v=93,65
p=84,1 v=75,52
p=30,74 v=-5,-33
p=34,22 v=-32,-91
p=85,94 v=-51,17
p=37,90 v=-9,55
p=9,85 v=-58,-37
p=58,30 v=-2,72
p=90,55 v=-53,-78
p=34,48 v=-8,-33
p=76,62 v=13,18
p=16,21 v=76,-99
p=42,71 v=46,68
p=95,9 v=-75,64
p=63,99 v=62,-39
p=73,97 v=68,92
p=79,17 v=-17,-39
p=16,54 v=27,90
p=48,34 v=9,-49
p=88,72 v=51,59
p=94,24 v=62,-43
p=93,37 v=22,24
p=90,80 v=-23,15
p=10,98 v=88,80
p=60,9 v=65,64
p=44,80 v=21,68
p=32,3 v=-95,83
p=83,13 v=78,1
p=20,49 v=70,-94
p=51,82 v=-30,71
p=31,12 v=61,86
p=67,51 v=4,3
p=19,25 v=67,76
p=96,6 v=-99,-11
p=35,13 v=-98,-42
p=89,13 v=-29,95
p=73,73 v=-60,87
p=50,4 v=77,98
p=5,86 v=61,49
p=14,56 v=-7,40
p=97,4 v=8,-64
p=19,67 v=-37,-47
p=61,23 v=-88,60
p=23,11 v=-59,11
p=11,16 v=33,-9
p=90,29 v=-12,75
p=81,62 v=-9,51
p=34,60 v=58,84
p=46,16 v=58,67
p=47,12 v=74,67
p=33,50 v=-31,87
p=76,32 v=-52,-66
p=56,39 v=-79,88
p=92,18 v=-75,63
p=9,17 v=36,70
p=70,42 v=-11,74
p=11,8 v=46,69
p=30,19 v=64,-5
p=78,42 v=-45,-46
p=98,64 v=-99,-66
p=83,31 v=14,-39
p=97,66 v=73,96
p=33,55 v=46,-38
p=67,59 v=59,3
p=19,34 v=82,-37
p=9,77 v=45,46
p=88,100 v=29,55
p=77,2 v=-54,-42
p=59,49 v=-79,-53
p=22,83 v=94,99
p=75,40 v=99,-15
p=44,33 v=-28,2
p=83,42 v=44,-65
p=61,13 v=-7,-8
p=39,25 v=-90,93
p=18,11 v=32,98
p=49,24 v=3,32
p=75,30 v=99,85
p=48,21 v=52,40
p=5,23 v=91,-52
p=40,41 v=92,-62
p=14,51 v=-34,-41
p=17,86 v=30,27
p=77,15 v=-11,-80
p=26,18 v=-77,73
p=26,42 v=26,-25
p=10,97 v=27,80
p=24,37 v=-40,-65
p=75,40 v=-81,-43
p=6,19 v=57,38
p=16,82 v=-74,-85
p=87,81 v=-32,15
p=36,91 v=53,-60
p=25,32 v=6,-71
p=11,69 v=-41,65
p=31,81 v=-22,33
p=7,51 v=-88,-93
p=75,18 v=84,-30
p=59,65 v=-33,-22
p=53,19 v=87,-14
p=53,98 v=-30,55
p=69,88 v=-27,-12
p=34,93 v=-37,52
p=16,17 v=11,65
p=65,53 v=-33,-28
p=81,45 v=-66,-5
p=97,92 v=18,-58
p=88,18 v=72,1
p=72,62 v=-34,64
p=79,70 v=29,26
p=64,66 v=7,75
p=48,65 v=-89,-68
p=47,18 v=-7,-64
p=50,89 v=-73,-64
p=13,91 v=33,29
p=63,6 v=-45,42
p=84,22 v=84,-71
p=68,100 v=53,5
p=89,66 v=-55,59
p=66,19 v=-17,95
p=49,16 v=61,-60
p=16,15 v=58,70
p=38,12 v=-37,47
p=98,44 v=81,-75
p=34,72 v=-3,28
p=36,54 v=16,31
p=48,63 v=55,6
p=14,68 v=33,90
p=18,67 v=-1,-53
p=10,35 v=58,59
p=13,67 v=57,-38
p=48,69 v=83,90
p=88,17 v=-75,26
p=69,72 v=-98,54
p=87,12 v=-48,16
p=99,2 v=14,64
p=71,97 v=47,39
p=69,64 v=14,-3
p=36,82 v=38,-93
p=24,77 v=21,-10
p=10,21 v=-34,-30
p=34,44 v=-98,47
p=38,9 v=-39,-74
p=76,21 v=44,60
p=66,75 v=-33,-66
p=26,31 v=9,-40"""

bonus : String 
bonus = """p=94,79 v=28,-49
p=61,1 v=59,28
p=98,17 v=44,-1
p=49,96 v=-53,-9
p=49,32 v=-61,-99
p=46,70 v=-93,51
p=2,97 v=-49,60
p=97,67 v=97,50
p=90,85 v=33,56
p=61,15 v=96,67
p=4,87 v=36,91
p=61,15 v=-21,-36
p=93,94 v=25,-44
p=83,30 v=38,-31
p=21,60 v=-62,82
p=53,8 v=-16,-4
p=60,80 v=32,20
p=79,63 v=75,83
p=21,93 v=7,-10
p=30,3 v=71,63
p=85,12 v=-2,66
p=48,40 v=98,-62
p=92,39 v=38,75
p=60,100 v=77,-42
p=60,47 v=69,9
p=48,48 v=66,-25
p=77,81 v=88,-14
p=93,93 v=-95,-10
p=20,13 v=20,-71
p=4,2 v=-15,97
p=99,54 v=-71,80
p=20,88 v=97,57
p=50,38 v=26,6
p=7,14 v=78,-2
p=2,1 v=30,28
p=16,9 v=33,65
p=53,47 v=18,-94
p=92,39 v=19,-28
p=91,68 v=3,-87
p=92,98 v=3,26
p=73,5 v=45,98
p=79,30 v=85,-31
p=86,75 v=-69,87
p=9,26 v=-87,2
p=54,54 v=-38,80
p=27,76 v=-60,-50
p=72,30 v=-11,-31
p=44,70 v=32,-18
p=89,91 v=-77,92
p=76,20 v=86,-35
p=56,58 v=-48,-91
p=45,17 v=58,67
p=31,100 v=39,26
p=31,44 v=-70,-27
p=42,65 v=10,-20
p=18,33 v=12,-65
p=10,57 v=99,-57
p=30,43 v=-80,-62
p=29,53 v=-46,-93
p=58,89 v=-24,22
p=4,70 v=-15,-53
p=93,69 v=-45,84
p=52,91 v=-5,91
p=12,26 v=44,-68
p=76,63 v=43,13
p=36,32 v=-83,-66
p=84,56 v=-10,45
p=96,31 v=-23,71
p=66,33 v=32,3
p=38,84 v=-99,20
p=52,96 v=-96,24
p=83,15 v=-58,-3
p=70,47 v=32,42
p=59,94 v=-64,92
p=33,14 v=-94,31
p=99,38 v=-98,39
p=67,80 v=-87,-16
p=3,62 v=97,81
p=89,81 v=-31,-50
p=83,76 v=14,-86
p=92,49 v=-10,-95
p=55,88 v=90,-82
p=72,69 v=16,49
p=73,34 v=16,3
p=16,101 v=46,94
p=61,68 v=67,14
p=63,35 v=-13,3
p=26,69 v=87,-20
p=19,17 v=-65,-3
p=56,94 v=21,57
p=96,24 v=30,68
p=28,46 v=-17,41
p=70,65 v=8,-90
p=82,66 v=96,82
p=72,102 v=8,-9
p=43,24 v=71,68
p=7,46 v=78,41
p=53,4 v=39,-76
p=98,101 v=60,-44
p=42,14 v=37,30
p=1,77 v=4,-52
p=99,69 v=-44,14
p=41,20 v=-11,-71
p=3,0 v=-20,94
p=29,94 v=79,-12
p=53,25 v=61,-35
p=54,3 v=-40,-8
p=59,60 v=93,-92
p=63,46 v=16,75
p=3,90 v=33,21
p=74,75 v=88,16
p=48,40 v=74,-30
p=86,43 v=-34,74
p=2,14 v=94,30
p=23,102 v=-49,25
p=85,12 v=-66,-5
p=58,66 v=13,13
p=49,49 v=-67,76
p=30,46 v=-25,75
p=38,92 v=31,-47
p=44,6 v=71,96
p=28,78 v=41,-86
p=41,69 v=-93,48
p=51,70 v=16,-89
p=47,32 v=-24,-33
p=21,75 v=-38,50
p=33,64 v=50,-91
p=79,85 v=-95,19
p=31,99 v=18,58
p=98,13 v=41,-5
p=4,13 v=89,-5
p=75,50 v=-58,-27
p=42,57 v=-27,44
p=77,82 v=43,18
p=4,80 v=57,-17
p=33,57 v=79,44
p=35,60 v=-14,-58
p=45,94 v=58,22
p=60,82 v=-32,-85
p=77,51 v=-5,-61
p=24,35 v=68,71
p=93,41 v=6,-30
p=79,92 v=88,90
p=14,59 v=-36,-24
p=82,58 v=96,10
p=79,58 v=64,10
p=32,61 v=84,-92
p=97,24 v=91,33
p=44,75 v=68,-53
p=70,11 v=-63,-6
p=42,11 v=-24,97
p=37,101 v=-72,24
p=22,33 v=-99,70
p=0,90 v=-81,89
p=53,53 v=-69,-95
p=9,36 v=76,-32
p=39,62 v=-11,-92
p=70,17 v=27,-4
p=0,91 v=65,-48
p=77,77 v=67,16
p=47,13 v=-80,-74
p=32,30 v=95,-34
p=91,89 v=-47,-83
p=15,23 v=-57,-2
p=9,48 v=-12,75
p=41,63 v=-67,80
p=24,10 v=92,28
p=76,87 v=96,-15
p=30,6 v=23,-42
p=39,100 v=-14,-45
p=17,49 v=4,41
p=74,94 v=-53,56
p=82,72 v=3,-20
p=64,86 v=-48,19
p=30,56 v=-25,-94
p=47,14 v=2,-5
p=1,96 v=-71,91
p=3,42 v=43,-30
p=59,63 v=51,11
p=27,82 v=58,86
p=30,55 v=74,77
p=63,45 v=-74,5
p=19,26 v=-30,-70
p=31,53 v=-43,42
p=3,22 v=28,-37
p=53,11 v=93,28
p=59,65 v=-69,46
p=91,7 v=-23,-42
p=91,88 v=-31,88
p=31,42 v=87,4
p=78,67 v=40,-22
p=38,48 v=15,-97
p=4,66 v=-50,-91
p=93,69 v=-81,-56
p=89,51 v=49,-62
p=16,15 v=-38,-74
p=54,51 v=56,41
p=33,54 v=-19,-61
p=21,66 v=79,-57
p=55,101 v=-61,-11
p=80,81 v=-87,-52
p=15,48 v=92,40
p=9,43 v=36,4
p=89,84 v=-71,52
p=27,56 v=31,-26
p=71,93 v=8,55
p=99,20 v=22,65
p=11,17 v=1,-39
p=10,65 v=-15,80
p=71,6 v=-19,-77
p=49,69 v=-29,81
p=56,60 v=-69,78
p=10,58 v=-41,43
p=42,9 v=-96,-42
p=94,47 v=-92,5
p=43,38 v=-27,2
p=25,59 v=-94,-94
p=28,60 v=76,78
p=9,30 v=1,68
p=23,58 v=-12,43
p=68,19 v=-64,-73
p=100,84 v=-90,86
p=71,93 v=-55,-83
p=70,88 v=-50,-16
p=25,75 v=-14,14
p=93,98 v=17,-47
p=70,66 v=-74,11
p=93,93 v=-79,-83
p=94,27 v=-79,-2
p=78,53 v=-13,41
p=89,11 v=-50,-76
p=10,52 v=86,-28
p=86,76 v=80,83
p=20,0 v=49,92
p=54,42 v=47,3
p=42,36 v=16,-68
p=71,101 v=-34,-12
p=57,52 v=48,-97
p=64,67 v=-5,-92
p=76,79 v=-18,-88
p=14,89 v=20,87
p=47,36 v=74,35
p=37,58 v=79,8
p=26,24 v=84,31
p=99,26 v=-47,-37
p=19,46 v=-97,-99
p=94,44 v=-10,72
p=78,9 v=24,26
p=86,62 v=80,78
p=56,0 v=34,23
p=18,11 v=25,-42
p=96,88 v=80,18
p=69,54 v=30,-28
p=52,42 v=-77,-32
p=43,101 v=45,22
p=84,100 v=-39,56
p=29,18 v=-83,-40
p=93,77 v=-84,14
p=26,55 v=79,41
p=80,52 v=59,40
p=6,51 v=65,74
p=23,96 v=92,89
p=100,9 v=-7,60
p=19,6 v=-57,59
p=7,37 v=-60,-68
p=54,84 v=5,85
p=71,79 v=-69,-54
p=75,8 v=56,94
p=74,33 v=-61,68
p=4,49 v=-23,39
p=76,12 v=-61,-42
p=31,77 v=-25,14
p=31,2 v=-57,92
p=29,26 v=-81,-3
p=65,49 v=98,-64
p=24,10 v=65,26
p=30,85 v=-44,51
p=10,17 v=63,-75
p=84,54 v=86,-97
p=76,22 v=14,64
p=21,31 v=71,67
p=22,62 v=71,43
p=6,86 v=-65,-52
p=33,21 v=42,98
p=72,52 v=43,-29
p=52,80 v=77,49
p=69,38 v=-98,-68
p=75,25 v=-74,-38
p=59,38 v=93,-68
p=93,14 v=-47,-76
p=58,80 v=-32,49
p=51,70 v=5,-23
p=0,83 v=-7,50
p=3,25 v=-15,-38
p=33,43 v=15,-32
p=20,32 v=4,33
p=35,6 v=15,93
p=0,37 v=-71,-34
p=75,57 v=-85,7
p=93,19 v=-74,63
p=53,12 v=-99,95
p=31,79 v=20,-20
p=69,10 v=-35,60
p=84,29 v=49,66
p=21,87 v=-6,-52
p=79,16 v=46,-7
p=60,71 v=88,80
p=24,88 v=-6,-86
p=57,62 v=-53,77
p=92,40 v=-84,1
p=80,10 v=-87,94
p=82,5 v=22,-45
p=38,54 v=-35,-63
p=24,19 v=-54,97
p=15,68 v=-33,-24
p=40,66 v=58,-59
p=35,56 v=10,-28
p=71,50 v=-13,73
p=9,6 v=89,-79
p=32,77 v=63,82
p=85,54 v=-26,-63
p=83,59 v=51,-27
p=35,19 v=63,97
p=95,15 v=30,27
p=49,101 v=58,90
p=84,35 v=27,-35
p=17,88 v=89,-86
p=40,69 v=63,-58
p=77,76 v=-53,13
p=79,29 v=-45,-37
p=90,1 v=-66,-12
p=17,42 v=49,-67
p=74,55 v=-8,6
p=44,41 v=-54,70
p=12,25 v=-15,-4
p=81,66 v=24,44
p=93,91 v=11,-85
p=100,83 v=59,84
p=0,65 v=-82,-25
p=38,92 v=-16,18
p=38,6 v=-72,-45
p=75,98 v=-87,20
p=14,99 v=-78,-14
p=70,75 v=-42,-22
p=58,84 v=56,84
p=54,71 v=16,11
p=39,30 v=90,66
p=38,9 v=74,-44
p=31,43 v=-91,36
p=75,9 v=-50,59
p=71,56 v=11,6
p=85,14 v=-87,-8
p=21,37 v=-1,34
p=34,74 v=-6,-91
p=21,1 v=84,22
p=10,64 v=89,-60
p=74,100 v=-13,55
p=52,56 v=-96,-97
p=14,35 v=-4,-1
p=51,72 v=-19,80
p=56,51 v=13,73
p=26,91 v=-33,-51
p=97,39 v=-79,69
p=75,27 v=40,65
p=35,102 v=-86,-13
p=96,69 v=91,79
p=55,24 v=58,64
p=67,90 v=45,-17
p=85,40 v=80,35
p=64,22 v=5,-74
p=55,30 v=-75,-37
p=7,46 v=38,-66
p=56,30 v=18,-37
p=49,74 v=-46,-91
p=37,41 v=52,1
p=29,80 v=-52,-89
p=90,98 v=-4,54
p=80,99 v=62,20
p=40,79 v=37,82
p=0,91 v=-89,-17
p=98,34 v=81,-36
p=63,81 v=96,-89
p=47,68 v=61,-59
p=20,52 v=-62,73
p=69,23 v=19,-74
p=19,93 v=15,-85
p=79,71 v=-18,-58
p=89,62 v=54,-61
p=43,102 v=82,-82
p=96,54 v=-7,5
p=29,96 v=-46,19
p=21,95 v=-17,-50
p=68,68 v=48,44
p=33,82 v=63,83
p=94,66 v=-63,9
p=98,87 v=62,-87
p=91,15 v=99,95
p=13,71 v=73,45
p=3,89 v=86,51
p=7,22 v=-92,63
p=96,52 v=6,73
p=27,46 v=-49,71
p=63,75 v=29,-91
p=61,77 v=-96,47
p=30,98 v=52,54
p=65,0 v=-80,90
p=74,88 v=85,85
p=71,75 v=-48,-91
p=69,94 v=-72,87
p=77,21 v=-16,97
p=49,76 v=55,-22
p=26,52 v=-36,73
p=54,3 v=47,91
p=15,78 v=2,-56
p=64,74 v=35,-23
p=29,102 v=-51,-48
p=33,48 v=-27,37
p=94,72 v=49,-58
p=60,53 v=-29,73
p=6,72 v=36,-58
p=39,68 v=-11,-25
p=42,91 v=5,-86
p=75,71 v=59,79
p=19,50 v=-94,-31
p=79,5 v=-26,57
p=57,100 v=93,-83
p=92,71 v=-39,-24
p=96,13 v=86,-9
p=51,19 v=-80,96
p=26,30 v=-86,31
p=11,77 v=-12,-22
p=46,89 v=58,-18
p=18,70 v=28,-93
p=50,28 v=-27,-4
p=74,12 v=-45,25
p=35,81 v=39,-55
p=61,54 v=-64,-64
p=1,45 v=-47,36
p=59,87 v=5,50
p=87,3 v=19,22
p=47,21 v=-6,28
p=37,63 v=7,-61
p=83,93 v=64,52
p=81,47 v=40,-32
p=27,5 v=-97,57
p=76,45 v=85,-67
p=56,69 v=18,-59
p=11,64 v=54,8
p=40,84 v=-25,49
p=7,63 v=75,-61
p=46,30 v=-53,-38
p=72,60 v=-10,75
p=50,43 v=8,-68
p=67,28 v=35,-73
p=39,34 v=-96,32
p=12,30 v=84,65
p=36,39 v=66,-35
p=68,3 v=-90,56
p=21,33 v=31,66
p=19,52 v=-94,-65
p=83,18 v=-95,-42
p=92,48 v=70,71
p=68,18 v=-29,-42
p=85,46 v=-2,-67
p=66,34 v=40,32
p=78,75 v=-74,-23
p=36,32 v=-14,-3
p=28,70 v=15,-59
p=58,30 v=-56,-38
p=77,68 v=-13,-94
p=30,93 v=7,-17
p=69,33 v=8,66
p=75,64 v=48,42
p=69,57 v=-8,74
p=51,12 v=-59,59
p=92,86 v=59,15
p=3,10 v=46,-79
p=97,78 v=83,81
p=39,40 v=15,34
p=0,92 v=99,-86
p=78,96 v=-93,87
p=76,3 v=-16,56
p=28,62 v=-4,-96
p=12,93 v=-39,-17
p=86,85 v=40,-54
p=62,19 v=10,27
p=5,71 v=23,-59
p=66,62 v=-66,41
p=19,85 v=-46,-20
p=43,102 v=-64,-83
p=16,72 v=-86,10
p=84,31 v=-55,-38
p=90,12 v=-15,-10
p=83,66 v=22,-95
p=7,76 v=12,80
p=13,28 v=-49,-39
p=40,87 v=-43,-88
p=32,64 v=-14,-27
p=21,53 v=-9,-65
p=22,9 v=92,92
p=67,83 v=40,48
p=24,72 v=-9,10
p=82,26 v=-58,29
p=97,94 v=-47,86
p=83,1 v=35,55
p=54,12 v=98,93
p=78,93 v=-21,17
p=19,14 v=-97,-78
p=86,8 v=27,-80
p=79,69 v=64,9
p=8,70 v=-7,-25
p=68,3 v=-40,90
p=43,44 v=55,-68
p=6,27 v=-47,-5
p=12,56 v=-7,39
p=30,72 v=28,-93
p=71,67 v=45,77
p=10,89 v=54,50
p=73,1 v=45,55
p=40,7 v=-65,-46
p=41,92 v=-93,-18
p=95,101 v=-52,88
p=82,8 v=-63,57
p=15,86 v=-1,-20
p=27,70 v=-14,-94
p=0,64 v=-36,-96
p=74,91 v=43,16
p=94,57 v=-7,-64
p=93,102 v=78,-49
p=41,62 v=-43,-28
p=73,15 v=-98,25
p=42,17 v=-51,60
p=0,81 v=-92,-56
p=66,50 v=24,71
p=56,91 v=37,16
p=76,55 v=-13,4
p=64,20 v=-16,-42
p=81,1 v=11,-14
p=7,2 v=-84,55
p=85,95 v=-74,-17
p=97,18 v=-87,-77
p=33,41 v=-1,-35
p=45,23 v=87,62
p=22,98 v=-4,87
p=19,75 v=65,45
p=51,33 v=-91,31
p=6,83 v=46,82
p=45,87 v=-54,-54
p=47,29 v=-46,64
p=52,20 v=-14,-42
p=74,0 v=5,-83
p=81,93 v=25,85
p=1,84 v=97,82
p=40,10 v=98,23
p=53,63 v=93,-28
p=100,98 v=57,18
p=39,79 v=-35,-57
p=47,24 v=21,-41
p=94,101 v=-15,-84
p=32,39 v=-14,-36
p=27,89 v=-62,15
p=23,87 v=-1,83
p=23,17 v=92,-9
p=10,52 v=81,37
p=73,48 v=72,70
p=79,32 v=11,99
p=33,65 v=39,-96
p=92,10 v=99,-80
p=61,78 v=45,80
p=24,42 v=-57,-35
p=48,61 v=26,-63
p=7,72 v=-7,-25
p=2,0 v=-55,-49
p=3,8 v=-55,91
p=52,25 v=26,28
p=61,45 v=-11,-34
p=18,4 v=-60,21
p=75,19 v=85,-77
p=67,64 v=-51,41
p=21,25 v=-51,62
p=29,95 v=-14,-52
p=91,69 v=70,8
p=9,97 v=-89,-17
p=49,28 v=21,63
p=95,0 v=78,-15
p=26,69 v=-78,-95
p=34,19 v=79,-43
p=14,82 v=12,-22
p=99,68 v=78,42
p=40,59 v=-99,-64
p=12,62 v=-28,40
p=2,25 v=-15,62
p=2,19 v=-23,-43
p=51,67 v=58,-27
p=49,12 v=-67,-11
p=40,58 v=-46,73
p=7,39 v=86,-2
p=44,73 v=71,78
p=25,100 v=12,-16
p=68,57 v=-56,4
p=37,12 v=-89,92
p=16,57 v=-91,38
p=90,32 v=54,-39
p=89,62 v=-63,-29
p=9,41 v=97,-36
p=15,33 v=36,-73
p=43,36 v=-51,31
p=18,91 v=-57,-88
p=56,77 v=37,-24
p=29,57 v=-86,38
p=12,1 v=-28,88
p=5,83 v=9,-22
p=91,63 v=83,-63
p=85,65 v=-74,75
p=24,18 v=-65,-78
p=62,11 v=29,-46
p=63,56 v=29,72
p=75,86 v=-85,82
p=48,85 v=-6,-90
p=25,85 v=-84,-90
p=18,85 v=26,-56
p=60,74 v=-8,-94
p=58,102 v=-32,87
p=16,73 v=28,43
p=61,98 v=77,-86
p=78,69 v=3,-27
p=57,80 v=29,11
p=28,4 v=92,20
p=76,54 v=64,71
p=91,58 v=75,38
p=84,7 v=-90,21
p=18,5 v=81,89
p=87,18 v=8,59
p=66,14 v=6,23
p=29,94 v=23,84
p=80,86 v=19,-56
p=77,95 v=88,-53
p=25,2 v=-33,19
p=76,7 v=64,-48
p=3,16 v=43,-45
p=20,34 v=42,98
p=99,43 v=-98,-2
p=75,91 v=-23,48
p=53,48 v=-5,68
p=8,86 v=31,-91
p=66,33 v=-18,-40
p=80,77 v=86,-94
p=38,28 v=-56,27
p=11,29 v=23,-7
p=41,64 v=-48,39
p=80,73 v=54,42
p=50,26 v=8,95
p=49,5 v=93,-15
p=50,37 v=93,-73
p=53,16 v=8,23
p=20,63 v=-62,-30
p=88,80 v=70,-93
p=23,13 v=-54,22
p=19,99 v=-94,-18
p=53,45 v=69,-36
p=24,39 v=-70,-38
p=10,79 v=-89,-59
p=57,78 v=-24,78
p=41,10 v=42,21
p=14,27 v=-81,-42
p=24,6 v=92,-49
p=35,32 v=-30,-6
p=69,99 v=32,85
p=5,18 v=17,-45
p=98,63 v=46,73
p=80,85 v=96,-57
p=3,52 v=-23,-68
p=48,101 v=-75,17
p=39,46 v=47,33
p=34,8 v=-1,89
p=82,67 v=-29,40
p=1,58 v=22,-66
p=79,56 v=32,2
p=10,9 v=-23,-48
p=15,72 v=-92,-27
p=89,69 v=-13,-28
p=57,48 v=26,68
p=4,11 v=99,90
p=17,74 v=-7,-95
p=53,51 v=-30,69
p=81,70 v=-16,-62
p=46,14 v=-1,91
p=44,11 v=76,90
p=89,63 v=24,73
p=48,67 v=-39,-44
p=89,78 v=-41,-22
p=68,30 v=42,77
p=70,92 v=8,62
p=67,32 v=47,-4
p=26,42 v=53,42
p=82,3 v=21,-11
p=99,20 v=37,31
p=71,72 v=-53,27
p=64,89 v=-51,70
p=82,47 v=-52,69
p=6,16 v=23,51
p=30,96 v=-78,44
p=32,71 v=6,-96
p=15,57 v=-56,67
p=44,2 v=90,-34
p=11,83 v=34,-90
p=15,71 v=-13,-93
p=34,60 v=98,30
p=45,27 v=35,-77
p=43,74 v=-53,45
p=17,80 v=63,-60
p=71,82 v=-62,27
p=28,49 v=80,-62
p=69,5 v=-49,-65
p=13,66 v=17,27
p=18,27 v=90,-40
p=29,75 v=-65,22
p=37,90 v=57,-43
p=5,88 v=-69,61
p=2,35 v=74,-44
p=25,56 v=32,-99
p=49,17 v=-33,62
p=66,35 v=94,29
p=35,74 v=-19,36
p=19,62 v=-44,18
p=59,92 v=-14,-3
p=75,57 v=23,76
p=80,34 v=-66,-28
p=31,22 v=-18,54
p=31,45 v=-56,49
p=67,98 v=-55,-87
p=91,34 v=-16,-46
p=29,86 v=12,-8
p=28,26 v=-42,4
p=79,80 v=-39,63
p=48,37 v=-98,-7
p=81,8 v=-49,-67
p=60,99 v=17,-17
p=18,95 v=93,-90
p=82,102 v=-76,-95
p=25,4 v=48,4
p=98,66 v=-51,57
p=46,42 v=-88,78
p=12,46 v=-7,49
p=99,72 v=-54,47
p=88,102 v=-15,76
p=52,33 v=36,-46
p=12,54 v=67,92
p=77,16 v=77,-93
p=61,99 v=24,77
p=47,18 v=17,33
p=24,78 v=43,61
p=57,56 v=-33,43
p=21,4 v=-64,50
p=83,1 v=26,8
p=62,82 v=21,-31
p=14,9 v=-89,-17
p=8,55 v=66,-4
p=55,78 v=-28,-19
p=20,10 v=48,-90
p=51,96 v=-24,78
p=93,55 v=-83,-32
p=94,51 v=-8,-46
p=43,73 v=52,-4
p=69,34 v=84,-78
p=95,2 v=-71,32
p=92,52 v=5,22
p=81,15 v=52,56
p=59,84 v=-20,-7
p=17,11 v=84,-15
p=45,97 v=47,31
p=58,84 v=93,-12
p=90,75 v=-1,-22
p=87,24 v=44,44
p=78,24 v=-47,-81
p=76,41 v=-99,73
p=59,52 v=70,-39
p=60,42 v=5,-95
p=85,57 v=-98,-11
p=37,81 v=-4,-10
p=11,15 v=24,34
p=6,78 v=-53,45
p=47,79 v=60,-33
p=9,92 v=-27,-67
p=69,30 v=-29,-29
p=85,71 v=98,-7
p=22,5 v=-77,-81
p=69,74 v=-62,-79
p=20,20 v=-6,2
p=66,72 v=-22,-44
p=99,96 v=86,-75
p=87,91 v=-11,86
p=39,66 v=-79,88
p=94,24 v=67,15
p=7,90 v=-71,71
p=23,7 v=15,87
p=90,67 v=-64,-37
p=55,77 v=58,-62
p=49,81 v=-28,56
p=3,74 v=23,-61"""

parsePos : String -> Maybe Pos 
parsePos s = 
  case String.split "=" s of 
    [_, s0] -> 
      case String.split "," s0 of 
        [a, b] -> 
          case (String.toInt a, String.toInt b) of 
            (Just x, Just y) -> Just (x, y)
            _ -> Nothing 
        _ -> Nothing 
    _ -> Nothing

parseRobot : String -> Maybe Robot
parseRobot s = 
  case String.split " " s of 
    [s1, s2] -> 
      case (parsePos s1, parsePos s2) of 
        (Just p, Just v) -> Just { position = p, velocity = v }
        _ -> Nothing
    _ -> Nothing

initRobots : DataSource -> List Robot
initRobots dataSource = 
  let 
    data = 
      case dataSource of 
        Sample -> sample 
        Input -> input 
        Bonus -> bonus 
  in 
    data |> String.split "\n" |> List.filterMap parseRobot 

initDim : DataSource -> (Int, Int)
initDim dataSource = 
  case dataSource of 
    Sample -> (11, 7)
    Input -> (101, 103)
    Bonus -> (101, 103)

initModel : Bool -> DataSource -> Model 
initModel findEasterEgg dataSource = 
  let 
    robots = initRobots dataSource
    (width, height) = initDim dataSource 
  in 
    { robots = robots
    , width = width 
    , height = height 
    , findEasterEgg = findEasterEgg
    , steps = 0
    , dataSource = dataSource
    , paused = True
    , finished = False 
    , tickInterval = defaultTickInterval
    , debug = "" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel False Input, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | ToggleEasterEgg
  | Faster 
  | Slower 
  | Clear 
  | UseSample 
  | UseBonus 
  | UseInput 

getNestedPositions : Int -> Int -> List (List Pos)
getNestedPositions width height = 
  let
    ys = List.range 0 (height - 1)
    xs = List.range 0 (width - 1)
  in 
    ys |> List.map (\y -> xs |> List.map (\x -> (x, y)))

move : Int -> Int -> Robot -> Robot
move width height robot = 
  let 
    (x, y) = robot.position
    (vx, vy) = robot.velocity 
    xMoved = (x + vx + width) |> modBy width
    yMoved = (y + vy + height) |> modBy height
  in 
    { robot | position = (xMoved, yMoved) }

moveAll : Int -> Int -> List Robot -> List Robot 
moveAll width height robots = 
  robots |> List.map (move width height)

countPositions : List Pos -> Int 
countPositions positions = 
  positions |> gatherEquals |> List.map (\(_, lst) -> 1 + List.length lst) |> List.sum 

toQuadrant : (Pos -> Bool) -> List Robot -> List Pos
toQuadrant predicate robots = 
  robots |> List.map (\r -> r.position) |> List.filter predicate 

calculateSafetyFactor : Int -> Int -> List Robot -> Int 
calculateSafetyFactor width height robots = 
  let 
    midRow = height // 2
    midCol = width // 2
    nw = robots |> toQuadrant (\(x, y) -> x < midCol && y < midRow) |> countPositions 
    ne = robots |> toQuadrant (\(x, y) -> x > midCol && y < midRow) |> countPositions 
    sw = robots |> toQuadrant (\(x, y) -> x < midCol && y > midRow) |> countPositions 
    se = robots |> toQuadrant (\(x, y) -> x > midCol && y > midRow) |> countPositions 
  in 
    nw * ne * sw * se

longLine : Set Pos -> Pos -> Bool 
longLine posSet (xStart, y) = 
  List.range xStart (xStart + 10) |> List.all (\x -> Set.member (x, y) posSet)

checkEasterEgg : List Robot -> Bool 
checkEasterEgg robots = 
  let 
    posList = robots |> List.map (\r -> r.position)
    posSet = posList |> Set.fromList 
  in 
    posList |> List.any (longLine posSet)

updateClear : Model -> Model
updateClear model = 
  initModel model.findEasterEgg model.dataSource 

updateStep : Model -> Model
updateStep model = 
  let 
    steps = model.steps 
    robots = moveAll model.width model.height model.robots 
    pause = model.paused || steps + 1 == 100 || (model.findEasterEgg && checkEasterEgg robots)
  in 
    { model | robots = robots, steps = steps + 1, paused = pause }

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  { model | paused = not model.paused }

updateToggleEasterEgg : Model -> Model
updateToggleEasterEgg model = 
  let
    findEasterEgg = not model.findEasterEgg
  in
    initModel findEasterEgg model.dataSource 

updateDataSource : DataSource -> Model -> Model
updateDataSource dataSource model = 
  initModel model.findEasterEgg dataSource  

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> 
      (updateClear model, Cmd.none)
    Tick ->
      (updateStep model, Cmd.none)
    Step ->
      (updateStep model, Cmd.none)
    Faster -> 
      ({model | tickInterval = model.tickInterval / 2 }, Cmd.none)
    Slower -> 
      ({model | tickInterval = model.tickInterval * 2 }, Cmd.none)
    TogglePlay -> 
      (updateTogglePlay model, Cmd.none)
    ToggleEasterEgg -> 
      (updateToggleEasterEgg model, Cmd.none)
    UseSample -> 
      (updateDataSource Sample model, Cmd.none)
    UseBonus -> 
      (updateDataSource Bonus model, Cmd.none)
    UseInput -> 
      (updateDataSource Input model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused || model.finished then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

-- VIEW

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2024 | Day 14: Restroom Redoubt"
  , body = [ viewBody model ] }

toCharElement : Set Pos -> Pos -> Html Msg 
toCharElement robotSet (x, y) = 
  Html.text (if Set.member (x, y) robotSet then "#" else ".")

viewBody : Model -> Html Msg
viewBody model =
  let
    textFontSize = 
      case model.dataSource of 
        Sample -> "24px"
        Input -> "12px"
        Bonus -> "12px"
    robotPositions = model.robots |> List.map (\r -> r.position) |> Set.fromList 
    nestedPositions = getNestedPositions model.width model.height 
    nestedElements = nestedPositions |> List.map (\ps -> ps |> List.map (toCharElement robotPositions))
    elements = nestedElements |> List.foldr (\a b -> List.append a (Html.br [] [] :: b)) []
    safetyFactor = calculateSafetyFactor model.width model.height model.robots
  in 
    Html.table 
      [ Html.Attributes.align "center"
      , Html.Attributes.style "width" "100%"
      , Html.Attributes.style "font-family" "Courier New" ]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "20px"]
              [ Html.div [] [Html.text "Advent of Code 2024" ]
              , Html.div [] [Html.text "Day 14: Restroom Redoubt" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2024/day/14" ] 
                [ Html.text "https://adventofcode.com/2024/day/14" ]
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
              , Html.input 
                [ Html.Attributes.type_ "radio", onClick UseBonus, Html.Attributes.checked (model.dataSource == Bonus) ] 
                []
              , Html.label [] [ Html.text "Bonus" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Clear ] 
                [ Html.text "Reset"]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Slower ] 
                [ text "Slower" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then text "Play" else text "Pause" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Faster ] 
                [ text "Faster" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Step ] 
                [ Html.text "Step" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleEasterEgg, Html.Attributes.checked model.findEasterEgg ] 
                []
              , Html.label [] [ Html.text " Find easter egg" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px" ] 
              [ 
                Html.div [] [ Html.text ("Steps: " ++ String.fromInt model.steps) ]
              , Html.div [] [ Html.text ("Safety factor: " ++ String.fromInt safetyFactor) ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" textFontSize
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [] elements
              ] ] 
              ]
