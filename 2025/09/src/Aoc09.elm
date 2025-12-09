module Aoc09 exposing (..)

{- Advent of Code 2025. Day 09: Movie Theater. -}

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Time

defaultTickInterval : Float
defaultTickInterval = 50

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample

type alias Pos = (Int, Int)

type alias State = 
  { count : Int 
  , seen : Set Pos }

type alias Line = (Pos, Pos)

type alias Model = 
  { dataSource : DataSource
  , redTiles : List Pos 
  , lines : List Line
  , rectangles : List (Int, (Pos, Pos))
  , largest : Maybe (Pos, Pos)
  , factor : Float 
  , redOrGreenOnly : Bool 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , debug : String }

sample : String
sample = """7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3"""

input : String
input = """98346,50117
98346,51321
97590,51321
97590,52570
98378,52570
98378,53784
98153,53784
98153,54933
97379,54933
97379,56194
97715,56194
97715,57357
97262,57357
97262,58502
96789,58502
96789,59844
97322,59844
97322,60862
96301,60862
96301,62158
96496,62158
96496,63353
96245,63353
96245,64573
96059,64573
96059,65697
95564,65697
95564,66656
94642,66656
94642,68048
94866,68048
94866,69061
94123,69061
94123,70153
93582,70153
93582,71391
93344,71391
93344,72190
92231,72190
92231,73500
92100,73500
92100,74339
91119,74339
91119,75394
90523,75394
90523,76365
89795,76365
89795,77786
89716,77786
89716,78751
88956,78751
88956,79523
87950,79523
87950,80419
87122,80419
87122,81374
86368,81374
86368,82436
85727,82436
85727,83340
84904,83340
84904,84297
84131,84297
84131,84951
83062,84951
83062,85736
82129,85736
82129,86195
80920,86195
80920,87238
80214,87238
80214,87725
79055,87725
79055,88764
78320,88764
78320,89657
77460,89657
77460,89956
76194,89956
76194,91070
75461,91070
75461,91758
74445,91758
74445,92101
73235,92101
73235,92261
71945,92261
71945,93283
71101,93283
71101,93988
70082,93988
70082,94522
68978,94522
68978,94976
67839,94976
67839,95058
66563,95058
66563,95021
65267,95021
65267,95501
64155,95501
64155,96035
63051,96035
63051,96082
61812,96082
61812,96820
60745,96820
60745,97209
59582,97209
59582,96899
58288,96899
58288,97446
57151,97446
57151,97443
55925,97443
55925,97781
54742,97781
54742,97847
53527,97847
53527,97964
52316,97964
52316,97780
51095,97780
51095,98318
49883,98318
49883,97566
48679,97566
48679,97545
47473,97545
47473,97451
46270,97451
46270,98168
44983,98168
44983,97283
43861,97283
43861,97748
42566,97748
42566,97553
41358,97553
41358,96726
40279,96726
40279,96793
39022,96793
39022,96846
37749,96846
37749,96565
36553,96565
36553,95546
35588,95546
35588,95771
34230,95771
34230,95439
33045,95439
33045,94564
32073,94564
32073,94197
30906,94197
30906,93871
29713,93871
29713,92803
28875,92803
28875,92611
27610,92611
27610,92347
26361,92347
26361,90998
25731,90998
25731,90775
24447,90775
24447,89904
23562,89904
23562,88985
22725,88985
22725,88768
21386,88768
21386,87803
20591,87803
20591,86828
19821,86828
19821,86644
18387,86644
18387,85516
17755,85516
17755,84696
16858,84696
16858,84108
15726,84108
15726,82980
15135,82980
15135,81765
14668,81765
14668,80912
13814,80912
13814,80362
12579,80362
12579,79279
11983,79279
11983,78255
11324,78255
11324,77249
10646,77249
10646,76260
9943,76260
9943,75253
9265,75253
9265,74137
8769,74137
8769,73091
8159,73091
8159,72007
7619,72007
7619,70921
7086,70921
7086,69804
6620,69804
6620,68965
5508,68965
5508,67845
5008,67845
5008,66451
5247,66451
5247,65435
4483,65435
4483,64370
3807,64370
3807,62988
4187,62988
4187,61973
3289,61973
3289,60792
2976,60792
2976,59480
3292,59480
3292,58342
2797,58342
2797,57109
2829,57109
2829,55995
1994,55995
1994,54760
2034,54760
2034,53543
1929,53543
1929,52296
2446,52296
2446,51110
1556,51110
1556,50108
94697,50108
94697,48668
2048,48668
2048,47447
1975,47447
1975,46221
1919,46221
1919,45042
2390,45042
2390,43799
2240,43799
2240,42668
2906,42668
2906,41385
2599,41385
2599,40229
3033,40229
3033,39000
3116,39000
3116,37837
3488,37837
3488,36745
4099,36745
4099,35590
4459,35590
4459,34522
5075,34522
5075,33098
4701,33098
4701,32139
5600,32139
5600,31152
6372,31152
6372,30021
6793,30021
6793,28669
6780,28669
6780,27498
7176,27498
7176,26736
8325,26736
8325,25614
8804,25614
8804,24425
9189,24425
9189,23412
9869,23412
9869,22361
10494,22361
10494,21371
11210,21371
11210,20320
11848,20320
11848,19835
13189,19835
13189,18548
13542,18548
13542,18030
14786,18030
14786,16513
14942,16513
14942,16156
16320,16156
16320,15426
17294,15426
17294,14589
18162,14589
18162,13554
18865,13554
18865,12508
19580,12508
19580,11712
20511,11712
20511,11495
21868,11495
21868,10721
22802,10721
22802,9580
23501,9580
23501,9343
24795,9343
24795,8400
25647,8400
25647,7597
26598,7597
26598,7290
27821,7290
27821,6954
29014,6954
29014,6287
30043,6287
30043,6019
31252,6019
31252,5325
32280,5325
32280,5104
33495,5104
33495,4777
34664,4777
34664,4504
35846,4504
35846,3826
36908,3826
36908,3702
38132,3702
38132,3406
39306,3406
39306,3383
40537,3383
40537,2845
41666,2845
41666,2992
42915,2992
42915,2459
44062,2459
44062,1987
45234,1987
45234,2539
46501,2539
46501,2138
47688,2138
47688,2150
48903,2150
48903,1538
50117,1538
50117,2006
51332,2006
51332,2553
52521,2553
52521,2295
53748,2295
53748,2232
54974,2232
54974,2289
56193,2289
56193,2272
57429,2272
57429,2341
58660,2341
58660,3021
59772,3021
59772,3447
60921,3447
60921,3629
62126,3629
62126,3932
63302,3932
63302,4676
64340,4676
64340,4457
65690,4457
65690,4946
66809,4946
66809,5755
67797,5755
67797,5513
69218,5513
69218,6439
70142,6439
70142,7122
71161,7122
71161,7836
72154,7836
72154,7981
73455,7981
73455,8711
74439,8711
74439,8936
75733,8936
75733,10147
76403,10147
76403,10678
77509,10678
77509,11358
78519,11358
78519,12124
79464,12124
79464,12881
80415,12881
80415,13723
81295,13723
81295,14502
82227,14502
82227,15128
83308,15128
83308,16376
83787,16376
83787,17042
84839,17042
84839,17766
85851,17766
85851,18843
86471,18843
86471,19620
87442,19620
87442,20888
87797,20888
87797,21624
88839,21624
88839,22535
89663,22535
89663,23943
89744,23943
89744,24843
90578,24843
90578,25629
91630,25629
91630,26746
92135,26746
92135,27723
92898,27723
92898,28916
93246,28916
93246,30250
93259,30250
93259,31171
94169,31171
94169,32171
94949,32171
94949,33395
95168,33395
95168,34439
95886,34439
95886,35658
96099,35658
96099,36857
96354,36857
96354,38119
96346,38119
96346,39276
96726,39276
96726,40399
97295,40399
97295,41680
97073,41680
97073,42904
97079,42904
97079,44093
97286,44093
97286,45275
97608,45275
97608,46441
98277,46441
98277,47695
97720,47695
97720,48909
97577,48909
97577,50117"""

parsePos : String -> Maybe (Int, Int)
parsePos s = 
  case s |> String.split "," |> List.filterMap String.toInt of  
    [ a, b ] -> Just (a, b)
    _ -> Nothing 

area : Pos -> Pos -> Int 
area a b = 
  let 
    (x1, y1) = a 
    (x2, y2) = b 
  in 
    (1 + abs (x2 - x1)) * (1 + abs (y2 - y1))

getRectangles : List (Int, (Pos, Pos)) -> List Pos -> List (Int, (Pos, Pos))
getRectangles acc tiles = 
  case tiles of 
    a :: rest -> 
      let 
        rs = rest |> List.map (\b -> (area a b, (a, b)))
      in 
        getRectangles (rs ++ acc) rest 
    _ -> acc |> List.sort |> List.reverse 

connectLoop : Pos -> List (Pos, Pos) -> List Pos -> List (Pos, Pos)
connectLoop first acc tiles = 
  case tiles of 
    [] -> 
      acc |> List.reverse
    [ last ] -> 
      connectLoop first ((last, first) :: acc) []
    a :: b :: t -> 
      connectLoop first ((a, b) :: acc) (b :: t) 

connect : List Pos -> List (Pos, Pos)
connect tiles = 
  case tiles of 
    [] -> [] 
    first :: _ -> connectLoop first [] tiles 

initModel : Bool -> DataSource -> Model 
initModel redOrGreenOnly dataSource = 
  let 
    data = 
      case dataSource of 
        Input -> input 
        Sample -> sample 
    factor = 
      case dataSource of 
        Input -> 0.005 
        Sample -> 40.0 
    tiles = data |> String.split "\n" |> List.filterMap parsePos
    lines = connect tiles 
    rectangles = getRectangles [] tiles 
  in 
    { dataSource = dataSource
    , redTiles = tiles
    , lines = lines
    , factor = factor
    , redOrGreenOnly = redOrGreenOnly
    , largest = Nothing 
    , rectangles = rectangles 
    , paused = True
    , finished = False  
    , tickInterval = defaultTickInterval 
    , debug = "?" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel False Sample, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | ToggleRedOrGreenOnly 
  | Faster 
  | Slower 
  | Clear 
  | UseInput
  | UseSample

getNestedPositions : Int -> Int -> List (List Pos)
getNestedPositions rows columns = 
  let
    ys = List.range 0 (rows - 1) 
    xs = List.range 0 (columns - 1)
  in 
    ys |> List.map (\y -> xs |> List.map (\x -> (x, y)))

updateClear : Model -> Model
updateClear model = 
  initModel model.redOrGreenOnly model.dataSource

order : (Int, Int) -> (Int, Int) 
order (a, b) = 
  if a < b then (a, b) else (b, a)

inRange : Int -> (Int, Int) -> Bool 
inRange v (vMin, vMax) = 
  vMin < v && v < vMax 

check : (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool 
check xRange yRange (x, y) = 
  inRange x xRange && inRange y yRange 

checkLine : (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int)) -> Bool 
checkLine xRange yRange ((xLine1, yLine1), (xLine2, yLine2)) = 
  let 
    (xMin, xMax) = xRange
    (yMin, yMax) = yRange 
  in 
    if xLine1 == xLine2 then 
        let 
          (yLineMin, yLineMax) = order (yLine1, yLine2)
        in 
          inRange xLine1 xRange && yLineMin <= yMin && yLineMax >= yMax
    else 
        let 
          (xLineMin, xLineMax) = order (xLine1, xLine2)
        in 
          inRange yLine1 yRange && xLineMin <= xMin && xLineMax >= xMax

violates : List Pos -> List (Pos, Pos) -> (Pos, Pos) -> Bool 
violates tiles lines ((x1, y1), (x2, y2)) = 
  let 
    xRange = order (x1, x2)
    yRange = order (y1, y2)
  in 
    (tiles |> List.any (check xRange yRange)) || (lines |> List.any (checkLine xRange yRange))

updateRedOrGreenOnlyStep : Model -> Model
updateRedOrGreenOnlyStep model = 
  case model.rectangles of 
    [] -> { model | finished = True, paused = True, debug = "WRONG" }
    (_, rect) :: rest -> 
      let 
        tiles = model.redTiles 
        lines = model.lines 
      in 
        if violates tiles lines rect then 
          { model | rectangles = rest, largest = Just rect, debug = "INVALID" }
        else 
          { model | finished = True, paused = True, largest = Just rect, rectangles = rest, debug = "FOUND" }

updateStep : Model -> Model
updateStep model = 
  if model.finished then model 
  else 
    if model.redOrGreenOnly then 
      updateRedOrGreenOnlyStep model 
    else 
      case model.rectangles of 
        [] -> { model | finished = True, paused = True, debug = "WRONG" }
        (_, rect) :: rest -> 
          { model | finished = True, paused = True, largest = Just rect, rectangles = rest }

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  if model.finished then 
    let
      m = updateClear model 
    in 
      { m | paused = False }
  else 
    { model | paused = not model.paused }

updateToggleRedOrGreenOnly : Model -> Model
updateToggleRedOrGreenOnly model = 
  { model | redOrGreenOnly = not model.redOrGreenOnly }

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
    ToggleRedOrGreenOnly -> 
      (updateToggleRedOrGreenOnly model, Cmd.none)
    UseInput -> 
      (initModel model.redOrGreenOnly Input, Cmd.none)
    UseSample -> 
      (initModel model.redOrGreenOnly Sample, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

-- VIEW

posToString : Float -> Pos -> String 
posToString factor pos = 
  case pos of 
    (x, y) -> 
      let 
        xStr = String.fromFloat (factor * toFloat x)
        yStr = String.fromFloat (factor * toFloat y)
      in 
        xStr ++ "," ++ yStr 

toTileElement : Float -> (Int, Int) -> Html Msg 
toTileElement factor (xPos, yPos) = 
  let 
    xVal = factor * toFloat xPos - 1.5
    yVal = factor * toFloat yPos - 1.5
  in
    Svg.rect
      [ Svg.Attributes.x (String.fromFloat xVal)
      , Svg.Attributes.y (String.fromFloat yVal)
      , Svg.Attributes.width "3" 
      , Svg.Attributes.height "3"
      , Svg.Attributes.fill "red" ]
      []

toBoxElement : Float -> (Pos, Pos) -> Svg Msg 
toBoxElement factor (pos1, pos2) = 
  let 
    (x1, y1) = pos1 
    (x2, y2) = pos2
    (xMin, xMax) = if x1 < x2 then (x1, x2) else (x2, x1)
    (yMin, yMax) = if y1 < y2 then (y1, y2) else (y2, y1)
    posList = [ (xMin, yMin), (xMax, yMin), (xMax, yMax), (xMin, yMax) ]
    pts = posList |> List.map (posToString factor) |> String.join " "
  in 
    Svg.polygon [ Svg.Attributes.points pts, Svg.Attributes.fill "none", Svg.Attributes.stroke "black" ] []

toPolygon : Model -> Svg Msg 
toPolygon model = 
  let 
    pts = model.redTiles |> List.map (posToString model.factor) |> String.join " "
  in 
    Svg.polygon [ Svg.Attributes.points pts, Svg.Attributes.fill "lightgreen", Svg.Attributes.stroke "lightgreen" ] []

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    polygon = toPolygon model 
    tileElements = model.redTiles |> List.map (toTileElement model.factor)
    polygonElementList = 
      if model.redOrGreenOnly then [ toPolygon model ] else []
    boxElementList = 
      case model.largest of 
        Just box -> 
          [ toBoxElement model.factor box ] 
        Nothing -> []
    elements = polygonElementList ++ boxElementList ++ tileElements
  in 
    Svg.svg
      [ viewBox ("0 0 500 500")
      , width "500"
      , height "500"
      -- , Svg.Attributes.style "background-color:pink" 
      ]
      elements

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2025 | Day 9: Movie Theater"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    elements = [ toSvg model ]    
    textFontSize =
      case model.dataSource of 
        Sample -> "24px"
        Input -> "8px"
    areaSize = 
      case model.largest of 
        Just (pos1, pos2) -> area pos1 pos2 |> String.fromInt 
        Nothing -> "?"
    -- debug = model.debug 
    -- debug = model.lines |> List.length |> String.fromInt
    debug = ""
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
              , Html.Attributes.style "padding" "10px"]
              [ Html.div [] [Html.text "Advent of Code 2025" ]
              , Html.div [] [Html.text "Day 9: Movie Theater" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2025/day/9" ] 
                [ Html.text "https://adventofcode.com/2025/day/9" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
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
                [ Html.Attributes.style "width" "80px", onClick Clear ] 
                [ Html.text "Reset" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Slower ] 
                [ Html.text "Slower" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then Html.text "Solve" else Html.text "Pause" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Faster ] 
                [ Html.text "Faster" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Step ] 
                [ Html.text "Step" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleRedOrGreenOnly, Html.Attributes.checked model.redOrGreenOnly ] 
                []
              , Html.label [] [ Html.text " Red or green only" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "1.2rem"
              , Html.Attributes.style "padding-top" "10px" ] 
              [ 
                Html.div [] [ Html.text areaSize ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" textFontSize
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [
                  Html.Attributes.align "center" 
                , Html.Attributes.style "max-width" "100%"
                ] elements
              ] ] 
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding" "0px" ] 
              [ 
                -- Html.div [] [ Html.text (model.moves |> List.length |> String.fromInt ) ]
              -- , Html.div [] [ Html.text (String.fromInt model.position) ]
                Html.div [] [ Html.text debug ]
              -- , Html.div [] [ Html.text model.message ]
              ] ] 
              ]
