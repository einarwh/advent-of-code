module Main exposing (..)

import Browser 
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Set exposing (Set)
import Array2D exposing (Array2D)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import Image

defaultTickInterval : Float
defaultTickInterval = 500

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type Viz = NoViz | SvgViz | ImgViz 

type alias Pos = (Int, Int)

type Instruction =
    TurnOn (Pos, Pos)
    | TurnOff (Pos, Pos)
    | Toggle (Pos, Pos)

type alias Model = 
  { viz : Viz 
  , bulbs : List (Pos, Int)
  , instructions : List Instruction
  , instructionNumber : Int 
  , totalInstructionCount : Int 
  , useBrightness : Bool 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , lastCommandText : String
  , counter : Int 
  , debug : String }

input : String
input = """toggle 461,550 through 564,900
turn off 370,39 through 425,839
turn off 464,858 through 833,915
turn off 812,389 through 865,874
turn on 599,989 through 806,993
turn on 376,415 through 768,548
turn on 606,361 through 892,600
turn off 448,208 through 645,684
toggle 50,472 through 452,788
toggle 205,417 through 703,826
toggle 533,331 through 906,873
toggle 857,493 through 989,970
turn off 631,950 through 894,975
turn off 387,19 through 720,700
turn off 511,843 through 581,945
toggle 514,557 through 662,883
turn off 269,809 through 876,847
turn off 149,517 through 716,777
turn off 994,939 through 998,988
toggle 467,662 through 555,957
turn on 952,417 through 954,845
turn on 565,226 through 944,880
turn on 214,319 through 805,722
toggle 532,276 through 636,847
toggle 619,80 through 689,507
turn on 390,706 through 884,722
toggle 17,634 through 537,766
toggle 706,440 through 834,441
toggle 318,207 through 499,530
toggle 698,185 through 830,343
toggle 566,679 through 744,716
toggle 347,482 through 959,482
toggle 39,799 through 981,872
turn on 583,543 through 846,710
turn off 367,664 through 595,872
turn on 805,439 through 964,995
toggle 209,584 through 513,802
turn off 106,497 through 266,770
turn on 975,2 through 984,623
turn off 316,684 through 369,876
turn off 30,309 through 259,554
turn off 399,680 through 861,942
toggle 227,740 through 850,829
turn on 386,603 through 552,879
turn off 703,795 through 791,963
turn off 573,803 through 996,878
turn off 993,939 through 997,951
turn on 809,221 through 869,723
turn off 38,720 through 682,751
turn off 318,732 through 720,976
toggle 88,459 through 392,654
turn off 865,654 through 911,956
toggle 264,284 through 857,956
turn off 281,776 through 610,797
toggle 492,660 through 647,910
turn off 879,703 through 925,981
turn off 772,414 through 974,518
turn on 694,41 through 755,96
turn on 452,406 through 885,881
turn off 107,905 through 497,910
turn off 647,222 through 910,532
turn on 679,40 through 845,358
turn off 144,205 through 556,362
turn on 871,804 through 962,878
turn on 545,676 through 545,929
turn off 316,716 through 413,941
toggle 488,826 through 755,971
toggle 957,832 through 976,992
toggle 857,770 through 905,964
toggle 319,198 through 787,673
turn on 832,813 through 863,844
turn on 818,296 through 818,681
turn on 71,699 through 91,960
turn off 838,578 through 967,928
toggle 440,856 through 507,942
toggle 121,970 through 151,974
toggle 391,192 through 659,751
turn on 78,210 through 681,419
turn on 324,591 through 593,939
toggle 159,366 through 249,760
turn off 617,167 through 954,601
toggle 484,607 through 733,657
turn on 587,96 through 888,819
turn off 680,984 through 941,991
turn on 800,512 through 968,691
turn off 123,588 through 853,603
turn on 1,862 through 507,912
turn on 699,839 through 973,878
turn off 848,89 through 887,893
toggle 344,353 through 462,403
turn on 780,731 through 841,760
toggle 693,973 through 847,984
toggle 989,936 through 996,958
toggle 168,475 through 206,963
turn on 742,683 through 769,845
toggle 768,116 through 987,396
turn on 190,364 through 617,526
turn off 470,266 through 530,839
toggle 122,497 through 969,645
turn off 492,432 through 827,790
turn on 505,636 through 957,820
turn on 295,476 through 698,958
toggle 63,298 through 202,396
turn on 157,315 through 412,939
turn off 69,789 through 134,837
turn off 678,335 through 896,541
toggle 140,516 through 842,668
turn off 697,585 through 712,668
toggle 507,832 through 578,949
turn on 678,279 through 886,621
toggle 449,744 through 826,910
turn off 835,354 through 921,741
toggle 924,878 through 985,952
turn on 666,503 through 922,905
turn on 947,453 through 961,587
toggle 525,190 through 795,654
turn off 62,320 through 896,362
turn on 21,458 through 972,536
turn on 446,429 through 821,970
toggle 376,423 through 805,455
toggle 494,896 through 715,937
turn on 583,270 through 667,482
turn off 183,468 through 280,548
toggle 623,289 through 750,524
turn on 836,706 through 967,768
turn on 419,569 through 912,908
turn on 428,260 through 660,433
turn off 683,627 through 916,816
turn on 447,973 through 866,980
turn on 688,607 through 938,990
turn on 245,187 through 597,405
turn off 558,843 through 841,942
turn off 325,666 through 713,834
toggle 672,606 through 814,935
turn off 161,812 through 490,954
turn on 950,362 through 985,898
turn on 143,22 through 205,821
turn on 89,762 through 607,790
toggle 234,245 through 827,303
turn on 65,599 through 764,997
turn on 232,466 through 965,695
turn on 739,122 through 975,590
turn off 206,112 through 940,558
toggle 690,365 through 988,552
turn on 907,438 through 977,691
turn off 838,809 through 944,869
turn on 222,12 through 541,832
toggle 337,66 through 669,812
turn on 732,821 through 897,912
toggle 182,862 through 638,996
turn on 955,808 through 983,847
toggle 346,227 through 841,696
turn on 983,270 through 989,756
turn off 874,849 through 876,905
turn off 7,760 through 678,795
toggle 973,977 through 995,983
turn off 911,961 through 914,976
turn on 913,557 through 952,722
turn off 607,933 through 939,999
turn on 226,604 through 517,622
turn off 3,564 through 344,842
toggle 340,578 through 428,610
turn on 248,916 through 687,925
toggle 650,185 through 955,965
toggle 831,359 through 933,536
turn off 544,614 through 896,953
toggle 648,939 through 975,997
turn on 464,269 through 710,521
turn off 643,149 through 791,320
turn off 875,549 through 972,643
turn off 953,969 through 971,972
turn off 236,474 through 772,591
toggle 313,212 through 489,723
toggle 896,829 through 897,837
toggle 544,449 through 995,905
turn off 278,645 through 977,876
turn off 887,947 through 946,977
turn on 342,861 through 725,935
turn on 636,316 through 692,513
toggle 857,470 through 950,528
turn off 736,196 through 826,889
turn on 17,878 through 850,987
turn on 142,968 through 169,987
turn on 46,470 through 912,853
turn on 182,252 through 279,941
toggle 261,143 through 969,657
turn off 69,600 through 518,710
turn on 372,379 through 779,386
toggle 867,391 through 911,601
turn off 174,287 through 900,536
toggle 951,842 through 993,963
turn off 626,733 through 985,827
toggle 622,70 through 666,291
turn off 980,671 through 985,835
turn off 477,63 through 910,72
turn off 779,39 through 940,142
turn on 986,570 through 997,638
toggle 842,805 through 943,985
turn off 890,886 through 976,927
turn off 893,172 through 897,619
turn off 198,780 through 835,826
toggle 202,209 through 219,291
turn off 193,52 through 833,283
toggle 414,427 through 987,972
turn on 375,231 through 668,236
turn off 646,598 through 869,663
toggle 271,462 through 414,650
turn off 679,121 through 845,467
toggle 76,847 through 504,904
turn off 15,617 through 509,810
toggle 248,105 through 312,451
turn off 126,546 through 922,879
turn on 531,831 through 903,872
toggle 602,431 through 892,792
turn off 795,223 through 892,623
toggle 167,721 through 533,929
toggle 813,251 through 998,484
toggle 64,640 through 752,942
turn on 155,955 through 892,985
turn on 251,329 through 996,497
turn off 341,716 through 462,994
toggle 760,127 through 829,189
turn on 86,413 through 408,518
toggle 340,102 through 918,558
turn off 441,642 through 751,889
turn on 785,292 through 845,325
turn off 123,389 through 725,828
turn on 905,73 through 983,270
turn off 807,86 through 879,276
toggle 500,866 through 864,916
turn on 809,366 through 828,534
toggle 219,356 through 720,617
turn off 320,964 through 769,990
turn off 903,167 through 936,631
toggle 300,137 through 333,693
toggle 5,675 through 755,848
turn off 852,235 through 946,783
toggle 355,556 through 941,664
turn on 810,830 through 867,891
turn off 509,869 through 667,903
toggle 769,400 through 873,892
turn on 553,614 through 810,729
turn on 179,873 through 589,962
turn off 466,866 through 768,926
toggle 143,943 through 465,984
toggle 182,380 through 569,552
turn off 735,808 through 917,910
turn on 731,802 through 910,847
turn off 522,74 through 731,485
turn on 444,127 through 566,996
turn off 232,962 through 893,979
turn off 231,492 through 790,976
turn on 874,567 through 943,684
toggle 911,840 through 990,932
toggle 547,895 through 667,935
turn off 93,294 through 648,636
turn off 190,902 through 532,970
turn off 451,530 through 704,613
toggle 936,774 through 937,775
turn off 116,843 through 533,934
turn on 950,906 through 986,993
turn on 910,51 through 945,989
turn on 986,498 through 994,945
turn off 125,324 through 433,704
turn off 60,313 through 75,728
turn on 899,494 through 940,947
toggle 832,316 through 971,817
toggle 994,983 through 998,984
toggle 23,353 through 917,845
toggle 174,799 through 658,859
turn off 490,878 through 534,887
turn off 623,963 through 917,975
toggle 721,333 through 816,975
toggle 589,687 through 890,921
turn on 936,388 through 948,560
turn off 485,17 through 655,610
turn on 435,158 through 689,495
turn on 192,934 through 734,936
turn off 299,723 through 622,847
toggle 484,160 through 812,942
turn off 245,754 through 818,851
turn on 298,419 through 824,634
toggle 868,687 through 969,760
toggle 131,250 through 685,426
turn off 201,954 through 997,983
turn on 353,910 through 832,961
turn off 518,781 through 645,875
turn off 866,97 through 924,784
toggle 836,599 through 857,767
turn on 80,957 through 776,968
toggle 277,130 through 513,244
turn off 62,266 through 854,434
turn on 792,764 through 872,842
turn off 160,949 through 273,989
turn off 664,203 through 694,754
toggle 491,615 through 998,836
turn off 210,146 through 221,482
turn off 209,780 through 572,894
turn on 766,112 through 792,868
turn on 222,12 through 856,241"""

tryParsePos : String -> Maybe Pos 
tryParsePos s = 
  case String.split "," s of 
    [xStr, yStr] -> 
      case (String.toInt xStr, String.toInt yStr) of 
        (Just x, Just y) -> Just (x, y)
        _ -> Nothing
    _ -> Nothing

tryParseTurnOn : String -> Maybe Instruction
tryParseTurnOn s = 
  case String.split " " s of 
    [_, _, posStr1, _, posStr2] -> 
      case (tryParsePos posStr1, tryParsePos posStr2) of 
        (Just pos1, Just pos2) -> Just (TurnOn (pos1, pos2))
        _ -> Nothing
    _ -> Nothing

tryParseTurnOff : String -> Maybe Instruction
tryParseTurnOff s = 
  case String.split " " s of 
    [_, _, posStr1, _, posStr2] -> 
      case (tryParsePos posStr1, tryParsePos posStr2) of 
        (Just pos1, Just pos2) -> Just (TurnOff (pos1, pos2))
        _ -> Nothing
    _ -> Nothing

tryParseToggle : String -> Maybe Instruction
tryParseToggle s = 
  case String.split " " s of 
    [_, posStr1, _, posStr2] -> 
      case (tryParsePos posStr1, tryParsePos posStr2) of 
        (Just pos1, Just pos2) -> Just (Toggle (pos1, pos2))
        _ -> Nothing
    _ -> Nothing

tryParse : String -> Maybe Instruction
tryParse s =
  if String.startsWith "turn on" s then 
    tryParseTurnOn s 
  else if String.startsWith "turn off" s then 
    tryParseTurnOff s
  else if String.startsWith "toggle" s then 
    tryParseToggle s
  else 
    Nothing

initModel : Viz -> Bool -> Model
initModel viz useBrightness =
  let 
    tickInterval = 
      case viz of 
        NoViz -> 50
        _ -> 500
    instructions = input |> String.split "\n" |> List.filterMap tryParse
    model = { viz = viz
            , bulbs = getPositions (0, 0) (999, 999) |> List.map (\p -> (p, 0))
            , instructions = instructions
            , instructionNumber = 0
            , totalInstructionCount = List.length instructions
            , lastCommandText = "press play to start"
            , paused = True 
            , finished = False
            , tickInterval = tickInterval
            , useBrightness = useBrightness
            , counter = 0
            , debug = "" }
  in 
    model 

init : () -> (Model, Cmd Msg)
init _ =
  let 
    model = initModel NoViz False
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | Solve 
  | TogglePlay 
  | ToggleBrightness
  | UseNoViz
  | UseSvgViz
  | UseImgViz
  | Clear 

getPositions : Pos -> Pos -> List Pos
getPositions (xMin, yMin) (xMax, yMax) = 
  let
    ys = List.range yMin yMax
    xs = List.range xMin xMax
  in 
    ys |> List.concatMap (\y -> xs |> List.map (\x -> (x, y)))

updateClear : Model -> Model
updateClear model = initModel NoViz model.useBrightness

isInsideBounds : Pos -> Pos -> Pos -> Bool
isInsideBounds (xMin, yMin) (xMax, yMax) (x, y) =
  xMin <= x && x <= xMax && yMin <= y && y <= yMax 

executeInstruction : Instruction -> List (Pos, Int) -> List (Pos, Int)
executeInstruction inst bulbs = 
  case inst of 
    TurnOn (pos1, pos2) -> 
      bulbs |> List.map (\(p, c) -> if isInsideBounds pos1 pos2 p then (p, 1) else (p, c))
    TurnOff (pos1, pos2) -> 
      bulbs |> List.map (\(p, c) -> if isInsideBounds pos1 pos2 p then (p, 0) else (p, c))
    Toggle (pos1, pos2) -> 
      bulbs |> List.map (\(p, c) -> if isInsideBounds pos1 pos2 p then (p, if c == 0 then 1 else 0) else (p, c))

executeInstructionBrightness : Instruction -> List (Pos, Int) -> List (Pos, Int)
executeInstructionBrightness inst bulbs = 
  case inst of 
    TurnOn (pos1, pos2) -> 
      bulbs |> List.map (\(p, c) -> if isInsideBounds pos1 pos2 p then (p, c + 1) else (p, c))
    TurnOff (pos1, pos2) -> 
      bulbs |> List.map (\(p, c) -> if isInsideBounds pos1 pos2 p then (p, Basics.max 0 (c - 1)) else (p, c))
    Toggle (pos1, pos2) -> 
      bulbs |> List.map (\(p, c) -> if isInsideBounds pos1 pos2 p then (p, c + 2) else (p, c))
      
updateStep : Model -> Model
updateStep model = 
  case model.instructions of 
    [] -> { model | paused = True, finished = True } 
    h :: t -> 
      let 
        str = 
          case h of 
            TurnOn ((x0, y0), (x1, y1)) -> 
              "turn on (" ++ String.fromInt x0 ++ "," ++ String.fromInt y0 ++ ") through (" ++ String.fromInt x1 ++ "," ++ String.fromInt y1 ++ ")"
            TurnOff ((x0, y0), (x1, y1)) -> 
              "turn off (" ++ String.fromInt x0 ++ "," ++ String.fromInt y0 ++ ") through (" ++ String.fromInt x1 ++ "," ++ String.fromInt y1 ++ ")"
            Toggle ((x0, y0), (x1, y1)) -> 
              "toggle (" ++ String.fromInt x0 ++ "," ++ String.fromInt y0 ++ ") through (" ++ String.fromInt x1 ++ "," ++ String.fromInt y1 ++ ")"
        execute = if model.useBrightness then executeInstructionBrightness else executeInstruction
        bulbs = execute h model.bulbs 
        num = model.instructionNumber
      in 
        { model | instructions = t, bulbs = bulbs, debug = str, instructionNumber = num + 1 }

updateSolveLoop : Model -> Model 
updateSolveLoop model = 
  case model.instructions of 
    [] -> model 
    _ -> model |> updateStep |> updateSolveLoop

updateSolve : Model -> Model
updateSolve model = 
  if model.finished then 
    let 
      m = initModel model.viz model.useBrightness
    in 
      {m | paused = False }
  else 
    updateSolveLoop model 

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  if model.finished then 
    let 
      m = initModel model.viz model.useBrightness
    in 
      {m | paused = False }
  else 
    { model | paused = not model.paused }

updateToggleBrightness : Model -> Model
updateToggleBrightness model = 
  let
    viz = model.viz 
    useBrightness = not model.useBrightness
  in
    initModel viz useBrightness

updateViz : Viz -> Model -> Model
updateViz viz model = 
  initModel viz model.useBrightness

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> 
      (updateClear model, Cmd.none)
    Solve -> 
      (updateSolve model, Cmd.none)
    Tick ->
      (updateStep model, Cmd.none)
    Step ->
      (updateStep model, Cmd.none)
    TogglePlay -> 
      (updateTogglePlay model, Cmd.none)
    UseNoViz -> 
      (updateViz NoViz model, Cmd.none)
    UseSvgViz -> 
      (updateViz SvgViz model, Cmd.none)
    UseImgViz -> 
      (updateViz ImgViz model, Cmd.none)
    ToggleBrightness -> 
      (updateToggleBrightness model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused || model.finished then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

-- VIEW

toRect : (Pos, Int) -> Html Msg
toRect ((xx, yy), value) = 
      let 
        fillColor = if value == 0 then "black" else "white"
        xf = (toFloat xx / 2)
        yf = (toFloat yy / 2)
        s = String.fromInt xx ++ "," ++ String.fromInt yy
        r =         
          rect
            [ x (String.fromFloat xf)
            , y (String.fromFloat yf)
            , id s 
            , width "0.5"
            , height "0.5"
            , fill fillColor
            ]
            []
      in
        r

chooseColor value = 
  case value of 
    0 -> "#000000"
    1 -> "#080808"
    2 -> "#101010"
    3 -> "#181818"
    4 -> "#202020"
    5 -> "#282828"
    6 -> "#303030"
    7 -> "#383838"
    8 -> "#404040"
    9 -> "#484848"
    10 -> "#505050"
    11 -> "#585858"
    12 -> "#606060"
    13 -> "#686868"
    14 -> "#696969"
    15 -> "#707070"
    16 -> "#787878"
    17 -> "#808080"
    18 -> "#888888"
    19 -> "#909090"
    20 -> "#989898"
    21 -> "#A0A0A0"
    22 -> "#A8A8A8"
    23 -> "#A9A9A9"
    24 -> "#B0B0B0"
    25 -> "#B8B8B8"
    26 -> "#BEBEBE"
    27 -> "#C0C0C0"
    28 -> "#C8C8C8"
    29 -> "#D0D0D0"
    30 -> "#D3D3D3"
    31 -> "#D8D8D8"
    32 -> "#DCDCDC"
    33 -> "#E0E0E0"
    34 -> "#E8E8E8"
    35 -> "#F0F0F0"
    36 -> "#F5F5F5"
    37 -> "#F8F8F8"
    _ -> "#FFFFFF"

toRectBrightness : (Pos, Int) -> (Html Msg)
toRectBrightness ((xx, yy), value) = 
      let 
        fillColor = chooseColor value
        xf = (toFloat xx / 2)
        yf = (toFloat yy / 2)
        s = String.fromInt xx ++ "," ++ String.fromInt yy
        r =         
          rect
            [ x (String.fromFloat xf)
            , y (String.fromFloat yf)
            , id s 
            , width "0.5"
            , height "0.5"
            , fill fillColor
            ]
            []
      in
        r

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    svgWidth = 500 |> String.fromInt
    svgHeight = 500 |> String.fromInt
    toRectFn = if model.useBrightness then toRectBrightness else toRect
    elements = model.bulbs |> List.map toRectFn
  in 
    svg
      [ viewBox ("0 0 " ++ svgWidth ++ svgHeight)
      , width svgWidth
      , height svgHeight
      , Svg.Attributes.style "max-width: 100%; background-color:black"
      ]
      elements

toImage : Model -> Html Msg 
toImage model = 
  let 
    pixels = model.bulbs |> List.map (\(p, c) -> if c == 0 then 0x000000FF else 0xFFFFFFFF)
    imageData = pixels |> Image.fromList 1000
    pngEncodeBase64Url = Image.toPngUrl imageData
  in 
    Html.img [ Html.Attributes.width 500, Html.Attributes.src pngEncodeBase64Url ] []

view : Model -> Html Msg
view model =
  let
    count = model.bulbs |> List.map (\(_, c) -> c) |> List.sum 
    elements = 
      case model.viz of 
        NoViz -> []
        SvgViz -> [ toSvg model ]
        ImgViz -> [ toImage model ]
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
              [ Html.div [] [Html.text "Advent of Code 2015" ]
              , Html.div [] [Html.text "Day 6: Probably a Fire Hazard" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2015/day/6" ] 
                [ Html.text "https://adventofcode.com/2015/day/6" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ] 
              [ 
                Html.div [] [ Html.text "The animation is sluggish and may crash some browsers." ]
              , Html.div [] [ Html.text "But it's also kind of cool." ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "16px" ]
              [ 
                Html.input 
                [ Html.Attributes.type_ "radio", onClick UseNoViz, Html.Attributes.checked (model.viz == NoViz) ] 
                []
              , Html.label [] [ Html.text "None" ]
              , Html.input 
                [ Html.Attributes.type_ "radio", onClick UseSvgViz, Html.Attributes.checked (model.viz == SvgViz) ] 
                []
              , Html.label [] [ Html.text "Svg" ]
              , Html.input 
                [ Html.Attributes.type_ "radio", onClick UseImgViz, Html.Attributes.checked (model.viz == ImgViz) ] 
                []
              , Html.label [] [ Html.text "Image" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Clear ] 
                [ Html.text "Clear"]
              -- , Html.button 
              --   [ Html.Attributes.style "width" "80px", onClick Solve ] 
              --   [ if model.paused then text "Solve" else text "Solve" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then text "Play" else text "Pause" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Step ] 
                [ Html.text "Step" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleBrightness, Html.Attributes.checked model.useBrightness ] 
                []
              , Html.label [] [ Html.text " Use brightness" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px" ] 
              [ 
                Html.div [] [ Html.text (String.fromInt count) ]
              , Html.div [] [ Html.text (String.fromInt model.instructionNumber ++ " of " ++ String.fromInt model.totalInstructionCount) ]
              , Html.label [] [ Html.text model.debug ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding" "10px" ] 
              elements ] ]
