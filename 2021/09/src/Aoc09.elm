module Aoc09 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time

delay : Float
delay = 10

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Position = (Int, Int)

type alias Cell = (Position, Int) 

type alias Basin = 
  { explorationPoints : List Position
  , color : String 
  , filledPoints : List Position }

type alias Model = 
  { floor : List (List Bool)
  , border : List Position
  , basins : List Basin
  , debug : String }

toDepthNumber : Char -> Int
toDepthNumber ch = ch |> String.fromChar |> String.toInt |> Maybe.withDefault 100

getAt : Int -> List a -> Maybe a
getAt n xs  = 
  if n < 0 then Nothing 
  else xs |> List.drop n |> List.head

lookup2d : Position -> List (List a) -> Maybe a 
lookup2d pos lst = 
  case pos of 
    (x, y) ->
      case getAt y lst of 
        Nothing -> Nothing 
        Just row -> 
          getAt x row 

lookup2default : Position -> List (List Int) -> Int 
lookup2default pos lst = 
  case pos of 
    (x, y) ->
      case getAt y lst of 
        Nothing -> 9
        Just row -> 
          getAt x row |> Maybe.withDefault 9

findNeighbourPositions : Position -> List Position 
findNeighbourPositions pos = 
  case pos of 
    (x, y) -> [ (x-1, y), (x+1, y), (x, y-1), (x, y+1) ]

findNeighbourDepths : Position -> List (List Int) -> List Int 
findNeighbourDepths pos depths = 
  pos 
  |> findNeighbourPositions 
  |> List.filterMap (\p -> lookup2d p depths) 

isLowPoint : Position -> List (List Int) -> Bool
isLowPoint pos depths = 
  case lookup2d pos depths of 
    Just depth -> 
      let 
        neighbours = findNeighbourDepths pos depths 
      in 
        neighbours |> List.all (\d -> depth < d) 
    Nothing -> False

findLowPoints : List (List Int) -> List Position 
findLowPoints depths = 
  depths 
  |> List.indexedMap (\y row -> row |> List.indexedMap (\x d -> if isLowPoint (x, y) depths then Just (x, y) else Nothing))
  |> List.concat
  |> List.filterMap identity

findLowPointsDebug : List (List Int) -> List String
findLowPointsDebug depths = 
  depths 
  |> List.indexedMap (\y row -> row |> List.indexedMap (\x d -> (d, findNeighbourDepths (x, y) depths)))
  |> List.concat
  |> List.map (\(d, ds) -> String.fromInt d ++ ":" ++ (ds |> List.map String.fromInt |> String.join "-"))

findLowPointsDebug2 : List (List Int) -> List String
findLowPointsDebug2 depths = 
  depths 
  |> List.indexedMap (\y row -> row |> List.indexedMap (\x d -> ((x,y), findNeighbourPositions (x, y))))
  |> List.concat
  |> List.map (\(p, ps) -> pos2str p ++ ":" ++ (ps |> List.map pos2str |> String.join " "))

posDepth2str pos depth = 
  case pos of 
    (x, y) -> "(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")->" ++ String.fromInt depth 

findLowPointsDebug3 : List (List Int) -> List String
findLowPointsDebug3 depths = 
  depths 
  |> List.indexedMap (\y row -> row |> List.indexedMap (\x d -> ((x,y), findNeighbourPositions (x, y))))
  |> List.concat
  |> List.map (\(p, ps) -> "P" ++ posDepth2str p (lookup2default p depths) ++ ":" ++ (ps |> List.map (\px -> posDepth2str px (lookup2default px depths)) |> String.join " "))


isBorderPoint : Position -> List (List Int) -> Bool
isBorderPoint pos depths = 
  case lookup2d pos depths of 
    Just depth -> 
      depth == 9
    Nothing -> False

findBorderPoints : List (List Int) -> List Position 
findBorderPoints depths = 
  depths 
  |> List.indexedMap (\y row -> row |> List.indexedMap (\x d -> if isBorderPoint (x, y) depths then Just (x, y) else Nothing))
  |> List.concat
  |> List.filterMap identity

createBasin : Position -> Basin 
createBasin pos = 
  { explorationPoints = [pos]
  , color = "red" 
  , filledPoints = [] }

pos2str pos = 
  case pos of 
    (x, y) -> "(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")"

init : () -> (Model, Cmd Msg)
init _ =
  let 
    input = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
    lines = String.lines input 
    numberLines = lines |> List.map (String.toList) |> List.map (List.map toDepthNumber)
    numberLinesStr = numberLines |> List.concatMap (List.map String.fromInt) |> String.join "."
    rowCount = List.length numberLines 
    colCount = 
        case List.head numberLines of 
          Nothing -> 0
          Just h -> List.length h 

    lowPoints = findLowPoints numberLines 
    lowPointStr = lowPoints |> List.map pos2str |> String.concat
    borderPoints = findBorderPoints numberLines
    basins = lowPoints |> List.map createBasin
    cells = numberLines |> List.map (List.map (\d -> d == 9))
    cellCount = cells |> List.concat |> List.length
    lowPointCount = lowPoints |> List.length
    foo = numberLines |> List.length

    bar = findLowPointsDebug3 numberLines |> String.join "|"
    model = { floor = cells
            , border = borderPoints
            , basins = basins
            , debug = "low:" ++ String.fromInt lowPointCount ++ ",{" ++ lowPointStr ++  "}," ++ "...[" ++ bar ++ "]"}
            --, debug = String.fromInt rowCount ++ "," ++ String.fromInt colCount ++ "," ++ String.fromInt foo ++ "," ++ String.fromInt cellCount ++ ",low:" ++ String.fromInt lowPointCount ++ ",{" ++ lowPointStr ++  "}," ++ numberLinesStr ++ "...[" ++ bar ++ "]"}
  in 
    (model, Cmd.none)


-- UPDATE

type Msg = Tick
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick -> (model, Cmd.none)
    --   let 
    --     updated = 
    --       case model.instructions of 
    --         [] -> model 
    --         inst :: rest -> 
    --           let 
    --             (pos, dir) = move inst (model.pos, model.dir)
    --           in 
    --             { pos = pos
    --             , dir = dir 
    --             , instructions = rest
    --             , visited = pos :: model.visited }
    --  in 
    --    (updated, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every delay (\_ -> Tick)

-- VIEW

toCellRect : String -> Position -> Svg Msg 
toCellRect color pos = 
  case pos of 
    (xVal, yVal) -> 
      let 
        w = 10 
        h = 10 
        xStr = String.fromInt (xVal * w)
        yStr = String.fromInt (yVal * h) 
      in 
        rect
          [ x xStr
          , y yStr
          , width (String.fromInt w)
          , height (String.fromInt h)
          , fill color
          ]
          []

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    borderRects = 
      model.border 
      |> List.map (\pos -> toCellRect "black" pos)
    rects = borderRects
  in 
    svg
      [ viewBox "0 0 400 400"
      , width "400"
      , height "400"
      ]
      rects


view : Model -> Html Msg
view model =
  let
    dim = 600
    s = toSvg model
    ptCount = model.border |> List.length
    ptCountStr = String.fromInt ptCount
  in 
    Html.table 
      []
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "40px"
              , Html.Attributes.style "padding" "20px"]
              [ Html.div [] [Html.text "Advent of Code 2021" ]
              , Html.div [] [Html.text "Day 9: Smoke Basin" ]] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "padding" "20px"] 
              [ Html.div [] [ s ] 
              , Html.div [] [ Html.text ptCountStr ]
              , Html.div [] [ Html.text model.debug ]
              ] ] ]
