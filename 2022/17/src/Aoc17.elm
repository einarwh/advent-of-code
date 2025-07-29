module Aoc17 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Set exposing (Set)
import Array2D exposing (Array2D)
import Html exposing (text)
import Time
import Keyboard exposing (RawKey)
import Keyboard.Arrows exposing (..)

defaultTickInterval : Float
defaultTickInterval = 10

chamberWidth : Int 
chamberWidth = 7 

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

type Dir = N | W | S | E

type Move = Blow | Fall

type Visit = Vertical | Horizontal | Both 

type Cell = Highlight Char | Plain Char 

type Shape = Dash | Cross | Angle | Bar | Box 

-- type alias Rock = (Pos, Shape)

type Rock = Moving (Pos, Shape) | Stopped Shape

type alias Model = 
  { chamber : Array2D Char
  , rock : Rock
  , highestRock : Int
  , dataSource : DataSource
  , move : Move
  , jetPattern : Array Char 
  , jetIndex : Int 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , message : String }

sample : String
sample = """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"""

input : String
input = """>>>><<><>><<<<>>>><<>>><>><><<>>>><<<<>>><<<>>>><<<>><<>>><<<<>>><<>>>><><><<>>>><<<>>><<<><<>>><<<<>><<<<>>>><<<<>>>><<<>>>><<<><><<<>>><<<>>><<<<>>>><<<<>>>><>><<>><<<>><<>><>>><<<<>><<<<><<<<><<>>><<<>><<<<>><<<><>>><<<<>>>><<<>>><<<>><>>><<<<><>>>><<<><<<>>><<<><<>><<<<><<<>>>><<<<>><<<<>>><<<<><<<>>><<><>>><>>><>>>><><<<<>>><<>>><><>>><<<>>><<<<>>><><<<<>>><<<<>>>><<><>>><>><<>><<>>>><<<<>><<<<><<<<>>><>>>><<>>><<><<<>>>><<>>><<<<>><<<>>><<<<>>>><<>>>><<>>><<<<>>>><<>>>><<><>><<<<>>><<<<>><<<><<<><<<><<>>><<<<>>>><<<<>>>><<<><<<>><<>>><>><<>><>>>><>><<<<>>><<>>><<<>>><<>>>><>>><>>><<><<<>>>><<<<>>><<<<>>><<>>>><<<<>><<>>>><<<>>><>>><<<><>>><>>>><<>>>><<>><><<<<><<<>>><<>><<><<>>>><<>>>><><<<<>>>><<<<><<<><>>><<<>><<<>>>><>><<<<>>><><<><<<<>><<<<>><<<<>>><><<<><<<>>><<<<>>><>><<<<><><<<>>>><<<<><>>>><<<>>>><<><<<><>>><<<><<<<><>>><<<>><<<>><<<><><<<>>><<><<<<>>><<<>>>><<>><>><<<>>><>>>><<<<><>>><<>>>><>>>><<>>>><>><><>>><<<>>><<<>><<>>>><<<>>>><><<>>>><<>>><<<<>>><<<<>>><<<<>><<<>><<<>>><<>><<<>><>><<<>><<>>><<>>>><<<>>><<>>>><>>>><<<<>><<<><<><<<>>><<<><<<<>>>><<<<><<<>>>><<<>>>><<<>>>><>>>><>><<<<>><<<<>>>><<<<>><<<>><><>>>><<>>>><<>><<<>>>><<<<>><<<<>><<>>>><<>><<<<>>><<<<><<<<>>><>>><><<<>>><>><<>>>><<<<><<>><<>>><<<<>>>><<>>><>>>><<><<>>><<>>><<><<>><><<<<>><<<>>><>><<<>>><><<<<>>>><>>>><<<>>><<<>><<<>>><<<<>>><>>>><<<>>><<<>><<<>>>><<>>>><>><>><<<>><<<<><<<<>><<<<>>><>><<<<>><<>><><<>>><<<>>>><<>>>><<<>>><<>>><>><>><<><<<>>><<<><<<>><><><<<>><>>>><<>>>><<<<>>><<<><>>><<<><<<<>><<<><<>>><<<<>><><<<>>>><<>>>><>>>><<>>>><<<<>>><><>>>><>><>>><<<<><<<>><<<<><<<>>>><<<<>>>><>>>><>>><<<<><<<>>>><<<>><<<><<<><<<<><<<<>>>><<><<>>>><<<>><<<<>>><<<>>><><<<><<<>>>><<<<>>><<<><>>>><<<<>><<<<>>>><>><>>>><<<<>>>><<<>>>><<>>><<<>>>><>>><<<>>><<<>>>><>>>><<<>>><<<>><><<<>>>><<<>><<<<>>><>>><<<>>>><<<<>>>><<<><<<><<>><>>><<<>>><<<><<<>><<><<<<>>>><<<<><<>>><>><>><><<<>>>><<<<>><<<<>><<<<>>>><><>>><<<>><<>>><<<<>>>><<<><<<><<<>>>><<<><<>>><<<<>>>><<><>>>><<<>>><<<<>>><<<<>><<<>><<<<><<<<>><>>><>>>><>><<<>>>><><<<<>>><>>><<>>>><<>>><<<<>>>><<<><<><<<<>>><><<>>><>>><<<>><<<>><<>>><<<>>>><<<<>>><<<<><<>>>><<<>>><><>>>><>>><><<<<><<>>>><><<<>>><<>>><><>>><<<<>>>><<<<>>>><<<>>><<<<>>><<<>>><><>><<<><<>>><<>>><<>>><<<<>>>><><<<>>>><<>><<>>>><><<<>>><<><<>>><<<<>>>><<<>><<<><<><>>>><<<>><>>>><<<<>>>><<>>>><>>><>>><<<>><<<<>>><<<><<<<>><<<<>><<<>><><<>>>><<><<<>>>><<<<>><<<<>>><<<>>>><<<>>><<<><<<<>><<<<>>>><<><<>>><>>><<<>><>><<<<>>>><<<>>>><<<><<>>>><<>>><<<<><<<<>>><<<>>><<>><<<<>><<<<><<<>><<<>><>><<>><>>><<<<>><<>>>><<>><<>><<>><<<<>>>><<>><<<<><<<>><><><<>><<<><>>>><<<>>><>>><><>><<<>>>><<>><<>><<>>><<<<><<><>><<<<>>>><>>>><<>>><><<<<>><<<<><>><<<>><<><>>><<>>><<>>>><<<>><<<>>>><<<<>>><<><<<>>><<>>>><<<>>><<<<>><<>>><<<<>><>>><<<<><<<>>>><>><<><><<<<>>>><>>>><<>>>><<<>>><<<>><>>><<>>><<<>><><<<>>>><>>><>>><>><<<<>>>><<><<<<><<>><<>><<><>><<>>><<<>>><<<>>>><<<<>><><<><><<<><<<>>><<>><>>><<>>><<<<>><<<>><<>>>><>>>><<<>>>><>>><<<>>><<<<><<>>><<<<>>><<>><<>>>><>>><<>><<>>><<<><<>>><<>>><<<>>><<<>><>><<<>><<<<>>><>>>><<<<>>><>>><<<>><<<<>>>><>>><<><<<<>><><<><<><>>><><>><<<<>><<<<><>>>><<>>><<<<>>><>><<>><>>><<<<>><<>>>><><<<<>>>><<<<>><<<>>><><<><<>>>><<>><>>><<>><>><><<><<<>><<<<>>><><<>><>><<>>>><<>><>><<<<>>>><<<<>><<><>><<<<>>><<<>>>><<<>><<><<>>><<>>><<<>>>><<><<<<>>><<><<<<>>>><<<<><<<>>><><<>>><<<<><>><<>><<<<>>><<<<><<<>>>><<<>>><<<><<<>><>><<><<<>>>><<>><<><>>>><<>>>><<<<><<<>>>><<>>>><<><<<><<<>>>><<<<>>>><<><<<><<>><<<<>>><<<>>><<>>><<<>>>><>>>><>>><>><<<>>>><<<>>><<><<<>><<<><<<><<<<><>><>>><<<>>><<<<>>><<<><<<>><>><<>>><<>>>><<<<>>>><<>>><<<><<<>>><<<<>><><<>>>><<>>>><<<<>>>><<<>><<<>>>><>>><<<>><><<<<>><<>>><<>>><>>><<<<>>>><<><<<<>>>><<<<><<<<>>>><><<>><<<<>>>><<<<><<<>><<<><>>>><<<><<<>>>><<>>><><<>>><<>>>><<<<>><<<>><<<<>><<<<>><>><<<<>>><>><<<<><<><>>><><<<<>>><>><>>><<<>><<<>><>>>><>>>><<>><<<<>><<<>><<<>>><<<>>>><><<><<><<<<><<>>><<>>>><<<>><<<>><>>><<<<>><<><>>><>>><<<<>>>><>>>><>><<<>><<>><<<>><<<<>>>><<<>>>><<>>><<>><<<><<<>>>><<<>><>>><<><>>>><<<>>>><>><>>><><>>>><<<<>><>><<<>>><<<><>><<<<>>><>>>><<<><<<<><<<<>><<>>><><<<>>><<>>><<<>>>><<<<><<<<><>>><<<<>><>>><<<<>>>><>>><<<<>>><>>>><>>>><<<<>>><<<<>>>><<<<>>><>>><<>>>><<<<>><>>><<<>><<<<>>>><<<<>>>><<<><><<<><><<><>><<<><<>>><<<><<>>><<<<>>>><<>>>><><<>>>><<>><<<<>><>>>><<<<>>><<<>>>><>><<<<>>>><<<>>>><<<>>><<<<><<>><<<>>>><>>>><>>>><<>><<>>>><><>>><<><<>><<<<>>><><<<<><<>>><<<>>><<<>>>><<<<>><<<><>><<<>>>><<<>>><<<><<<<>>>><<<<><><<<>>>><<<>>><<<<>>>><>><<<<>>><<<>>>><<>>><>><<><><>>><<<>>>><<<<><<>>>><<<>>><<<>>>><<>><<<>>><<<<>>>><<>>>><<>>>><>><<<>>>><<>>>><<>>><<<>>><<>><<><<<<><<>>>><<<<>>><><<<><<<<>>><<<>>>><<>>>><><<>>><<>>><<<>>>><><>>><<<><<<>><<<<><>>><<<<>><<<>>>><<<>>>><<<>><<>><<<>><<<<>><<>><<>>>><<<<><<<>><>><><<><>>>><<<<>>><<<>>>><<<<>>><<<><<>><<<<>>><<<<>>>><<<<>>>><<<>>>><<<<>><<<<>>><<>>><>>><<<><<<<>>><>>><<><<<><<<<>><<<>>>><<<>>>><<<<>><<<>><>>><<<<>>>><><><<>>>><<><<<>>><<<>><<>><>>><<>><<<>><<<>>>><<<>>><<>><<<>><<<<>>>><<<<>>>><<<>>>><><><>>><<>>><<>>>><<>>><<<<>>>><<>>><<>><<<>>><<<>>>><<>>><>>>><>><<>>><<<>><<<><>>>><>>><<>>>><>><<<<><<<>>>><<<>><<<>><<<><><><<<<><>>><<<>>>><<<<>><<<<>>><>><<<>>><<<><<<<>>><<>><<>>>><><>><<><<><<<>><<><>>><<><<>>><<<<>>>><<<<><<<>><<><<<<>><<<<>>><<>>><<>>>><<<<>>>><<<>>>><<>>><<>>>><<<<>><>>><<><<><<>><<<><>><<<<><<<>>>><<>>><<<>>><<>>>><<<<>>>><<>>><<<<>><<<>>>><<<>>>><<<<>>>><<<>>>><<<<>>>><>><<<<>>><>>>><>>><>><<>>><<<<>>>><<<>>>><>>>><<<><<<<>><<<>>>><><<>>>><>><<<>>><<<>>>><<><<>>>><>>><>>><>>><>><<<><>>>><<<<>><<>><<<>><>>>><<<<>><<<>>>><<<>><<<<><<<<>>><<<<>>><<<<>>><<<>><<<<>>>><<<<><<<<>>>><>>>><<<><<<<>>>><<>><<<>>><<<>>>><>>><<<<>>>><<>>><>><<><<<>><<<>><<<>>><>>>><<<<>>><<>>>><<<<><<<<>>>><<<<>>>><<<<>>>><<<>><<>><>>><<<<>>>><<<<>>><><<<<>>>><>>><<>>>><<>>>><><<<<>>>><>><>>>><<>>>><>><<>>><<<>><<>>><<><<<<>>>><<<<>><><<<>>><<<>><>><<<<>><>>><<<<>>>><<<<>>>><><><<<>>>><<<>>><<<<><<<>>>><>>><<>>><<<<><>>>><>>><<<<>>>><>>><<<<>>>><<<<><<<<>>><<<<>>>><<<>>>><<<>><<<<>>>><>><<<<>>><>>><<<<>>>><>><>><<<<>><<<>>><><>>><<><<<<><<>>>><>>><>>><>><>>><<<><><>>>><>>>><<>><<<<>><<<>><<<<>><>>>><<<<>>>><<<>>><<>>>><><<>><<<>><<<>>>><<><>>><<>><<>>>><<<>>><<>>><<>>><>><<<>>><<>>><<<<>>><>>>><<<><><<<<>><<<<>>><>>>><><>>>><<<>>>><<<>>><<<<>>>><<<<>>>><<>><<<>><>>><>>>><<<<>><<>>><<>><<<>>>><<<>><<<<><<>>>><>><>><<<<>>><<<><<>>>><<<<>>><<<><>>><<<><<<>>>><<><>>>><<<<><>>><><<<<>>>><<>>>><>>>><<>>>><>>>><>><<<<>>>><>><>>>><<>>><<<>>>><<<>><><<<<><<<<>>>><<>>>><<<<>>><<>>>><<>><<<>>>><<>><>>><<<<>>><>>>><><<<><<<>>><<<<><><<<>>><><<>>><<><<<<>>>><>>>><<<>>>><<>>><>>>><<<<>>>><<<>><>><<<><>><<<>>><>>><>>><><<<>>>><><<<<>>><<<<>>>><<<<>>><<<<>>><>>>><<<<>>><<>><<<<>><>>><<><<<><<<><<<<>>>><<<><<<<>>><<>>>><<>><<<<>>>><<<<>>>><>>><<<<>>>><>>>><<>>><><<<>><>><>>>><><>><<<<>><<<<>>>><<><<<<><<>>><>>>><<><<<<>><><<<>>><<<><>>><<><<>>><<<<>>><<<>>><<<<><<<>>>><<<<><<>>>><<<><<>>><<<><><<<<>>>><<<>>><>>><<><<<>>><<<>><<>>><>><<>><>>><>><>><>>>><<<>><<<<><>>>><<<>>><<<>><<<><><<>><<<>>><<<<>>>><>><<<><<<><<<<><<>><>><<<<>>>><<<>>><<><<<>>><<<><>><>>><>>>><><>>><<<>><><<<<>><<<><<<<>>><<<<>>><<<>>>><<>><<>>>><<><<<>>><<<<>><<>>>><>>>><<<<>><<>><<<>>>><<<>>><<<>><<<<><<<><<>>>><<<>>>><<<>>>><<>>><<<<>>><<>>>><<>>><><>>>><<<>>>><<<><<>>><<<><<<<>><<<>><<>>>><<>>>><<<>>>><<>><>>>><<<<>><>><<>>>><<<<>>><<<<>>>><<<<>>><<><<<>>>><<<<>>><<>><<><<><<>><<<<><<<<>>><>>><<>>><><<<<>><>>><<<<><<<<><<<>>>><<><<<>>>><<<<>>>><>>><>><><><<<<><>><<<<>><<<<>>><<>>>><<<><<<<><<<<>>><<>><<>>>><>><<>><<><<>>>><>>>><><<<<>>>><<<><>>>><<<<>><<>>><<<<><<<<>>><<<<><>><<<<>>>><<>>><<>>>><<<><<<>><>>><<<>>>><>><<<>><><<<>>>><><>>>><>>><<<>>>><>>>><>>>><<<><<>>><<><<<<>>><>><<><<<><<<>>><>>><>>><<>><<><>><><>>><<><<<<><<>>>><<<>>><>>><<<<>>>><<<>>><<<><<<>>><<>><<<>><>><<>><<<><<<<>><<>>>><<<<>>>><<><<>>>><<<<><>>>><<<<><><>>><<<>><<<<><<>>>><<<>><<<>>><<<<><>><<<><<>>>><<<<>>><<<>>><>>>><<><<>>><<>><<<><<<<>>><>>><<>><>>>><>><<<>>>><><<<<>>>><<<>>>><>><<<>>><>>><<>>>><<<<>>>><<<>>><<><<<<><<<<>>><>>><><<<<>>>><<<><<<>>>><<<<>><<>>><<<<>>>><<>>><<<<>>><>><<<>>>><>>><<<<><<<<>>>><<<<>><<><>>>><<<<>>><<<<>><<>>>><><<<>><>>>><<<><>><<<>>>><>><<>>>><<<>>><>><<<>><>><<<><<>>><<<<>>><<<><<<>><><<<>><><<<<>>><<<<><<<>><<>>><<>><<<><<<><<<<>>>><<<><<<><<<<><>>><><<<>>><<<<>>><<<>>><<<>>>><<<<>>><<<><<>><>>>><<>><<<>><<<<>>><>><<>><<>>>><>><>>><<<<><<<><>><>>><><<>><<<><<>>>><>><<<>><<<<><<<>>>><<>><<<<>><<<<><<<>>><<<<><<<<>><<>><<<>>>><<>>>><<<>>><<><<>>>><<<><<><<<<><<<>><<>>><<<<>><<>><<<>>>><<>>>><<<<>>><<<<>>><<<<>><>><<<<>>>><<>>>><<<<><<>>>><<><><><<<<>><<<>><><<<<>>><>>>><<><<<<>><<<<>>><><<<<>><<<>><<>><<<<><>><<<>>>><<<><<<<>>>><>>>><<<<>>><<<>>><<<<><<<>>><<<<><<>><<>>>><>><<<<><<<<>>>><<<<>>>><<>>>><<<>>><<>><<<>>><<<<>>>><<<<><>>><<>><<<<>><><<<<>>><>>><<<<>><>><<><<>>><>>><<<<>><><<<>><>><<<>><<<<>><<>><<<>><>><<>><<<>><>>>><<<<>>>><>>>><<<>><<<>>><<>><>><<<>>>><<<<>>>><>><>><<><<<<>>>><<><>>>><>>><<<>>>><<<<><<<<>><><<<<>>>><>>>><<>><<<<><<>>><<<<><>><><<<>>><<>>><>>>><><<<<><<<<>>><>>><>>>><<<>>>><>>>><<><<<>>><<>><<<>>><>><>><<<<>><<>>>><<<><><<<<><<>>><<<<><<<><<<>><>>><<<>>>><>><<<><<<<>>><<<<>>><<>><<>><>>>><<<<>>>><>><>>><>>>><<<<>><<<<>>><<<><<<>>><><>>>><<<>>><>>>><<<>>>><<<<>><<<<><<<<>>>><<>>>><>>><<<>>><<<>><<<<>><<<>><>><><>>><<><>>>><<<<>><<>><<>>><<>>>><<<<>>>><<<<><><<<<>><<<>>><<><<><<<<>>>><<>>>><<<>>>><>>><<>>>><><<<<>><<>>><<>>>><<><<>><>><<<>>><<<>>>><>><>><>>><<<<><<<>><<<<>><<<<>>><><>>><>><<>><>><>>><><<>>><>>><<<>>><>>><>><<<<>>>><<>>><>><<<>>>><><<>>>><<<>>>><>>>><<<>><<<>><<>>><<<>><<<<><<<><<<<>><<>>><>>><<<>>>><<<>><<<>><<<><<>><<<<><<<>><<<<><<<<><<<<><<<<><>>>><<<>>>><<<<>>>><<<><>>><<>><<<<>>>><>><<<<><>>>><<<<>><<><<<><>><>>>><<<><>>>><<<><<<><<<<>><<<><<<<><<<>>><<>>>><<>>><<<<><<<<><><<<<><<<>><>>><><>>><><<<<>>><<>>>><<<<>>><<<<>>>><<<>><>>><<<<>><>>><<<><<<><<>><<<><<>><<>>><<><>>><<<<><>>>><<<>><<<>>>><<<<>>><<<<>>><<<<>>><<>>>><>><>><>><<>><<<>>><>>><<><<>>><>>><<<>>>><<<><>><>><<>>><<<>><>>>><<<<>><<<<>><<<<><<<>><<>>><>>>><<<<>><>>><<<<><<<<>><>>><<>>><<<<>>><<<>>><><<<<>"""

read : DataSource -> String
read dataSource = 
  case dataSource of 
    Input -> input
    Sample -> sample

initModel : DataSource -> Model 
initModel dataSource = 
  let 
    data = read dataSource
    chamber = Array2D.repeat (2022*4) 7 '.'
    rock = ((2, 3), Dash)
  in 
    { chamber = chamber 
    , highestRock = 0
    , rock = Moving rock
    , dataSource = dataSource
    , move = Blow
    , jetPattern = data |> String.toList |> Array.fromList  
    , jetIndex = 0
    , paused = True
    , finished = False  
    , tickInterval = defaultTickInterval 
    , message = "?" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel Sample, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | Faster 
  | Slower 
  | Clear 
  | UseInput
  | UseSample
  | KeyDown RawKey

getAllPositions : Array2D Char -> List Pos
getAllPositions board = 
  let
    ys = List.range 0 (Array2D.rows board - 1)
    xs = List.range 0 (Array2D.columns board - 1)
  in 
    ys |> List.concatMap (\y -> xs |> List.map (\x -> (x, y)))

tryMoveRight : Rock -> Rock  
tryMoveRight rock =
  case rock of 
    Stopped _ -> rock 
    Moving ((x, y), shape) -> 
      case shape of 
        Dash -> if x + 4 < chamberWidth then Moving ((x + 1, y), Dash) else Moving ((x, y), shape)
        Cross -> if x + 3 < chamberWidth then Moving ((x + 1, y), Cross) else Moving ((x, y), shape)
        Angle -> if x + 3 < chamberWidth then Moving ((x + 1, y), Angle) else Moving ((x, y), shape)
        Bar -> if x + 1 < chamberWidth then Moving ((x + 1, y), Bar) else Moving ((x, y), shape)
        Box -> if x + 2 < chamberWidth then Moving ((x + 1, y), Box) else Moving ((x, y), shape)

tryMoveLeft : Rock -> Rock  
tryMoveLeft rock =
  case rock of 
    Stopped _ -> rock 
    Moving ((x, y), shape) -> 
      case shape of 
        Dash -> if x > 0 then Moving ((x - 1, y), Dash) else Moving ((x, y), shape)
        Cross -> if x > 0 then Moving ((x - 1, y), Cross) else Moving ((x, y), shape)
        Angle -> if x > 0 then Moving ((x - 1, y), Angle) else Moving ((x, y), shape)
        Bar -> if x > 0 then Moving ((x - 1, y), Bar) else Moving ((x, y), shape)
        Box -> if x > 0 then Moving ((x - 1, y), Box) else Moving ((x, y), shape)

tryMoveDown : Rock -> Rock  
tryMoveDown rock = 
  case rock of 
    Stopped _ -> rock 
    Moving ((x, y), shape) -> 
      case shape of 
        Dash -> if y > 0 then Moving ((x, y - 1), Dash) else Moving ((x, y), shape)
        Cross -> if y > 0 then Moving ((x, y - 1), Cross) else Moving ((x, y), shape)
        Angle -> if y > 0 then Moving ((x, y - 1), Angle) else Moving ((x, y), shape)
        Bar -> if y > 0 then Moving ((x, y - 1), Bar) else Moving ((x, y), shape)
        Box -> if y > 0 then Moving ((x, y - 1), Box) else Moving ((x, y), shape)

moveRock : Char -> Rock -> Rock 
moveRock move rock =
  case move of 
    '>' -> tryMoveRight rock
    '<' -> tryMoveLeft rock 
    'v' -> tryMoveDown rock
    _ -> rock

moveRockJet : Char -> Rock -> Rock 
moveRockJet move rock =
  case move of 
    '>' -> tryMoveRight rock
    '<' -> tryMoveLeft rock
    _ -> rock

addRockPositions : Array2D Char -> List Pos -> Array2D Char 
addRockPositions chamber positions = 
  case positions of 
    [] -> chamber 
    (x, y) :: rest -> 
      addRockPositions (Array2D.set y x '#' chamber) rest 

nextMove : Move -> Move 
nextMove move = 
  case move of 
    Blow -> Fall 
    Fall -> Blow

nextShape : Shape -> Shape
nextShape shape = 
  case shape of 
    Dash -> Cross 
    Cross -> Angle 
    Angle -> Bar 
    Bar -> Box 
    Box -> Dash

updateClear : Model -> Model
updateClear model = 
  initModel model.dataSource

updateBlow : Model -> Model
updateBlow model = 
  case model.rock of 
    Moving _ -> 
      let 
        jetMove = model.jetPattern |> Array.get model.jetIndex |> Maybe.withDefault ' '
        jetIndex = (model.jetIndex + 1) |> modBy (Array.length model.jetPattern)
        r = moveRockJet jetMove model.rock 
      in 
        { model | message = jetMove |> String.fromChar, jetIndex = jetIndex, rock = r, move = Fall }
    Stopped _ -> 
      model

findHighestRock : Array2D Char -> Int 
findHighestRock chamber = 
  let 
    positions = getAllPositions chamber 
    index = positions |> List.filterMap (\(x, y) -> if Array2D.get y x chamber == Just '#' then Just y else Nothing) |> List.maximum |> Maybe.withDefault 0 
  in 
    index + 1

updateFall : Model -> Model
updateFall model = 
  case model.rock of 
    Moving (pos, shape) -> 
      let 
        r = tryMoveDown model.rock 
      in 
        if r == model.rock then 
          let 
            chamber = addRockPositions model.chamber (r |> toRockPositions) 
            highestRock = findHighestRock chamber 
          in 
            { model | rock = Stopped shape, chamber = chamber, move = Blow, highestRock = highestRock }
        else 
          { model | message = "v", rock = r, move = Blow }
    Stopped _ -> 
      model

updateStep : Model -> Model
updateStep model = 
  case model.rock of 
    Moving _ -> 
      case model.move of 
        Blow -> updateBlow model 
        Fall -> updateFall model 
    Stopped shape -> 
      let 
        y = model.highestRock + 3
      in 
        { model | rock = Moving ((2, y), nextShape shape)}

updateSideways : Char -> Model -> Model
updateSideways move model = 
  let 
    rock = moveRock move model.rock
  in 
    { model | rock = rock }

updateKey : Char -> Model -> Model
updateKey move model = 
  case move of 
    '>' -> updateSideways move model
    '<' -> updateSideways move model
    'v' -> updateFall model
    _ -> model

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  { model | paused = not model.paused }

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
    UseInput -> 
      (initModel Input, Cmd.none)
    UseSample -> 
      (initModel Sample, Cmd.none)
    KeyDown rawKey ->
      case Keyboard.Arrows.arrowKey rawKey of 
        Just Keyboard.ArrowUp -> 
          (updateKey '^' model, Cmd.none)
        Just Keyboard.ArrowLeft -> 
          (updateKey '<' model, Cmd.none)
        Just Keyboard.ArrowDown -> 
          (updateKey 'v' model, Cmd.none)
        Just Keyboard.ArrowRight -> 
          (updateKey '>' model, Cmd.none)
        Just (Keyboard.Character "W") -> 
          (updateKey '^' model, Cmd.none)
        Just (Keyboard.Character "A") -> 
          (updateKey '<' model, Cmd.none)
        Just (Keyboard.Character "S") -> 
          (updateKey 'v' model, Cmd.none)
        Just (Keyboard.Character "D") -> 
          (updateKey '>' model, Cmd.none)
        _ -> 
          (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused then Sub.none else Time.every model.tickInterval (\_ -> Tick)
    keySub = Keyboard.downs KeyDown
  in 
    Sub.batch [ tickSub, keySub ] 

-- VIEW

toChamberRow : Set Pos -> Array2D Char -> List Int -> Int -> String 
toChamberRow rocks chamber xs y = 
  let 
    s = xs |> List.filterMap (\x -> if Set.member (x, y) rocks then Just '@' else (Array2D.get y x chamber)) |> String.fromList
  in 
    "|" ++ s ++ "|"

toChamberRows : Set Pos -> Int -> Array2D Char -> List String 
toChamberRows rocks highest chamber = 
  let
    ys = List.range 0 (highest  + 7)
    xs = List.range 0 (Array2D.columns chamber - 1)
    rows = ys |> List.map (toChamberRow rocks chamber xs)
  in 
    List.reverse ("+-------+" :: rows)

toRowElements : String -> List (Html Msg)
toRowElements rowText = 
  [ Html.text rowText, Html.br [] [] ]

isBox wh (x, y) = 
  if (Array2D.get y x wh == Just 'O' || Array2D.get y x wh == Just '[') then Just (x, y) else Nothing

movePoint : Pos -> Pos -> Pos
movePoint (x0, y0) (x1, y1) = 
  (x0 + x1, y0 + y1)

toRockPositions : Rock -> List Pos
toRockPositions rock = 
  case rock of 
    Stopped _ -> []
    Moving (pos, shape) -> 
      case shape of 
        Dash -> [(0, 0), (1, 0), (2, 0), (3, 0)] |> List.map (movePoint pos)
        Cross -> [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)] |> List.map (movePoint pos)
        Angle -> [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)] |> List.map (movePoint pos)
        Bar -> [(0, 0), (0, 1), (0, 2), (0, 3)] |> List.map (movePoint pos)
        Box -> [(0, 0), (1, 0), (0, 1), (1, 1)] |> List.map (movePoint pos)

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2022 | Day 17: Pyroclastic Flow"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    textFontSize = "20px"
    rockPositions = model.rock |> toRockPositions |> Set.fromList 
    rows = toChamberRows rockPositions model.highestRock model.chamber
    elements = rows |> List.concatMap (toRowElements)
    rockPosStr = 
      case model.rock of 
        Moving ((x, y), shape) -> "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++  ")"
        Stopped _ -> "_"
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
              [ Html.div [] [Html.text "Advent of Code 2022" ]
              , Html.div [] [Html.text "Day 17: Pyroclastic Flow" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ 
                Html.text " ["
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
              , Html.text "] " ] 
          ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2024/day/15" ] 
                [ text "https://adventofcode.com/2024/day/15" ]
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
                [ text "Reset" ]
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
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding-top" "10px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] [ Html.text (String.fromInt model.highestRock) ]
              , Html.div [] [ Html.text rockPosStr ]
              , Html.div [] [ Html.text model.message ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" textFontSize
              , Html.Attributes.style "padding" "10px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] elements
              ] ] ]
