module Main exposing (..)

{- Advent of Code 2022. Day 17: Pyroclastic Flow. -}

import Browser 
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
  Browser.element
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

type BasicShape = Dash | Cross | Angle | Bar | Box 

type alias Shape = (BasicShape, Int) 

-- type alias Rock = (Pos, Shape)

type Rock = Moving (Pos, Shape) | Stopped Shape

type alias Model = 
  { chamber : Array2D Char
  , rock : Rock
  , rocksFallen : Int 
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
    rock = ((2, 3), (Dash, 0))
  in 
    { chamber = chamber 
    , highestRock = 0
    , rocksFallen = 0
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
  (initModel Input, Cmd.none)

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

isForbidden : Array2D Char -> Pos -> Bool
isForbidden chamber (x, y) = 
  if x < 0 || x >= chamberWidth || y < 0 then True 
  else 
    case Array2D.get y x chamber of 
      Just '#' -> True 
      _ -> False

tryMove : Array2D Char -> Rock -> (Pos -> Pos) -> Result Rock Rock 
tryMove chamber rock moveFn = 
  case rock of 
    Stopped _ -> Err rock
    Moving (oldPos, shape) ->
      let 
        (x, y) = moveFn oldPos 
      in  
        if x >= 0 && x < chamberWidth && y >= 0 then 
          let 
            rockPositions = toRockPositions (Moving ((x, y), shape))
          in 
            if rockPositions |> List.any (isForbidden chamber) then 
              Err rock
            else 
              Ok <| Moving ((x, y), shape)
        else 
          Err rock

tryMoveRight : Array2D Char -> Rock -> Result Rock Rock  
tryMoveRight chamber rock =
  tryMove chamber rock (\(x, y) -> (x + 1, y))

tryMoveLeft : Array2D Char -> Rock -> Result Rock Rock  
tryMoveLeft chamber rock =
  tryMove chamber rock (\(x, y) -> (x - 1, y))


tryMoveDown : Array2D Char -> Rock -> Result Rock Rock  
tryMoveDown chamber rock = 
  tryMove chamber rock (\(x, y) -> (x, y - 1))

tryRotate : Array2D Char -> Rock -> Result Rock Rock  
tryRotate chamber rock = 
  case rock of 
    Stopped _ -> Err rock
    Moving (pos, (shape, rot)) ->
      case (shape, rot) of 
        (Dash, 0) -> 
          let 
            moveFn = \(x, y) -> (x + 2, y - 1)
            newRock = (Moving (pos, (Dash, 1)))
          in 
            case tryMove chamber newRock moveFn of 
              Ok moved -> Ok moved 
              Err _ -> Err rock
        (Dash, _) -> 
          let 
            moveFn = \(x, y) -> (x - 2, y + 1)
            newRock = (Moving (pos, (Dash, 0)))
          in 
            case tryMove chamber newRock moveFn of 
              Ok moved -> Ok moved 
              Err _ -> Err rock
        (Bar, 0) -> 
          let 
            moveFn = \(x, y) -> (x - 2, y + 1)
            newRock = (Moving (pos, (Bar, 1)))
          in 
            case tryMove chamber newRock moveFn of 
              Ok moved -> Ok moved 
              Err _ -> Err rock
        (Bar, _) -> 
          let 
            moveFn = \(x, y) -> (x + 2, y - 1)
            newRock = (Moving (pos, (Bar, 0)))
          in 
            case tryMove chamber newRock moveFn of 
              Ok moved -> Ok moved 
              Err _ -> Err rock
        (Angle, 0) -> 
          let 
            moveFn = \(x, y) -> (x, y)
            newRock = (Moving (pos, (Angle, 1)))
          in 
            case tryMove chamber newRock moveFn of 
              Ok moved -> Ok moved 
              Err _ -> Err rock
        (Angle, 1) -> 
          let 
            moveFn = \(x, y) -> (x, y)
            newRock = (Moving (pos, (Angle, 2)))
          in 
            case tryMove chamber newRock moveFn of 
              Ok moved -> Ok moved 
              Err _ -> Err rock
        (Angle, 2) -> 
          let 
            moveFn = \(x, y) -> (x, y)
            newRock = (Moving (pos, (Angle, 3)))
          in 
            case tryMove chamber newRock moveFn of 
              Ok moved -> Ok moved 
              Err _ -> Err rock
        (Angle, _) -> 
          let 
            moveFn = \(x, y) -> (x, y)
            newRock = (Moving (pos, (Angle, 0)))
          in 
            case tryMove chamber newRock moveFn of 
              Ok moved -> Ok moved 
              Err _ -> Err rock
        _ -> 
          Err rock
          
moveRock : Array2D Char -> Char -> Rock -> Result Rock Rock 
moveRock chamber move rock =
  case move of 
    '>' -> tryMoveRight chamber rock
    '<' -> tryMoveLeft chamber rock 
    'v' -> tryMoveDown chamber rock
    '^' -> 
      tryRotate chamber rock
      -- Ok rock
    _ -> 
      Err rock

moveRockJet : Array2D Char -> Char -> Rock -> Result Rock Rock 
moveRockJet chamber move rock =
  case move of 
    '>' -> tryMoveRight chamber rock
    '<' -> tryMoveLeft chamber rock
    _ -> Err rock

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
nextShape (shape, _) = 
  case shape of 
    Dash -> (Cross, 0) 
    Cross -> (Angle, 0) 
    Angle -> (Bar, 0) 
    Bar -> (Box, 0) 
    Box -> (Dash, 0)

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
        rock = 
          case moveRockJet model.chamber jetMove model.rock of 
            Ok moved -> moved 
            Err notMoved -> notMoved
      in 
        { model | message = jetMove |> String.fromChar, jetIndex = jetIndex, rock = rock, move = Fall }
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
        result = tryMoveDown model.chamber model.rock 
      in 
        case result of 
          Ok rock -> 
            { model | message = "v", rock = rock, move = Blow }
          Err rock -> 
            let 
              chamber = addRockPositions model.chamber (rock |> toRockPositions) 
              highestRock = findHighestRock chamber 
              rocksFallen = model.rocksFallen + 1
              paused = model.paused || model.rocksFallen == 2022
            in 
              { model | message = "v", rock = Stopped shape, chamber = chamber, move = Blow, highestRock = highestRock, rocksFallen = rocksFallen, paused = paused }
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
    rock = 
      case moveRock model.chamber move model.rock of 
        Ok moved -> moved 
        Err notMoved -> notMoved
  in 
    { model | rock = rock, message = move |> String.fromChar }

updateRotate : Model -> Model
updateRotate model = 
  case model.rock of 
    Moving (pos, shape) -> 
      let 
        result = tryRotate model.chamber model.rock 
      in 
        case result of 
          Ok rock -> 
            { model | message = "^", rock = rock }
          Err rock -> 
            model 
    Stopped _ -> 
      model

updateKey : Char -> Model -> Model
updateKey move model = 
  case model.rock of 
    Moving _ ->
      case move of 
        '>' -> updateSideways move model
        '<' -> updateSideways move model
        'v' -> updateFall model
        '^' -> updateRotate model
        _ -> model
    Stopped shape -> 
      let 
        y = model.highestRock + 3
      in 
        { model | rock = Moving ((2, y), nextShape shape)}

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
    ys = List.range 0 (highest  + 3)
    xs = List.range 0 (Array2D.columns chamber - 1)
    rows = ys |> List.map (toChamberRow rocks chamber xs)
  in 
    List.reverse ("+-------+" :: rows)

toRowElements : String -> List (Html Msg)
toRowElements rowText = 
  [ Html.text rowText, Html.br [] [] ]

toStyledHtmlElement : String -> Html Msg
toStyledHtmlElement symbol = 
  let
    cssClass = 
      case symbol of 
        "." -> "draw-empty adaptive"
        _ -> "draw adaptive"
  in
    Html.span [ Html.Attributes.class cssClass ]  [ Html.text symbol ]

toStyledRowElements : String -> List (Html Msg)
toStyledRowElements rowText = 
  let
    chars = rowText |> String.toList   
    elements = chars |> List.map (String.fromChar >> toStyledHtmlElement) 
  in
    List.append elements [ Html.br [] [] ]

isBox wh (x, y) = 
  if (Array2D.get y x wh == Just 'O' || Array2D.get y x wh == Just '[') then Just (x, y) else Nothing

movePoint : Pos -> Pos -> Pos
movePoint (x0, y0) (x1, y1) = 
  (x0 + x1, y0 + y1)

toRockPositions : Rock -> List Pos
toRockPositions rock = 
  case rock of 
    Stopped _ -> []
    Moving (pos, (shape, rot)) -> 
      case shape of 
        Dash -> 
          if rot == 0 then 
            [(0, 0), (1, 0), (2, 0), (3, 0)] |> List.map (movePoint pos)
          else 
            [(0, 0), (0, 1), (0, 2), (0, 3)] |> List.map (movePoint pos)
        Cross -> 
          [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)] |> List.map (movePoint pos)
        Angle -> 
          case rot of 
            0 -> 
              [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)] |> List.map (movePoint pos)
            1 -> 
              [(0, 2), (1, 2), (2, 0), (2, 1), (2, 2)] |> List.map (movePoint pos)
            2 -> 
              [(2, 2), (1, 2), (0, 0), (0, 1), (0, 2)] |> List.map (movePoint pos)
            _ -> 
              [(2, 0), (1, 0), (0, 0), (0, 1), (0, 2)] |> List.map (movePoint pos)
        Bar -> 
          if rot == 0 then 
            [(0, 0), (0, 1), (0, 2), (0, 3)] |> List.map (movePoint pos)
          else 
            [(0, 0), (1, 0), (2, 0), (3, 0)] |> List.map (movePoint pos)
        Box -> [(0, 0), (1, 0), (0, 1), (1, 1)] |> List.map (movePoint pos)

view : Model -> Html Msg
view model =
  let
    rockPositions = model.rock |> toRockPositions |> Set.fromList 
    rows = toChamberRows rockPositions model.highestRock model.chamber
    elements = rows |> List.concatMap (toStyledRowElements)
    rockPosStr = 
      case model.rock of 
        Moving ((x, y), shape) -> "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++  ")"
        Stopped _ -> "_"
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
              [ Html.div [] [Html.text "Advent of Code 2022" ]
              , Html.div [] [Html.text "Day 17: Pyroclastic Flow" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2022/day/17" ] 
                [ text "https://adventofcode.com/2022/day/17" ]
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
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "1.2rem"
              , Html.Attributes.style "padding-top" "10px" ] 
              [ 
                Html.div [] [ Html.text ("Rocks fallen: " ++ (String.fromInt model.rocksFallen |> String.padLeft 4 ' ')) ]
              , Html.div [] [ Html.text ("Tower height: " ++ (String.fromInt model.highestRock |> String.padLeft 4 ' ')) ]
              , Html.div [] [ Html.text model.message ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" "1.4rem"
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [
                  Html.Attributes.align "center" 
                , Html.Attributes.style "max-width" "100%"
                ] elements
              ] ] ]
