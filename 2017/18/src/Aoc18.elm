module Aoc18 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Html exposing (text)
import Array exposing (Array)
import Dict exposing (Dict)
import Queue exposing (Queue)
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

type alias Name = String

type Value = Reg Name | Num Int 

type Instruction = 
  Snd Name 
  | Rcv Name 
  | Set (Name, Value) 
  | Add (Name, Value) 
  | Mul (Name, Value) 
  | Mod (Name, Value) 
  | Jgz (Value, Value) 

type State = 
  Ready 
  | Running 
  | Waiting 
  | Terminated 

type alias Program = 
  { pid : Int 
  , ptr : Int
  , state : State
  , sent : Int
  , sound : Maybe Int
  , registers : Dict String Int 
  , inbox : Queue Int 
  , outbox : Queue Int }

type alias Model = 
  { program0 : Program 
  , program1 : Program 
  , duet : Bool
  , instructions : Array Instruction 
  , result : Maybe Int 
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , debug : String }

input : String
input = """set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 680
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19"""

orElseWith : (() -> Maybe a) -> Maybe a -> Maybe a 
orElseWith ifNoneThunk maybe = 
  case maybe of 
    Just a -> Just a 
    Nothing -> ifNoneThunk()

parseValue : String -> Value 
parseValue s = 
  case String.toInt s of 
    Just n -> Num n 
    _ -> Reg s 

parseSnd : List String -> Maybe Instruction
parseSnd parts = 
  case parts of 
    ["snd", r] -> Just (Snd r)
    _ -> Nothing 

parseRcv : List String -> Maybe Instruction
parseRcv parts = 
  case parts of 
    ["rcv", r] -> Just (Rcv r)
    _ -> Nothing 

parseSet : List String -> Maybe Instruction
parseSet parts = 
  case parts of 
    ["set", r, s] -> Just (Set (r, parseValue s))
    _ -> Nothing 

parseAdd : List String -> Maybe Instruction
parseAdd parts = 
  case parts of 
    ["add", r, s] -> Just (Add (r, parseValue s))
    _ -> Nothing 

parseMul : List String -> Maybe Instruction
parseMul parts = 
  case parts of 
    ["mul", r, s] -> Just (Mul (r, parseValue s))
    _ -> Nothing 

parseMod : List String -> Maybe Instruction
parseMod parts = 
  case parts of 
    ["mod", r, s] -> Just (Mod (r, parseValue s))
    _ -> Nothing 

parseJgz : List String -> Maybe Instruction
parseJgz parts = 
  case parts of 
    ["jgz", s1, s2] -> Just (Jgz (parseValue s1, parseValue s2))
    _ -> Nothing 

parseInstruction : String -> Maybe Instruction
parseInstruction line = 
  let 
    parts = line |> String.split " "
  in 
    parts 
    |> parseSnd
    |> orElseWith (\_ -> parseRcv parts)
    |> orElseWith (\_ -> parseSet parts)
    |> orElseWith (\_ -> parseAdd parts)
    |> orElseWith (\_ -> parseMul parts)
    |> orElseWith (\_ -> parseMod parts)
    |> orElseWith (\_ -> parseJgz parts)
    |> orElseWith (\_ -> Nothing)

instructions : Array Instruction
instructions = 
  input |> String.split "\n" |> List.filterMap parseInstruction |> Array.fromList

getRegisterName : Instruction -> Maybe String 
getRegisterName inst = 
  case inst of 
    Snd n -> Just n 
    Rcv n -> Just n 
    Add (n, _) -> Just n
    Mul (n, _) -> Just n
    Mod (n, _) -> Just n
    Jgz (Reg n, _) -> Just n 
    _ -> Nothing

registerNames : List String
registerNames = 
  instructions |> Array.toList |> List.filterMap getRegisterName 

initRegisters : Int -> List String -> Dict String Int 
initRegisters pid regNames = 
  let 
    register = List.foldl (\n d -> Dict.insert n 0 d) Dict.empty regNames
  in 
    register |> Dict.insert "p" pid 

initProgram : Int -> List String -> Program  
initProgram pid regNames = 
  let 
    registers = initRegisters pid regNames 
  in 
  { pid = pid
  , ptr = 0  
  , state = Ready
  , sent = 0
  , sound = Nothing
  , registers = registers
  , inbox = Queue.empty
  , outbox = Queue.empty }

initModel : Bool -> Model 
initModel duet = 
  let 
    p0 = initProgram 0 
    p1 = initProgram 1 
  in 
    { program0 = initProgram 0 registerNames
    , program1 = initProgram 1 registerNames
    , duet = duet 
    , instructions = instructions  
    , result = Nothing 
    , paused = True 
    , finished = False  
    , tickInterval = defaultTickInterval 
    , debug = "" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel False, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | TogglePlay 
  | ToggleDuet
  | Faster 
  | Slower 
  | Clear 

readValue : String -> Dict String Int -> Int 
readValue r registers = 
  Dict.get r registers |> Maybe.withDefault 0 

writeValue : String -> Int -> Dict String Int -> Dict String Int 
writeValue r n registers = 
  Dict.insert r n registers

resolveValue : Value -> Dict String Int -> Int 
resolveValue v registers = 
  case v of 
    Num n -> n 
    Reg r -> readValue r registers

executeInstruction : Bool -> Instruction -> Program -> Program 
executeInstruction duet inst program = 
  case inst of 
    Snd r -> 
      let 
        n = readValue r program.registers 
      in 
        if duet then 
          let 
            outbox = program.outbox |> Queue.enqueue n 
            ptr = program.ptr + 1
            sent = program.sent + 1
          in 
            { program | outbox = outbox, ptr = ptr, sent = sent }
        else 
          { program | sound = Just n, ptr = program.ptr + 1}
    Rcv r -> 
      if duet then 
        case program.inbox |> Queue.dequeue of 
          Just (received, inbox) -> 
            let 
              registers = writeValue r received program.registers
            in 
              { program | registers = registers, inbox = inbox, ptr = program.ptr + 1 }
          Nothing -> 
            { program | state = Waiting }
      else
        let 
          n = readValue r program.registers
        in 
          if n == 0 then 
            { program | ptr = program.ptr + 1 }
          else 
            { program | state = Terminated }
    Set (r, v) -> 
      let 
        n = resolveValue v program.registers
        registers = writeValue r n program.registers 
      in 
        { program | registers = registers, ptr = program.ptr + 1 }
    Add (r, v) -> 
      let 
        n = readValue r program.registers
        m = resolveValue v program.registers
        registers = writeValue r (n + m) program.registers 
      in 
        { program | registers = registers, ptr = program.ptr + 1 }
    Mul (r, v) -> 
      let 
        n = readValue r program.registers
        m = resolveValue v program.registers
        registers = writeValue r (n * m) program.registers 
      in 
        { program | registers = registers, ptr = program.ptr + 1 }
    Mod (r, v) -> 
      let 
        n = readValue r program.registers
        m = resolveValue v program.registers
        registers = writeValue r (n |> modBy m) program.registers 
      in 
        { program | registers = registers, ptr = program.ptr + 1 }
    Jgz (v1, v2) -> 
      let 
        x = resolveValue v1 program.registers
        y = resolveValue v2 program.registers
      in 
        if x > 0 then 
          { program | ptr = program.ptr + y }
        else 
          { program | ptr = program.ptr + 1 }

executeNextInstruction : Bool -> Array Instruction -> Program -> Program 
executeNextInstruction duet instArr program = 
  case Array.get program.ptr instArr of 
    Just inst -> executeInstruction duet inst program 
    Nothing -> { program | state = Terminated }

updateClear : Model -> Model
updateClear model = 
  initModel model.duet 

updateSoloStep : Model -> Model
updateSoloStep model = 
  let 
    program = model.program0 
  in 
    case program.state of 
      Terminated -> { model | finished = True, result = program.sound } 
      Waiting -> model 
      _ -> 
        let 
          p = executeNextInstruction model.duet model.instructions program
        in 
          { model | program0 = p }

transferMessages : (Program, Program) -> (Program, Program) 
transferMessages (p0, p1) = 
  case (Queue.isEmpty p0.outbox, Queue.isEmpty p1.outbox) of 
    (True, True) ->
      (p0, p1)
    (False, _) -> 
      case Queue.dequeue p0.outbox of 
        Just (msg, outbox) -> 
          let 
            inbox = Queue.enqueue msg p1.inbox 
          in 
            ({p0 | outbox = outbox}, {p1 | inbox = inbox}) |> transferMessages
        Nothing -> 
          (p0, p1) 
    (_, False) -> 
      case Queue.dequeue p1.outbox of 
        Just (msg, outbox) -> 
          let 
            inbox = Queue.enqueue msg p0.inbox 
          in 
            ({p0 | inbox = inbox}, {p1 | outbox = outbox}) |> transferMessages
        Nothing -> 
          (p0, p1) 

checkInbox : Program -> Program 
checkInbox program = 
  case program.state of 
    Waiting -> 
      if Queue.isEmpty program.inbox then program 
      else { program | state = Running } 
    _ -> 
      program

updateDuetStep : Model -> Model
updateDuetStep model = 
  let 
    program0 = model.program0 
    program1 = model.program1
  in 
    case (program0.state, program1.state) of 
      (Terminated, Terminated) -> 
        { model | finished = True, result = Just (program1.sent) } 
      (Waiting, Waiting) -> 
        { model | finished = True, result = Just (program1.sent) } 
      _ -> 
        let 
          p0 = program0 |> checkInbox |> executeNextInstruction model.duet model.instructions
          p1 = program1 |> checkInbox |> executeNextInstruction model.duet model.instructions
          (p0t, p1t) = transferMessages (p0, p1)
        in 
          { model | program0 = p0t, program1 = p1t }
          

updateStep : Model -> Model
updateStep model = 
  if model.duet then 
    updateDuetStep model 
  else 
    updateSoloStep model 

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  { model | paused = not model.paused }

updateToggleDuet : Model -> Model
updateToggleDuet model = 
  let
    duet = not model.duet
  in
    initModel duet 

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
    ToggleDuet -> 
      (updateToggleDuet model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused || model.finished then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

-- VIEW

toRegisterElement : (String, Int) -> Html Msg 
toRegisterElement (r, v) = 
  let 
    str = r ++ ": " ++ String.fromInt v 
  in 
    Html.text str

toMessageElement : Int -> Html Msg 
toMessageElement v = 
  let 
    str = String.fromInt v 
  in 
    Html.text str

unparseValue : Value -> String 
unparseValue v = 
  case v of 
    Reg r -> r 
    Num n -> String.fromInt n

unparse : Instruction -> String 
unparse inst = 
  case inst of 
    Snd r -> "snd " ++ r 
    Rcv r -> "rcv " ++ r 
    Set (r, v) -> "set " ++ r ++ " " ++ unparseValue v
    Add (r, v) -> "add " ++ r ++ " " ++ unparseValue v
    Mul (r, v) -> "mul " ++ r ++ " " ++ unparseValue v
    Mod (r, v) -> "mod " ++ r ++ " " ++ unparseValue v
    Jgz (v, w) -> "jgz " ++ unparseValue v ++ " " ++ unparseValue w

programTable : Bool -> Array Instruction -> Program -> Html Msg 
programTable duet instArr program = 
  let 
    programStr = "program " ++ String.fromInt program.pid
    pointerStr = "ptr: " ++ String.fromInt program.ptr
    instStr = instArr |> Array.get program.ptr |> Maybe.map unparse |> Maybe.withDefault "?"
    stateStr = 
      case program.state of 
        Ready -> "ready"
        Running -> "running"
        Waiting -> "waiting"
        Terminated -> "terminated"
    brElement = Html.br [] []
    regElements = program.registers |> Dict.toList |> List.map toRegisterElement     
    regCellElements = 
      (Html.text "registers") :: regElements |> List.intersperse brElement
    cellElements = 
      if duet then 
        let 
          sentStr = program.sent |> String.fromInt
        in 
          [ Html.text "sent", brElement, Html.text sentStr ]
      else 
        let 
          soundStr = program.sound |> Maybe.map String.fromInt |> Maybe.withDefault "?"
        in 
          [ Html.text "sound", brElement, Html.text soundStr ]
    rows = 
      [ Html.tr 
          [ ] 
          [ Html.td 
            [ Html.Attributes.style "padding" "4px 10px"
            , Html.Attributes.style "border" "solid" ] 
            [ Html.text programStr
            ] 
          ] 
      , Html.tr 
          [ ] 
          [ Html.td 
            [ Html.Attributes.style "padding" "4px 10px"
            , Html.Attributes.style "border" "solid" ] 
            [ Html.text stateStr
            ]
          ] 
      , Html.tr 
          [ ] 
          [ Html.td 
            [ Html.Attributes.style "padding" "4px 10px"
            , Html.Attributes.style "border" "solid" ] 
            [ Html.text pointerStr
            ]
          ] 
      , Html.tr 
          [ ] 
          [ Html.td 
            [ Html.Attributes.style "padding" "4px 10px"
            , Html.Attributes.style "border" "solid" ] 
            [ Html.text instStr
            ]
          ] 
      , Html.tr 
          [ ] 
          [ Html.td 
            [ Html.Attributes.style "padding" "4px 10px"
            , Html.Attributes.style "border" "solid" ] 
            regCellElements
          ]  
      , Html.tr 
          [ ] 
          [ Html.td 
            [ Html.Attributes.style "padding" "4px 10px"
            , Html.Attributes.style "border" "solid" ] 
            cellElements
          ]
      ]
    rowElements = 
      if duet then  
        let 
          inboxSizeStr = program.inbox |> Queue.length |> String.fromInt
          extraRow = 
            Html.tr 
              [] 
              [ Html.td 
                [ Html.Attributes.style "padding" "4px 10px"
                , Html.Attributes.style "border" "solid" ] 
                [ Html.text "inbox", brElement, Html.text inboxSizeStr ]
            ] 
        in 
          List.append rows [ extraRow ] 
      else 
        rows         
  in 
    Html.table 
      [ Html.Attributes.style "border" "solid"
      , Html.Attributes.style "width" "220px" ]
      rowElements

singleTable : Model -> Html Msg 
singleTable model =
  Html.table 
    []
    [ Html.tr 
        [ ] 
        [ Html.td [] [ programTable False model.instructions model.program0 ] ]
    ]

duetTable : Model -> Html Msg 
duetTable model =
  Html.table 
    []
    [ Html.tr 
        [] 
        [ Html.td [] [ programTable True model.instructions model.program0 ]
        , Html.td [] [ programTable True model.instructions model.program1] ]
    ]

contentTable : Model -> Html Msg 
contentTable model = 
  if model.duet then duetTable model else singleTable model 

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2017 | Day 18: Duet"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    elements = [ contentTable model ]
    debug = "Instructions: " ++ String.fromInt (Array.length model.instructions)
    resultStr = 
      case model.result of 
        Nothing -> "?"
        Just res -> String.fromInt res
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
              [ Html.div [] [Html.text "Advent of Code 2017" ]
              , Html.div [] [Html.text "Day 18: Duet" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2017/day/18" ] 
                [ Html.text "https://adventofcode.com/2017/day/18" ]
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
                [ Html.Attributes.type_ "checkbox", onClick ToggleDuet, Html.Attributes.checked model.duet ] 
                []
              , Html.label [] [ Html.text " Duet" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px" ] 
              [ 
                Html.div [] [ Html.text resultStr ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" "16px"
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [] elements
              ] ] 
              ]
