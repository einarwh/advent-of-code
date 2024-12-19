module Aoc17 exposing (..)

import Browser exposing (Document)
import BigInt exposing (BigInt)
import Bitwise
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick, onInput)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Dict exposing (Dict)
import Array exposing (Array)
import Html exposing (text)
import Time

-- MAIN

defaultBackgroundColor = "#808080"
lightBackgroundColor = "#989898"
darkBackgroundColor = "#696969"

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample | Quine 

type BootStatus = Booting Float | Booted

type alias Computer =
  { regA : BigInt
  , regB : BigInt
  , regC : BigInt
  , pointer : Int
  , program : Array Int
  , outputs : List Int }

type alias Model =
  { computer : Computer
  , initialA : BigInt
  , overwrittenA : String 
  , step : Int 
  , dataSource : DataSource
  , backgroundColor : String
  , paused : Bool 
  , bootStatus : BootStatus
  , tickInterval : Float
  , bootTickInterval : Float
  , darkBackgroundTickInterval : Float
  , lightBackgroundTickInterval : Float
  , debug : String }

parseRegister : String -> BigInt
parseRegister s =
  case s |> String.split ": " of
    [_, str] -> str |> BigInt.fromIntString |> Maybe.withDefault (BigInt.fromInt 0)
    _ -> BigInt.fromInt 0

parseProgram : String -> Array Int
parseProgram s =
  case s |> String.split ": " of
    [_, str] ->
      str |> String.split "," |> List.filterMap (String.toInt) |> Array.fromList
    _ -> [] |> Array.fromList

parseComputer : String -> Computer
parseComputer data =
  case String.lines data of
    [a, b, c, _, p] ->
      { regA = parseRegister a
      , regB = parseRegister b
      , regC = parseRegister c
      , pointer = 0
      , program = parseProgram p
      , outputs = [] }
    _ ->
      { regA = BigInt.fromInt 0
      , regB = BigInt.fromInt 0
      , regC = BigInt.fromInt 0
      , pointer = 0
      , program = Array.empty
      , outputs = [] }

initComputer : DataSource -> Computer
initComputer dataSource =
  let
    sample = """Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"""
    sample2 = """Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"""
    input = """Register A: 37283687
Register B: 0
Register C: 0

Program: 2,4,1,3,7,5,4,1,1,3,0,3,5,5,3,0"""
    quine = """Register A: 108107566389757
Register B: 0
Register C: 0

Program: 2,4,1,3,7,5,4,1,1,3,0,3,5,5,3,0"""
    data =
      case dataSource of
        Sample -> sample
        Input -> input
        Quine -> quine
    computer = parseComputer data
  in
    computer

initModel : BootStatus -> DataSource -> Model
initModel bootStatus dataSource =
  let
    computer = initComputer dataSource
    model = { computer = computer
            , dataSource = dataSource
            , initialA = computer.regA
            , overwrittenA = ""
            , step = 0
            , backgroundColor = defaultBackgroundColor
            , bootStatus = bootStatus
            , paused = True 
            , tickInterval = 200
            , bootTickInterval = 1000
            , darkBackgroundTickInterval = 177
            , lightBackgroundTickInterval = 477
            , debug = "" }
  in
    model

init : () -> (Model, Cmd Msg)
init _ =
  (initModel Booted Quine, Cmd.none)
  -- (initModel (Booting 0) Input, Cmd.none)

-- UPDATE

type Msg =
  Clear
  | Tick 
  | TogglePlay
  | Faster
  | Slower
  | Step
  | UseSample
  | UseInput
  | BootTick
  | DefaultBackgroundTick
  | DarkBackgroundTick
  | LightBackgroundTick
  | OverwriteRegA String

tryReadOpcode : Computer -> Maybe Int 
tryReadOpcode computer = 
  let 
    pt = computer.pointer 
  in 
    computer.program |> Array.get pt

literal : Computer -> Int
literal computer = 
  let 
    pt = computer.pointer 
  in 
    computer.program |> Array.get (pt + 1) |> Maybe.withDefault 0

shrinkInt : BigInt -> Int 
shrinkInt big = 
  case big |> BigInt.toString |> String.toInt of 
    Nothing -> 0
    Just n -> n 

combo : Computer -> BigInt 
combo computer =
  case literal computer of
    0 -> BigInt.fromInt 0
    1 -> BigInt.fromInt 1
    2 -> BigInt.fromInt 2
    3 -> BigInt.fromInt 3
    4 -> computer.regA
    5 -> computer.regB
    6 -> computer.regC
    _ -> BigInt.fromInt 0

pow x y = if y < 1 then 1 else x * pow x (y - 1)

writeA : BigInt -> Computer -> Computer 
writeA value computer = 
  { computer | regA = value }

writeB : BigInt -> Computer -> Computer 
writeB value computer = 
  { computer | regB = value }

writeC : BigInt -> Computer -> Computer 
writeC value computer = 
  { computer | regC = value }

nextInstruction : Computer -> Computer 
nextInstruction computer =
    { computer | pointer = computer.pointer + 2 }

jump : Int -> Computer -> Computer  
jump target computer =
  { computer | pointer = target }

output : Int -> Computer -> Computer 
output value computer =
    { computer | outputs = value :: computer.outputs }

zip : List a -> List b -> List (a, b) 
zip lst1 lst2 = 
  case (lst1, lst2) of 
    ((h1 :: r1), (h2 :: r2)) -> (h1, h2) :: zip r1 r2 
    _ -> []

zipWithDefaults : a -> b -> List a -> List b -> List (a, b) 
zipWithDefaults a b lst1 lst2 = 
  case (lst1, lst2) of 
    ((h1 :: r1), (h2 :: r2)) -> (h1, h2) :: zipWithDefaults a b r1 r2 
    ((h1 :: r1), []) -> (h1, b) :: zipWithDefaults a b r1 []
    ([], (h2 :: r2)) -> (a, h2) :: zipWithDefaults a b [] r2 
    ([], []) -> []

toBitsLoop : BigInt -> BigInt -> BigInt -> List BigInt -> List BigInt 
toBitsLoop big0 big2 n result = 
  if BigInt.gt n big0 then 
    let 
      (divResult, remResult) = BigInt.divmod n big2 |> Maybe.withDefault (big0, big0)
    in 
      toBitsLoop big0 big2 divResult (remResult :: result)
  else 
    result |> List.reverse

toBits : BigInt -> List BigInt 
toBits n =
  let 
    big0 = BigInt.fromInt 0
    big2 = BigInt.fromInt 2
  in 
    toBitsLoop big0 big2 n [] 

fromBitsLoop : BigInt -> BigInt -> List BigInt -> BigInt 
fromBitsLoop big0 big2 bits = 
  bits 
  |> List.indexedMap (\ix b -> BigInt.mul b (BigInt.pow big2 (BigInt.fromInt ix)))
  |> List.foldl (BigInt.add) big0 

fromBits : List BigInt -> BigInt 
fromBits bits = 
  let 
    big0 = BigInt.fromInt 0
    big2 = BigInt.fromInt 2
  in 
    fromBitsLoop big0 big2 bits

bitXor : (BigInt, BigInt) -> BigInt 
bitXor (bit1, bit2) = 
  if bit1 == bit2 then BigInt.fromInt 0 else BigInt.fromInt 1 

bigXor : BigInt -> BigInt -> BigInt 
bigXor a b =
  let
    abits = toBits a 
    bbits = toBits b 
    big0 = BigInt.fromInt 0
    zipped = zipWithDefaults big0 big0 abits bbits
    bits = zipped |> List.map bitXor
  in 
    bits |> fromBits

division : Computer -> BigInt 
division computer =
  let 
    numerator = computer.regA
    denominator = BigInt.pow (BigInt.fromInt 2) (combo computer)
  in 
    BigInt.div numerator denominator 

adv : Computer -> Computer 
adv computer = 
  computer
  |> writeA (division computer)
  |> nextInstruction

bdv : Computer -> Computer 
bdv computer =
  computer
  |> writeB (division computer)
  |> nextInstruction

cdv : Computer -> Computer 
cdv computer =
  computer
  |> writeC (division computer)
  |> nextInstruction

bxl : Computer -> Computer 
bxl computer =
  computer
  |> writeB (bigXor (BigInt.fromInt (literal computer)) (computer.regB))
  |> nextInstruction

bst : Computer -> Computer 
bst computer =
  computer
  |> writeB (combo computer |> BigInt.modBy (BigInt.fromInt 8) |> Maybe.withDefault (BigInt.fromInt 0))
  |> nextInstruction

jnz : Computer -> Computer 
jnz computer =
  if computer.regA == BigInt.fromInt 0 then
    computer |> nextInstruction
  else

    computer |> jump (literal computer)

bxc : Computer -> Computer
bxc computer =
  computer
  |> writeB (bigXor (computer.regB) (computer.regC))
  |> nextInstruction

out : Computer -> Computer 
out computer =
  computer
  |> output (combo computer |> BigInt.modBy (BigInt.fromInt 8) |> Maybe.withDefault (BigInt.fromInt 0) |> shrinkInt)
  |> nextInstruction

executeOpcode : Int -> Computer -> Computer 
executeOpcode opcode computer = 
  case opcode of
    0 -> adv computer
    1 -> bxl computer
    2 -> bst computer
    3 -> jnz computer
    4 -> bxc computer
    5 -> out computer
    6 -> bdv computer
    7 -> cdv computer
    _ -> computer

executeInstruction : Computer -> Maybe Computer 
executeInstruction computer = 
  case tryReadOpcode computer of
    Nothing -> Nothing 
    Just opcode -> Just (executeOpcode opcode computer)

updateClear : Model -> Model
updateClear model =
  initModel (Booting 0) model.dataSource

updateProgramFinished : Model -> Model
updateProgramFinished model = 
  { model | paused = True } 

updateProgramRunning : Computer -> Model -> Model
updateProgramRunning computer model = 
  { model | computer = computer, step = model.step + 1 } 

updateStep : Model -> Model
updateStep model =
  case model.computer |> executeInstruction of 
    Nothing -> updateProgramFinished model 
    Just computer -> updateProgramRunning computer model 

updateDataSource : DataSource -> Model -> Model
updateDataSource dataSource model =
  { model | dataSource = dataSource, computer = initComputer dataSource }

updateBackgroundColor : String -> Model -> Model
updateBackgroundColor color model =
  { model | backgroundColor = color }

updateBootTick : Model -> Model
updateBootTick model =
  let
    bootStatus =
      case model.bootStatus of
        Booting ticks ->
          if ticks <= 8 * model.bootTickInterval then
            Booting (ticks + model.bootTickInterval)
          else
            Booted
        Booted -> Booted
  in
    { model | bootStatus = bootStatus }

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  { model | paused = not model.paused }

updateOverwriteRegA : String -> Model -> Model
updateOverwriteRegA overwrittenA model = 
  if String.isEmpty overwrittenA then 
    let computer = model.computer in 
      { model | overwrittenA = "", computer = { computer | regA = model.initialA } }
  else 
    case overwrittenA |> BigInt.fromIntString of 
      Just a -> 
        let 
          computer = model.computer 
        in 
          { model | overwrittenA = overwrittenA, computer = { computer | regA = a } }
      Nothing -> 
        model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear ->
      (updateClear model, Cmd.none)
    Step ->
      (updateStep model, Cmd.none)
    Tick ->
      (updateStep model, Cmd.none)
    Faster -> 
      ({model | tickInterval = model.tickInterval / 2 }, Cmd.none)
    Slower -> 
      ({model | tickInterval = model.tickInterval * 2 }, Cmd.none)
    TogglePlay -> 
      (updateTogglePlay model, Cmd.none)
    UseSample ->
      (updateDataSource Sample model, Cmd.none)
    UseInput ->
      (updateDataSource Input model, Cmd.none)
    BootTick ->
      (updateBootTick model, Cmd.none)
    DefaultBackgroundTick ->
      (updateBackgroundColor defaultBackgroundColor model, Cmd.none)
    DarkBackgroundTick ->
      (updateBackgroundColor darkBackgroundColor { model | darkBackgroundTickInterval = 1377 }, Cmd.none)
    LightBackgroundTick ->
      (updateBackgroundColor lightBackgroundColor { model | lightBackgroundTickInterval = 7977 }, Cmd.none)
    OverwriteRegA overwrittenA -> 
      (updateOverwriteRegA overwrittenA model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    tick =
      case model.bootStatus of
        Booted -> if model.paused then Sub.none else Time.every model.tickInterval (\_ -> Tick)
        Booting _ -> Time.every model.bootTickInterval (\_ -> BootTick)
    defaultBackgroundTick = Time.every 277 (\_ -> DefaultBackgroundTick)
    darkBackgroundTick = Time.every model.darkBackgroundTickInterval (\_ -> DarkBackgroundTick)
    lightBackgroundTick = Time.every model.lightBackgroundTickInterval (\_ -> LightBackgroundTick)
  in
    Sub.batch
    [ tick
    , defaultBackgroundTick
    , darkBackgroundTick
    , lightBackgroundTick ]

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

toBootedElements : Model -> List (Svg Msg)
toBootedElements model =
  let
    svgWidth = String.fromInt (800)
    svgHeight = String.fromInt (400)
    computer = model.computer
    programStr = computer.program |> Array.toList |> List.map String.fromInt |> String.join ","
    textStroke = "black"
    textFill = "#F28C28"
    pointer = computer.pointer
    paddings = 1 + 2 * pointer
    pointerPosition = 70
    pointerPositionStr = String.fromInt pointerPosition
    pointerText = 
      if pointer < Array.length computer.program then 
        String.padLeft paddings ' ' "^"
      else 
        ""
    outputsText = computer.outputs |> List.reverse |> List.map (String.fromInt) |> String.join ","
    borderElement = rect [ x "0", y "0", width svgWidth, height svgHeight, strokeWidth "4px", stroke "black", fill "none" ] []
    regABox = rect [ x "60", y "10", width "720", height "40", strokeWidth "2px", stroke "black", fill "white", fillOpacity "0.2" ] []
    regALabel = text_ [ x "20", y "42", stroke textStroke, fill textFill ] [ Svg.text "A" ]
    regAValue = text_ [ x "70", y "42", stroke textStroke, fill textFill ] [ Svg.text (BigInt.toString computer.regA) ]
    regBBox = rect [ x "60", y "60", width "720", height "40", strokeWidth "2px", stroke "black", fill "white", fillOpacity "0.2" ] []
    regBLabel = text_ [ x "20", y "92", stroke textStroke, fill textFill ] [ Svg.text "B" ]
    regBValue = text_ [ x "70", y "92", stroke textStroke, fill textFill ] [ Svg.text (BigInt.toString computer.regB) ]
    regCBox = rect [ x "60", y "110", width "720", height "40", strokeWidth "2px", stroke "black", fill "white", fillOpacity "0.2" ] []
    regCLabel = text_ [ x "20", y "142", stroke textStroke, fill textFill ] [ Svg.text "C" ]
    regCValue = text_ [ x "70", y "142", stroke textStroke, fill textFill ] [ Svg.text (BigInt.toString computer.regC) ]
    programBox = rect [ x "60", y "160", width "720", height "80", strokeWidth "2px", stroke "black", fill "white", fillOpacity "0.2" ] []
    programLabel = text_ [ x "20", y "192", stroke textStroke, fill textFill ] [ Svg.text "P" ]
    programValue = text_ [ x "70", y "192", stroke textStroke, fill textFill ] [ Svg.text programStr ]
    pointerHat = text_ [ x "70", y "232", Svg.Attributes.style "white-space:pre", stroke textStroke, fill textFill ] [ Svg.text pointerText ]
    outputsBox = rect [ x "60", y "250", width "720", height "140", strokeWidth "2px", stroke "black", fill "white", fillOpacity "0.2" ] []
    outputsValue = text_ [ x "70", y "282", stroke textStroke, fill textFill ] [ Svg.text outputsText ]
  in
    [ borderElement, regALabel, regABox, regAValue, regBLabel, regBBox, regBValue, regCLabel, regCBox, regCValue, programLabel, programBox, programValue, pointerHat, outputsBox, outputsValue ]

toBootingElements model ticks =
  let
    svgWidth = String.fromInt (800)
    svgHeight = String.fromInt (400)
    textStroke = "black"
    textFill = "#F28C28"
    -- ticksStr = String.fromFloat ticks
    criticalText = text_ [ x "20", y "42", stroke textStroke, fill textFill ] [ Svg.text ("Situation critical!") ]
    bootstrappingText = text_ [ x "20", y "92", stroke textStroke, fill textFill ] [ Svg.text "Bootstrapping process failed." ]
    dots = String.repeat (floor (ticks / model.bootTickInterval) - 4) "."
    debuggerText = text_ [ x "20", y "142", stroke textStroke, fill textFill ] [ Svg.text ("Initializing debugger" ++ dots) ]
    textElements =
      [ Just criticalText
      , if ticks >= 2 * model.bootTickInterval then Just bootstrappingText else Nothing
      , if ticks >= 4 * model.bootTickInterval then Just debuggerText else Nothing ]
      |> List.filterMap identity
    borderElement = rect [ x "0", y "0", width svgWidth, height svgHeight, strokeWidth "4px", stroke "black", fill "none" ] []
  in
    List.append textElements [ borderElement ]

toSvg : Model -> Html Msg
toSvg model =
  let
    svgWidth = String.fromInt (800)
    svgHeight = String.fromInt (400)
    viewBoxStr = "0 0 " ++ svgWidth ++ " " ++ svgHeight
    backgroundColor = model.backgroundColor
    elements =
      case model.bootStatus of
        Booting ticks ->
          toBootingElements model ticks
        Booted ->
          toBootedElements model
  in
    svg
      [ viewBox viewBoxStr
      , width svgWidth
      , height svgHeight
      , Svg.Attributes.style ("background-color:" ++ backgroundColor)
      ]
      elements

view : Model -> Document Msg
view model =
  { title = "Advent of Code 2024 | Day 17: Chronospatial Computer"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    commandsStr =
      case model.bootStatus of
        Booting ticks -> String.fromFloat ticks
        Booted -> "booted"
    textFontSize =
      case model.dataSource of
        Sample -> "24px"
        Input -> "14px"
        Quine -> "14px"
    elements = []
    s = toSvg model
    qval = toFloat 108107566389757
    denom = toFloat (pow 2 6)
    res = qval / denom |> floor
    litcom = 3
    regB = BigInt.fromInt 6 
    -- (BigInt.fromInt (Bitwise.xor (literal computer) (computer.regB |> shrinkInt)))
    (divVal, remVal) = (BigInt.divmod (BigInt.fromInt 13) (BigInt.fromInt 2)) |> Maybe.withDefault (BigInt.fromInt 0, BigInt.fromInt 0)
    debugStr1 = BigInt.toString (BigInt.modBy (BigInt.fromInt 13) (BigInt.fromInt 2) |> Maybe.withDefault (BigInt.fromInt -100))
    debugStr2 = BigInt.toString (BigInt.modBy (BigInt.fromInt 2) (BigInt.fromInt 13) |> Maybe.withDefault (BigInt.fromInt -100))
    debugStr3 = "(" ++ BigInt.toString divVal ++ "," ++ BigInt.toString remVal ++ ")"
    debugStr4 = (bigXor (BigInt.fromInt 177) (BigInt.fromInt 377)) |> BigInt.toString
    debugStr5 = (Bitwise.xor 177 377) |> String.fromInt
    debugStr6 = (BigInt.fromInt 177) |> toBits |> List.map (BigInt.toString) |> String.join " "
    debugStr7 = (BigInt.fromInt 377) |> toBits |> List.map (BigInt.toString) |> String.join " "
    debugStr8 = (BigInt.fromInt 13) |> toBits |> List.map (BigInt.toString) |> String.join " "
    debugStr9 = if BigInt.gt (BigInt.fromInt 13) (BigInt.fromInt 7) then "13" else "7"
    big0 = BigInt.fromInt 0
    abits = toBits (BigInt.fromInt 177)
    bbits = toBits (BigInt.fromInt 377)
    zipped = zipWithDefaults big0 big0 abits bbits
    xored = zipped |> List.map bitXor
    xoredStr = xored |> List.map (BigInt.toString) |> String.join " "
    multed = xored |> List.indexedMap (\ix b -> BigInt.mul b (BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt ix)))

    reconstructStr = xored |> fromBitsLoop (BigInt.fromInt 0) (BigInt.fromInt 2) |> BigInt.toString
    zippedStr = zipped |> List.map (\(a, b) -> "(" ++ BigInt.toString a ++ ", " ++ BigInt.toString b ++ ")") |> String.join ":"
    -- debugStr = String.fromInt (Bitwise.xor litcom (shrinkInt regB))
    numerator = BigInt.fromInt 108107566389757
    denominator = BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt 6)
    divResult = BigInt.div numerator denominator 
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
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "10px"]
              [ Html.div [] [Html.text "Advent of Code 2024" ]
              , Html.div [] [Html.text "Day 17: Chronospatial Computer" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2024/day/17" ]
                [ Html.text "https://adventofcode.com/2024/day/17" ]
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
                [ Html.Attributes.style "width" "80px", onClick Clear ] 
                [ Html.text "Reboot" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Slower ] 
                [ Html.text "Turtle" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then Html.text "Run" else Html.text "Halt" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Faster ] 
                [ Html.text "Turbo" ]
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
                  Html.input 
                    [ Html.Attributes.placeholder "Overwrite register A"
                    , Html.Attributes.value model.overwrittenA
                    , onInput OverwriteRegA ] 
                    []
              , Html.div [] [ Html.text commandsStr ]
              , Html.div [] [ Html.text ("DebugStr1 " ++ debugStr1) ]
              , Html.div [] [ Html.text ("DebugStr2 " ++ debugStr2) ]
              , Html.div [] [ Html.text ("DebugStr3 " ++ debugStr3) ]
              , Html.div [] [ Html.text ("DebugStr4 (bigXor) " ++ debugStr4) ]
              , Html.div [] [ Html.text ("DebugStr5 (intXor) " ++ debugStr5) ]
              , Html.div [] [ Html.text ("DebugStr6 (177 as bits) " ++ debugStr6 ++ " *") ]
              , Html.div [] [ Html.text ("DebugStr7 (377 as bits) " ++ debugStr7) ]
              , Html.div [] [ Html.text ("DebugStr8 (13 as bits) " ++ debugStr8) ]
              , Html.div [] [ Html.text ("DebugStr9 (gt?) " ++ debugStr9) ]
              , Html.div [] [ Html.text ("zipped " ++ zippedStr) ]
              , Html.div [] [ Html.text ("xored " ++ xoredStr) ]
              , Html.div [] [ Html.text ("recons " ++ reconstructStr) ]
              , Html.div [] [ Html.text ("numerator: " ++ BigInt.toString numerator) ]
              , Html.div [] [ Html.text ("denominator: " ++ BigInt.toString denominator) ]
              , Html.div [] [ Html.text ("divResult: " ++ BigInt.toString divResult) ]
              , Html.div [] [ Html.text (String.fromFloat qval) ]
              , Html.div [] [ Html.text ("Step: " ++ (String.fromInt model.step)) ]
              ] ]
      , Html.tr
          []
          [ Html.td
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white"
              , Html.Attributes.style "font-family" "Source Code Pro, monospace"
              , Html.Attributes.style "font-size" "32px"
              , Html.Attributes.style "padding" "10px"
              , Html.Attributes.style "width" "200px" ]
              [
                Html.div [] [ s ]
              ] ] ]
