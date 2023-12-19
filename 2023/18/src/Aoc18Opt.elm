module Aoc18Opt exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Mouse exposing (..)
import Svg exposing (..)
import Svg.Events
import Svg.Attributes exposing (..)
import Set exposing (Set)
import Time 

delay : Float
delay = 100

inputname : String
inputname = "input"

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Position = (Int, Int)

type alias Direction = (Int, Int)

type alias Instruction = 
  { dir : Direction
  , meters : Int }

type alias Basin = 
  { startPoint : Position 
  , trench : Set Position
  , trenchChunks : Set Position
  , chunkSize : Int 
  , explorationPoints : Set Position
  , filledPoints : Set Position
  , filledPointsChunks : Set Position 
  , filledPointsUnmovable : Set Position 
  , fillInside : Bool
  , widthInUnits : Int 
  , heightInUnits : Int
  , cubicMeters : Int }

type alias Model = 
  { basin : Basin
  , mousePoint : Position
  , unitSize : Int 
  , running : Bool
  , debugPoint : String  
  , debug : String }

type Msg = Click | Tick | MouseMove (Float, Float)

toDirection : String -> Maybe Direction 
toDirection s = 
  case s of 
    "U" -> Just (0, -1)
    "D" -> Just (0, 1)
    "R" -> Just (1, 0) 
    "L" -> Just (-1, 0)
    _ -> Nothing 

parseLine : String -> Instruction 
parseLine s = 
  case String.split " " s of 
    a :: b :: _ -> 
      let 
        d = a |> toDirection |> Maybe.withDefault (0, 0)
        m = b |> String.toInt |> Maybe.withDefault 0
      in { dir = d, meters = m }
    _ -> { dir = (0, 0), meters = 0 }

move : Position -> Direction -> Int -> List Position 
move (xStart, yStart) (xStep, yStep) meters =
  List.range 1 meters |> List.map (\m -> (xStart + xStep * m, yStart + yStep * m))

digLagoonLoop : Position -> List Position -> List Instruction -> List Position 
digLagoonLoop current positions instructions = 
  case instructions of 
    [] -> positions 
    inst :: remaining -> 
      let 
        nextPositions = move current inst.dir inst.meters 
        next = nextPositions |> List.reverse |> List.head |> Maybe.withDefault (0, 0)
      in 
        remaining |> digLagoonLoop next (positions ++ nextPositions)

digLagoon : List Instruction -> List Position 
digLagoon instructions = 
  digLagoonLoop (0, 0) [] instructions 

adjustLagoon : List Position -> List Position
adjustLagoon positions = 
  let 
    xs = positions |> List.map (Tuple.first)
    ys = positions |> List.map (Tuple.second)
    xMin = xs |> List.minimum |> Maybe.withDefault 0
    yMin = ys |> List.minimum |> Maybe.withDefault 0
    xOffset = 0 - xMin 
    yOffset = 0 - yMin 
  in 
    positions |> List.map (\(x, y) -> (x + xOffset, y + yOffset))

findStartPosition : List Position -> Position
findStartPosition positions =
  let 
    xs = positions |> List.map Tuple.first 
    ys = positions |> List.map Tuple.second 
    xStart = xs |> List.minimum |> Maybe.withDefault 0
    yStart = positions |> List.filterMap (\(x, y) -> if x == xStart then Just y else Nothing) |> List.minimum |> Maybe.withDefault 0
  in 
    (xStart + 1, yStart + 1)

findDimensions : Set Position -> (Int, Int) 
findDimensions trench = 
  let 
    positions = trench |> Set.toList
    xs = positions |> List.map (Tuple.first)
    ys = positions |> List.map (Tuple.second)
    xMax = xs |> List.maximum |> Maybe.withDefault 123
    yMax = ys |> List.maximum |> Maybe.withDefault 123
  in 
    (xMax + 1, yMax + 1)

init : () -> (Model, Cmd Msg)
init _ =
  let 
    input = """R 5 (#58a492)
U 5 (#630fe3)
L 3 (#7087b2)
U 5 (#5f3ee1)
L 3 (#313202)
U 5 (#03d101)
L 10 (#7c5562)
U 4 (#404f43)
L 3 (#412982)
U 7 (#79c1c3)
L 6 (#12d412)
U 4 (#083683)
L 4 (#775942)
U 6 (#030323)
L 6 (#0c9622)
U 8 (#6cdc33)
L 6 (#03d682)
U 2 (#0e9c53)
L 5 (#493272)
U 2 (#03d643)
L 2 (#3305b2)
U 9 (#244203)
L 6 (#6c6a70)
U 4 (#341f73)
L 8 (#28bb00)
U 3 (#74c533)
R 4 (#3cfdb0)
U 3 (#3fd773)
R 3 (#52a350)
U 9 (#3c2073)
R 3 (#316de0)
U 6 (#3c4213)
R 6 (#0b7720)
U 5 (#3734e3)
R 2 (#929370)
U 7 (#372c31)
R 7 (#503d70)
U 7 (#27d791)
R 5 (#4cac80)
U 6 (#24f401)
R 3 (#4c95a0)
U 6 (#43bb01)
R 5 (#2ff3b2)
U 4 (#2fb481)
R 2 (#7c5eb2)
U 2 (#141aa1)
R 9 (#1259d2)
U 4 (#4e9531)
R 4 (#2ad362)
U 3 (#27d1b1)
R 9 (#149920)
U 3 (#0b0163)
R 4 (#65fc30)
U 4 (#5f54e3)
R 3 (#473e00)
U 4 (#0d0da3)
R 8 (#327d60)
U 3 (#187fc3)
L 8 (#66f6c0)
U 3 (#6981f3)
L 3 (#42b9d0)
U 4 (#6f32f3)
L 3 (#42b9d2)
U 7 (#16b7a3)
L 5 (#66f6c2)
D 7 (#0298a3)
L 8 (#29c4a0)
D 2 (#4f2473)
L 4 (#266862)
D 10 (#436fd1)
L 4 (#3482b2)
U 5 (#30fad3)
L 4 (#6ac712)
U 6 (#30fad1)
L 2 (#4ee392)
U 4 (#436fd3)
R 6 (#59ff52)
U 4 (#73a323)
L 4 (#20ba82)
U 8 (#5e5873)
L 4 (#578df2)
D 7 (#47e4f3)
L 2 (#6a4792)
D 8 (#2a00b3)
L 6 (#854930)
U 8 (#594903)
L 3 (#854932)
D 3 (#53db83)
L 2 (#19e4f0)
D 8 (#7c7af3)
L 5 (#5062a0)
U 4 (#2d3db3)
L 3 (#2fe132)
U 7 (#7059f3)
L 3 (#4cc5b2)
U 5 (#2a28a1)
R 3 (#6d89e2)
U 3 (#4398a1)
R 3 (#09b672)
U 11 (#4a32e1)
R 3 (#0d6002)
D 10 (#3ece11)
R 2 (#81a642)
D 4 (#096ba1)
R 5 (#31df02)
U 8 (#7ccf71)
R 5 (#6239a2)
U 7 (#6584c1)
R 6 (#6c1fe0)
U 4 (#1e1b01)
L 3 (#5fbee0)
U 2 (#5bfc41)
L 12 (#49e020)
U 4 (#48a6d1)
R 8 (#6814a2)
U 2 (#226a71)
R 7 (#6814a0)
U 4 (#436dc1)
R 5 (#2a2f92)
D 4 (#28eaf3)
R 9 (#25ab52)
D 3 (#22bd81)
R 2 (#83aa92)
D 3 (#22bd83)
L 5 (#159182)
D 2 (#0387c3)
L 6 (#3599c2)
D 4 (#4e6413)
R 5 (#48f562)
D 7 (#564e23)
R 6 (#46fbf2)
U 6 (#1e12d3)
R 4 (#0bd062)
U 5 (#1ffbb3)
R 3 (#5fbb82)
U 3 (#2d0803)
R 6 (#101d70)
U 8 (#11b903)
R 6 (#9af6c0)
D 6 (#11b901)
R 9 (#1b70a0)
U 6 (#479173)
R 7 (#15cc02)
D 2 (#0aa953)
R 9 (#858072)
D 3 (#3e60f3)
R 3 (#2b3862)
D 3 (#1f4af3)
R 7 (#40b892)
D 3 (#4f9151)
R 3 (#343292)
D 5 (#0ec741)
R 6 (#549592)
D 3 (#7e9611)
R 5 (#0215c2)
D 6 (#57bdb3)
R 3 (#6964c2)
D 6 (#38c523)
R 4 (#172242)
D 2 (#80a643)
R 8 (#21dee0)
D 2 (#1a89e3)
R 8 (#5ea820)
D 2 (#2baab3)
R 4 (#6d2bb0)
D 5 (#4bfb93)
L 10 (#1f13e0)
D 5 (#4fa9d3)
R 10 (#8c3f92)
D 5 (#014b33)
R 4 (#0786d2)
D 5 (#713641)
R 5 (#30eb92)
D 5 (#4591f3)
R 4 (#44fef2)
U 3 (#4591f1)
R 2 (#5636e2)
U 9 (#0cfbb1)
L 5 (#2ded32)
U 4 (#06c881)
L 3 (#5c8ab0)
U 7 (#5ba411)
R 8 (#362110)
U 5 (#023861)
R 3 (#92abc2)
D 6 (#423051)
R 5 (#3bdda2)
D 4 (#59ab71)
R 2 (#103492)
D 4 (#1bc8b1)
R 5 (#739582)
D 5 (#4591d1)
R 3 (#03a662)
U 12 (#0934f1)
R 4 (#2a4662)
D 5 (#2b07d1)
R 3 (#49fd52)
D 7 (#6b8561)
R 3 (#18d152)
D 3 (#573fa1)
L 3 (#57c6b2)
D 2 (#664a03)
L 10 (#190712)
D 4 (#34c2b3)
R 3 (#21f322)
D 3 (#25dc13)
R 3 (#4be7d2)
D 11 (#469a13)
L 4 (#437350)
D 8 (#1a9503)
R 4 (#456d20)
D 4 (#49d533)
R 4 (#88e072)
U 3 (#250333)
R 8 (#4be7d0)
U 3 (#16f773)
R 3 (#8c07b2)
U 5 (#53ad91)
R 3 (#54b682)
U 3 (#3b3621)
R 3 (#54b680)
U 3 (#5bb551)
R 5 (#05def2)
U 5 (#2241f1)
R 4 (#7a0222)
U 2 (#0e6b23)
R 3 (#4b2592)
U 5 (#936c13)
R 2 (#48da12)
U 7 (#727bb3)
L 5 (#072a52)
U 2 (#5c9f13)
L 3 (#4592b0)
U 4 (#3eb493)
L 3 (#9828d0)
U 7 (#3eb491)
L 4 (#108bb0)
U 2 (#88ff83)
L 3 (#043dd0)
U 2 (#88ff81)
L 5 (#3ccdf0)
U 3 (#29f651)
L 5 (#3b58f0)
U 9 (#1dfe43)
L 3 (#3707e0)
U 7 (#1dfe41)
L 5 (#335470)
U 2 (#29f653)
L 3 (#0d04b0)
U 3 (#045723)
R 2 (#3df010)
U 6 (#1eaaa3)
R 6 (#390270)
U 5 (#1e73d1)
R 7 (#672f40)
D 5 (#1e73d3)
R 6 (#83a5d0)
D 6 (#50dd53)
R 2 (#48d590)
D 3 (#5e3bf3)
R 5 (#4064a0)
D 11 (#08fa63)
L 5 (#3f2af0)
D 5 (#3f7793)
R 5 (#6d57f0)
U 2 (#135f33)
R 3 (#445122)
U 10 (#5285b3)
R 3 (#3dc912)
U 6 (#5285b1)
R 4 (#355342)
U 2 (#53a883)
R 6 (#4c4be0)
U 8 (#0303f3)
R 4 (#6b2190)
U 4 (#62c1b3)
R 7 (#3241e0)
D 4 (#057a23)
R 3 (#692490)
D 3 (#0eadb3)
R 8 (#526c20)
D 4 (#5721d3)
R 4 (#33dd40)
D 3 (#4a6513)
R 2 (#1c8770)
D 5 (#0db573)
L 7 (#2a30a2)
D 6 (#186ba3)
R 7 (#788af2)
D 7 (#186ba1)
R 5 (#001542)
D 5 (#3830c3)
R 3 (#32b480)
D 9 (#73a7a3)
R 5 (#5b0230)
D 5 (#562d53)
R 8 (#7e0d30)
D 3 (#059e93)
L 6 (#3284d0)
D 3 (#373711)
L 2 (#4de350)
D 7 (#6179f1)
L 2 (#57f0b0)
D 6 (#4cd0b1)
L 6 (#6a0260)
D 3 (#4cd0b3)
R 4 (#47ed90)
D 8 (#4e4a61)
R 4 (#029330)
D 3 (#56c261)
R 4 (#029332)
U 11 (#08aee1)
R 4 (#3f9e40)
D 3 (#124841)
R 5 (#6af022)
D 10 (#189be1)
R 2 (#4058d2)
D 3 (#6cd791)
R 8 (#2c43c2)
D 5 (#36a191)
R 6 (#2c43c0)
U 5 (#3fff21)
R 3 (#4058d0)
U 8 (#06ad11)
R 4 (#6af020)
D 3 (#487a71)
R 3 (#77aa00)
D 10 (#335cc1)
R 4 (#445960)
U 5 (#36c3f1)
R 3 (#5d13b0)
U 9 (#1f2761)
R 2 (#49e9e0)
U 3 (#7eb501)
R 4 (#578e30)
U 2 (#66dd31)
R 7 (#469130)
U 4 (#7f9823)
R 6 (#1e1e90)
U 5 (#127fa3)
R 5 (#14ac40)
D 6 (#130523)
R 3 (#07a7b0)
D 6 (#5f7be3)
R 8 (#4fff10)
D 6 (#1ad8f1)
R 2 (#32df80)
D 5 (#57a811)
R 7 (#084830)
D 4 (#127fa1)
R 10 (#1fa950)
D 6 (#70fee3)
R 5 (#04f0e2)
D 2 (#4052d3)
R 5 (#3f7cc2)
D 6 (#8a1e13)
R 4 (#3be9a2)
D 4 (#8a1e11)
R 7 (#49c392)
D 3 (#4052d1)
L 6 (#1b27c2)
D 3 (#4ae683)
L 7 (#109120)
D 3 (#634013)
L 5 (#19bbb0)
D 6 (#21de43)
L 4 (#15dd60)
D 5 (#4b3bc3)
L 4 (#691db2)
D 5 (#2f3753)
L 4 (#0ed142)
D 5 (#4d2823)
L 5 (#378122)
D 4 (#5b8733)
L 3 (#35bdc2)
U 6 (#2d1093)
L 6 (#54e720)
U 8 (#12e8d1)
L 4 (#8bb140)
U 4 (#12e8d3)
R 6 (#049570)
U 4 (#01c483)
L 6 (#15dd62)
U 6 (#5d8b23)
R 4 (#4e5940)
U 7 (#92cae1)
L 8 (#1a3a50)
U 3 (#0f4801)
L 6 (#326690)
U 4 (#01c2a1)
L 5 (#051e00)
D 7 (#61eb71)
L 7 (#459650)
D 2 (#9994f1)
L 3 (#471090)
D 8 (#0e7a31)
L 2 (#550610)
D 8 (#194501)
R 6 (#4b09c0)
D 4 (#8e6791)
R 6 (#4b09c2)
D 6 (#0bd3d1)
L 8 (#4527f0)
D 7 (#30e601)
L 6 (#14a132)
D 3 (#795df1)
L 7 (#60edd2)
D 4 (#056141)
L 4 (#41dbc2)
D 3 (#488971)
R 8 (#38fef0)
D 4 (#2cb3b1)
R 3 (#38fef2)
D 3 (#369c41)
L 6 (#3e5bd2)
D 4 (#19bcb3)
L 4 (#173b32)
D 4 (#0de2b3)
L 12 (#377942)
D 3 (#899b43)
L 3 (#3e99b2)
D 3 (#00b211)
L 3 (#4b5ad2)
D 5 (#1f0501)
L 8 (#4abb70)
D 4 (#7f6001)
L 5 (#4abb72)
D 3 (#20c3d1)
L 7 (#1d68a0)
D 9 (#471091)
L 4 (#4c8650)
D 3 (#25ab01)
L 10 (#35f0b0)
D 2 (#695131)
L 10 (#051850)
D 5 (#60dd93)
L 9 (#567cc0)
D 4 (#2e1ea3)
L 5 (#2ffa30)
D 5 (#22b051)
L 5 (#911430)
D 4 (#396521)
L 4 (#11ec70)
D 3 (#356891)
R 8 (#6e13c0)
D 4 (#4d3291)
R 3 (#277462)
D 5 (#57f681)
R 3 (#2f8dc0)
D 3 (#020321)
R 9 (#628620)
D 6 (#152051)
L 6 (#9213e2)
D 5 (#4ec671)
L 6 (#277460)
D 8 (#13c7b1)
L 5 (#1b3e00)
U 6 (#5e57f1)
L 4 (#4a7b20)
U 6 (#5e57f3)
L 4 (#437f90)
U 3 (#020143)
L 4 (#034420)
D 9 (#152c63)
L 3 (#2296b2)
D 2 (#771a63)
L 4 (#2296b0)
D 5 (#251a83)
R 10 (#034422)
D 5 (#6b7823)
L 10 (#19c8c0)
D 3 (#87f251)
L 5 (#53ad40)
U 7 (#534f61)
L 7 (#3ee5c0)
U 6 (#0c6d71)
L 4 (#2d8ba0)
D 6 (#29a941)
L 5 (#0a35f0)
U 6 (#2e83e1)
L 2 (#0a35f2)
U 11 (#45afc1)
L 3 (#2a9d30)
U 3 (#366473)
L 8 (#1e5c90)
U 6 (#73e5e3)
R 8 (#01d790)
U 4 (#3b0e81)
L 2 (#155640)
U 4 (#0f5351)
L 8 (#608130)
U 2 (#7af281)
L 3 (#332700)
U 4 (#05b091)
L 2 (#0faf80)
U 9 (#0091e1)
R 3 (#0b3210)
U 6 (#1ee971)
L 7 (#24f6d2)
U 4 (#519181)
R 7 (#5e5392)
U 3 (#3f1351)
R 3 (#022282)
U 4 (#857db1)
R 9 (#09a712)
U 4 (#3c1341)
R 4 (#025442)
D 8 (#69a253)
R 4 (#767622)
D 7 (#69a251)
R 6 (#4267a2)
D 4 (#320361)
R 5 (#3c5870)
U 7 (#267773)
R 8 (#4b2b40)
U 4 (#267771)
R 4 (#3f77e0)
U 8 (#32f0d1)
R 3 (#834a60)
U 5 (#2e98c1)
R 3 (#635c80)
D 6 (#1e7241)
R 4 (#36a5c0)
D 9 (#3c5311)
R 3 (#0907f0)
U 6 (#394291)
R 2 (#952cc0)
U 9 (#394293)
R 4 (#6b81a0)
D 5 (#592621)
R 4 (#59ea50)
U 8 (#4ef493)
R 7 (#3e3f10)
U 3 (#514253)
L 7 (#3e3f12)
U 9 (#23e503)
L 3 (#3eb3b0)
U 5 (#2e6433)
L 3 (#67c590)
D 5 (#77b101)
L 3 (#21c682)
D 11 (#754aa1)
L 3 (#21c680)
U 6 (#058471)
L 3 (#0ec8c0)
U 10 (#76bdd3)
L 6 (#07fb80)
U 7 (#386853)
R 8 (#55a690)
D 4 (#02f453)
R 8 (#2fb280)
U 4 (#4e8811)
R 5 (#7c2910)
U 3 (#639261)
L 4 (#471b80)
U 5 (#592623)
L 5 (#42bc00)
U 9 (#62c471)
L 7 (#6a8c32)
D 4 (#13d861)
L 4 (#94bb72)
D 6 (#13d863)
L 5 (#2e3ba2)
D 3 (#24ee61)
L 4 (#888a22)
D 9 (#366271)
L 4 (#1201a2)
D 6 (#238401)
L 2 (#246412)
D 7 (#1c23f1)
L 4 (#41d152)
D 3 (#6b5d81)
L 6 (#659b22)
U 3 (#6f1a03)
L 3 (#32db02)
U 4 (#186773)
L 3 (#0043e2)
U 7 (#295c31)
L 3 (#3bd622)
U 8 (#25f1c1)
L 3 (#52ea70)
D 5 (#302c41)
L 4 (#52ea72)
D 5 (#4cb371)
R 4 (#0062e2)
D 6 (#016ab1)
L 4 (#6511c2)
D 3 (#1fae91)
L 3 (#5ca060)
U 7 (#6daef1)
L 5 (#5ca062)
U 7 (#4904a1)
L 7 (#6cc852)
U 3 (#0e2451)
L 3 (#109dd2)
U 5 (#5b3813)
R 8 (#6cb452)
U 2 (#25f353)
R 2 (#6cb450)
U 5 (#4bd743)
L 5 (#609a42)
U 4 (#774593)
L 3 (#609a40)
D 2 (#6dd693)
L 7 (#138f72)
D 7 (#37ed73)
L 7 (#0ad4c2)
U 6 (#32a9f3)
L 8 (#859122)
U 4 (#143d73)
L 11 (#64f202)
U 2 (#18d5c3)
L 3 (#24cb12)
D 10 (#2e7523)
L 2 (#461b52)
D 2 (#5d9883)
L 3 (#6ae660)
D 5 (#1ac423)
R 11 (#2c9fe2)
D 3 (#2e1f33)
L 11 (#8686b2)
D 5 (#0bbc53)
R 3 (#232a62)
D 6 (#3f85d3)
R 3 (#787172)
D 9 (#20eda3)
R 3 (#404292)
D 3 (#3ab463)
R 3 (#22a182)
D 6 (#08f6e3)
L 2 (#776202)
D 2 (#4f3193)
L 5 (#549e22)
D 2 (#5cf443)
L 5 (#549e20)
D 5 (#4e5523)
R 6 (#6f6852)
D 3 (#00e383)
R 10 (#4f9722)
D 5 (#24e003)
R 8 (#138892)
D 3 (#782033)
R 10 (#02bf92)
D 4 (#0d0ad3)
L 8 (#5a8342)
D 2 (#244f11)
L 3 (#182ce0)
D 4 (#6c4331)
L 9 (#140250)
D 5 (#325901)
L 3 (#033dc0)
D 3 (#0c7231)
L 11 (#54fbe0)
D 4 (#094653)
L 4 (#76ee60)
D 11 (#094651)
L 3 (#00f3f0)
D 8 (#0c7233)
L 4 (#255340)
D 5 (#332281)
L 3 (#487c52)
D 3 (#70ada1)
L 10 (#508b12)
U 5 (#1e0d21)
L 6 (#825812)
U 7 (#2f5771)
L 7 (#063ef2)
U 5 (#34f4c1)
L 5 (#1fa462)
U 7 (#74be53)
L 3 (#3c4402)
U 4 (#837ef1)
L 10 (#4fb490)
U 2 (#573091)
L 2 (#408920)
U 5 (#3969e3)
L 3 (#77eff0)
U 5 (#3969e1)
L 7 (#2bec10)
U 4 (#7ac481)
R 8 (#0b7162)
U 2 (#0d6aa1)
R 5 (#502642)
U 3 (#5169c3)
R 12 (#674d02)
U 3 (#5169c1)
L 5 (#713512)
U 7 (#6bd621)
L 7 (#69b9f2)
U 3 (#749153)"""

    sample = """R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"""

    data = if inputname == "sample" then sample else input 

    lines = data |> String.split "\n"

    instructions = lines |> List.map parseLine

    chunkSize = 16

    positions = instructions |> digLagoon |> adjustLagoon
    trench = positions |> Set.fromList

    trenchChunks = trench |> Set.map (\(x, y) -> (x // chunkSize, y // chunkSize))

    startPoint = findStartPosition positions

    (xStart, yStart) = startPoint

    (widthInUnits, heightInUnits) = findDimensions trench
    maxUnits = Basics.max widthInUnits heightInUnits
    maxDim = 800
    unitSize = maxDim // maxUnits

    basin = { startPoint = startPoint 
            , trench = trench
            , chunkSize = chunkSize 
            , trenchChunks = trenchChunks
            , explorationPoints = Set.empty |> Set.insert startPoint
            , filledPoints = Set.empty
            , filledPointsChunks = Set.empty 
            , filledPointsUnmovable = Set.empty
            , fillInside = True
            , widthInUnits = widthInUnits
            , heightInUnits = heightInUnits
            , cubicMeters = 0 }

    debugText = "(" ++ String.fromInt xStart ++ ", " ++ String.fromInt yStart ++ ")"

    model = { basin = basin 
            , mousePoint = startPoint
            , running = False
            , unitSize = unitSize
            , debugPoint = ""
            , debug = debugText }
  in 
    (model, Cmd.none)

-- UPDATE

findNeighbourPositions : Position -> List Position 
findNeighbourPositions pos = 
  case pos of 
    (x, y) -> [ (x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1) ]

isTrenchPoint : Set Position -> Position -> Bool
isTrenchPoint trench pos = 
  Set.member pos trench 

isFreeSpace : Set Position -> Position -> Bool 
isFreeSpace trench pos =
  isTrenchPoint trench pos |> not

isWithinBounds : Int -> Int -> Position -> Bool 
isWithinBounds width height (x, y) =
  x >= 0 && x < width && y >= 0 && y < height

updateBasin : Basin -> Basin 
updateBasin basin =
  if Set.isEmpty basin.explorationPoints then 
    basin 
  else 
    let 
      trench = basin.trench
      explorationSet = basin.explorationPoints 
      filledPoints = explorationSet |> Set.union basin.filledPoints
      addedPoints = basin.filledPoints |> Set.diff filledPoints |> Set.toList
      exploreNext = 
        addedPoints 
        |> List.concatMap findNeighbourPositions 
        |> Set.fromList 
        |> Set.filter (isWithinBounds basin.widthInUnits basin.heightInUnits)
        |> Set.filter (isFreeSpace trench)
      cubicMeters = if basin.fillInside then Set.size trench + Set.size filledPoints else Set.size filledPoints
    in 
      { startPoint = basin.startPoint 
      , trench = trench
      , trenchChunks = basin.trenchChunks
      , chunkSize = basin.chunkSize
      , explorationPoints = exploreNext
      , filledPoints = filledPoints
      , filledPointsChunks = basin.filledPointsChunks
      , filledPointsUnmovable = basin.filledPointsUnmovable
      , fillInside = basin.fillInside
      , widthInUnits = basin.widthInUnits
      , heightInUnits = basin.heightInUnits
      , cubicMeters = cubicMeters }

updateModel : Model -> Model
updateModel model = 
  { model | basin = updateBasin model.basin }

countTrenchCrossings : Set Position -> Position -> Int -> Int -> Int 
countTrenchCrossings trench (x, y) alongside crossings = 
  if y < 0 then 
    crossings 
  else 
    let 
      pos = (x, y)
      left = (x - 1, y)
      right = (x + 1, y)
      next = (x, y - 1)
    in 
      if Set.member pos trench then 
        -- Check left and right.
        if Set.member left trench && Set.member right trench then 
          -- Regular crossing. Increase and continue.
          countTrenchCrossings trench next 0 (crossings + 1)
        else if Set.member left trench then 
          -- Either beginning or ending of passing alongside wall. 
          if alongside < 0 then 
            -- Already passing alongside wall, ended in no crossing.
            countTrenchCrossings trench next 0 crossings 
          else if alongside > 0 then 
            -- Already passing alongside wall, ended in an crossing.
            countTrenchCrossings trench next 0 (crossings + 1) 
          else 
            -- Start passing alongside wall. Started left, expect right for crossing.
            countTrenchCrossings trench next -1 crossings 
        else if Set.member right trench then 
          -- Either beginning or ending of passing alongside wall. 
          if alongside < 0 then 
            -- Already passing alongside wall, ended in a crossing! 
            countTrenchCrossings trench next 0 (crossings + 1) 
          else if alongside > 0 then 
            -- Already passing alongside wall, ended in no crossing.
            countTrenchCrossings trench next 0 crossings
          else 
            -- Start passing alongside wall. Started left, expect right for crossing.
            countTrenchCrossings trench next 1 crossings 
        else
          -- Must be passing alongside wall. Keep going.
          countTrenchCrossings trench next alongside crossings 
      else 
        -- Just empty space. Keep going.
        countTrenchCrossings trench next 0 crossings

isStartPointInsideTrench : Set Position -> Position -> Bool
isStartPointInsideTrench trench (x, y) = 
  let 
    crossings = countTrenchCrossings trench (x, y) 0 0
  in 
    1 == modBy 2 crossings 

findNeighbourPointInsideTrench : Set Position -> Position -> Position
findNeighbourPointInsideTrench trench (x, y) = 
  let 
    candidates = [ (x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y) ]
  in 
    candidates |> List.filter (isStartPointInsideTrench trench) |> List.head |> Maybe.withDefault (x, y)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      if model.running then 
        (updateModel model, Cmd.none)
      else
        (model, Cmd.none)
    Click ->
      let 
        basin = model.basin
        replacement = 
          if model.running then 
            -- Stop and reset.
            { basin | filledPoints = Set.empty, cubicMeters = 0 } 
          else 
            -- Setup for start.
            let 
              startPoint = 
                if Set.member model.mousePoint basin.trench then 
                  -- Find neighbour that is inside the trench!
                  findNeighbourPointInsideTrench basin.trench model.mousePoint
                else 
                  model.mousePoint
              explorationPoints = Set.empty |> Set.insert startPoint
              fillInside = isStartPointInsideTrench basin.trench startPoint
              -- TODO : Check if start point is inside or outside, set fill color as appropriate.
              -- Just go north until y = 0 and count trench crossings.
            in 
              { basin | startPoint = startPoint, explorationPoints = explorationPoints, fillInside = fillInside }
      in 
        ({ model | running = (not model.running), basin = replacement }, Cmd.none)
    MouseMove (x, y) -> 
      let 
        xInt = round (x / toFloat model.unitSize)
        yInt = round (y / toFloat model.unitSize)
        mousePoint = (xInt, yInt)
        txt = "(" ++ String.fromInt xInt ++ ", " ++ String.fromInt yInt ++ ")"
      in 
        ({ model | debugPoint = txt, mousePoint = mousePoint }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every delay (\_ -> Tick)

-- VIEW

toTrenchBox : Int -> (Int, Int) -> Html Msg 
toTrenchBox unitSize (xPos, yPos) = 
  let 
    xVal = unitSize * xPos
    yVal = unitSize * yPos
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt unitSize) 
      , height (String.fromInt unitSize)
      , fill "red" ]
      []

toTrenchChunkBox : Int -> Int -> (Int, Int) -> Html Msg 
toTrenchChunkBox chunkSize unitSize (xPos, yPos) = 
  let 
    xVal = chunkSize * unitSize * xPos
    yVal = chunkSize * unitSize * yPos
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt (chunkSize * unitSize)) 
      , height (String.fromInt (chunkSize * unitSize)) 
      , stroke "black"
      , opacity "0.5"
      , fill "purple" ]
      []

toFilledBox : String -> Int -> (Int, Int) -> Html Msg 
toFilledBox fillColor unitSize (xPos, yPos) = 
  let 
    xVal = unitSize * xPos
    yVal = unitSize * yPos
  in
    rect
      [ x (String.fromInt xVal)
      , y (String.fromInt yVal)
      , width (String.fromInt unitSize) 
      , height (String.fromInt unitSize)
      , fill fillColor ]
      []

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    svgWidth = (model.unitSize * model.basin.widthInUnits) |> String.fromInt
    svgHeight = (model.unitSize * model.basin.heightInUnits) |> String.fromInt
    trenchBoxes = model.basin.trench |> Set.toList |> List.map (toTrenchBox model.unitSize)
    trenchChunkBoxes = model.basin.trenchChunks |> Set.toList |> List.map (toTrenchChunkBox model.basin.chunkSize model.unitSize)
    fillColor = if model.basin.fillInside then "orange" else "gainsboro"
    filledBoxes = model.basin.filledPoints |> Set.toList |> List.map (toFilledBox fillColor model.unitSize)
    elements = trenchBoxes ++ filledBoxes ++ trenchChunkBoxes
  in 
    svg
      [ viewBox ("0 0 " ++ svgWidth ++ svgHeight)
      , width svgWidth
      , height svgHeight
      , Svg.Events.onClick Click
      , onMove (.offsetPos >> MouseMove)
      , Svg.Attributes.style "background-color:white" ]
      elements

view : Model -> Html Msg
view model =
  let
    s = toSvg model
    txt = model.basin.trenchChunks |> Set.size |> String.fromInt
    txt2 = String.fromInt model.basin.chunkSize 
  in 
    Html.table 
      []
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "36px"
              , Html.Attributes.style "padding" "16px"]
              [ Html.div [] [Html.text "Advent of Code 2023" ]
              , Html.div [] [Html.text "Day 18: Lavaduct Lagoon" ]] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "36px"
              , Html.Attributes.style "padding" "24px"] 
              [ Html.div [ Html.Attributes.align "center" ] [ s ] 
              , Html.div [] [ Html.text <| if model.basin.cubicMeters > 0 then (String.fromInt model.basin.cubicMeters) ++ " m³" else "Click to start!" ]
              , Html.div [] [ Html.text <| txt ]
              , Html.div [] [ Html.text <| txt2 ]
              ] ] ]
