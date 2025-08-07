module Aoc24 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

defaultTickInterval : Float
defaultTickInterval = 500

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample

type alias Hecs = (Int, (Int, Int))

type alias Pos = (Int, Int)

type Move = W | NW | SW | E | NE | SE

type alias TileMoves = List Move

type alias InitializingModel = 
  { currentTile : Maybe Hecs
  , movesLeft : TileMoves 
  , tilesToPlace : List TileMoves
  , tilesPlaced : Set Hecs 
  , tickInterval : Float 
  , paused : Bool 
  , finished : Bool 
  , debug : String }

type alias AnimatingModel = 
  { blackTiles : Set Hecs 
  , paused : Bool 
  , days : Int
  , debug : String }

type State = 
  Initializing InitializingModel

type alias Model = 
  { dataSource : DataSource
  , state : State
  , debug : String }

sample : String 
sample = """sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew"""

input : String
input = """swswswnwnweseseseenwwswsenewseseseswsw
nwwsweswswseswswswwwwswwswwswew
ewswnesenenwenenesenenenwnewe
newsewneesewseseeseseseesewewsese
enwseewneeesweeeenwesenewsene
nwnwnwnwsenwnwswnwnwnwnwenenenwnwnwneww
swswswwwswswseswseeese
seesewseeseseeeseseesesese
eeeneneeneeeewneseeenenenweew
seseeseseseeenweseswsesewewsesesenene
nwnenenwwnenwswsenwnenwsenwnwsewseswnene
nwnwnwneswnwsenwnwwnwswnwnwnwenwnwnwe
swwswswwsweswswswswwswswsw
wseseeseseneswenweseweesenweswe
nwneenwnenwnwnwseswnenwnwseswnenenwnwwne
seseswswwswswswwswnesenwswseswswneeswswsw
swsenenenwswseneswswswwwnwswwesewenw
swnewneeeweeenenwneeeseeewew
esesenwswnwsenwswswseswnweswsenwseee
swnwneswswnwswsesweseenwsweswnwswswsw
eswswnwesewwnenesesenewneweswwswsw
nwneneneneneneseneeneneeeneswnenenewe
eneneswsenwsweneswnwneswnenenewnenenwswe
ewwwwwewwwwwswwwswwwnwsw
seeseseenweeeneeseeeseswesesesw
neeeneneeenenwseneswwnwnenweneneswe
wwneenwwnweweneeeswewseseneee
nwnwwwwsewnwwwnwnenwnwwnww
sewweneeneneewnwnenenenenesenesew
wwswnwnewwwwnwnww
wwswwwweswsenwne
swneewnweseseenwseeeseeseseesenwne
ewesenweeeseswnwese
sesweenweseseseswenweseseseseeseee
seseseseseeeseeseewswsenenwsesesese
eenwsweswnwwnwnwswswswswseswswswswswsw
eeseeeeswseeenenweeeeeee
nwwwwwnwnwnwnwswwenwnw
eneswsenweesenwseesweseswewwseee
eeeweeseesene
wwnwnesewnwnwwwwnwenwnwnewsenwnwnw
eseeeswneeeeeenwneenee
neneneneneneneewnenwnenwne
seswsenwseseswseseswswsewnesesesesesese
neesenwseswsesesesenwswsw
swnweweweseneseeweswnwsewswnenwsw
neswneneswewwnwseseswseesesesesesenesw
swnwnenwnenwnenwnwneseenesewnenenenese
sesenwseswsesesenwsweseswwswswsesese
neeseswswseseneeenesesesewesenewsese
swnwnwwswnwweneenwwswnenewenwsese
esenwnenenenewseswneene
eneswnwweswnwwwnwnwwswenwwseene
sesenewsenwswwseeesenwseesewswsenenwne
nenwnenwnenenenwsenenewnenene
sewwwswwwwwsewnewwwwwnewnew
nwneneneneswnewwneenenesenwwneneseene
seeeseseseseseseseeesewesese
newnesewnesewswnewwwsesewenewnwnwsw
eseswseseseneseswsesew
wewnwswswwswswswswwwwwsw
eseseeseeswesenwsese
wnwenwwnewwwnwwswwwwsewnw
wsenwnwnenwwwsenwwnewswwwwsenenwww
wswnwsenenwwnwseewwwswnenwnewww
seeeneenesweeesweenw
eeswnewwnweneneseneeneenesee
sesenesesesesesenwsesesesesenwsesesesesesw
wswnenenesewneseenwnwewweewsew
swnwneneeeeeneeeneweenweneswe
seeeseeweeeewneeseesenwseseee
sesesesesesenesesesesenwseseseseseseseswnw
neseeeeeeeweswseeeeeewsee
nwnwnwnwnwwswnwwnwnweswsenwnwweenw
wnwnwwnenwwnwnwnenwnwnwsesesenenenenwnw
senesesesesesesesesesesesesewse
swswseswswseneswswenwswsenwwswsenwswsw
neneeeeneeeeeswneeeenene
nenwneneseneneneseswnenwneenenewnesenene
swswneneneneweseenene
wenwsenewnewnwsenwenwwswswswswseswsenw
wneswwnwewewwwwwswswwwwww
sewsenwseseseneneseseseseswswseswseesese
nenenenwseeeeneneneesw
seseseeesewnwsweeseseeseesewenesese
swnweseeswwwswswnw
nwewewwnwswswwwswweswswswwneswsw
nwneneeneneeswwswsenwseneswnwswnewnene
seneneeseswesewswswswnwwswswnwnwseswnew
seseswsewswsesenwswsesesesesesesewnesenesw
wwenwnwwnwsenwnwnenwwwnwnwsenwwne
seswswseweneswneseswwwesweseww
senwsesesesenwseswseseseseseeswseesesee
eeeewseeeseee
weeswseswenenwneeeeneeseseeswe
swswwswseswsenwswsewnesenwneswseeswsese
swwwseswwsweswswsewswswswnwnenwswsw
swswswswneseswswneswswswswswseswneswswswsw
wnenweweeeweneeseeeeeswwne
newsweesenewnwesweenesweneneene
swswswswneseswwswswewwnwswwswwswwse
swseseseswsesesewseseeneseswsenwseseswse
senwwnwneesenenwnwswnwnwnesenwnenwnwnw
sweneswewswneeenweewneeeseenwsw
swesenwwswnwswenwnwswswswswswsewneswsw
swsenenwswwnwswseswswswesesesesw
neneenwseswsesesesweseseswseseesenwse
nenwnwnwwnwnwsenenwnenwnwnwnenene
nenenwswnwnenwnwnwnwnwnwnwnwnwneenwnwse
seeenweseseswweeseenesesesweenese
seeeneeesenewneneeneneeenenewnee
nwnenwnewnenenenesenenenenenenenenw
swswseswseswswswnwsenesw
swswsesewseseswswsesesweswwswswswnesenesw
wnewswneswwswswwswswswwesweseseswnw
newsenwnwsenwsesewsewnenwwwwswwsw
nwseewwwsenwwnwnwwwseenw
sweeesweeneneeeenweenweeneee
swnenewwneneneneneneseneneneneseneseswwne
eeswnwewneenweneeswse
seseseseseseswseswswseseesenwsesesenw
seseseseseseswsesesesesesenesese
nenenenwnenenwneenwnenenwnenwneswnesew
nwnesesenenwnwswswnenenwnwwnwwnenenwneene
senwnwnwnwsewwnwnwnwnwnwnwnenwnwwnwnw
wwwwsenwwwewwneswwwwwwww
swswswswnwswnwseswsesweswswswswswnwnweswsw
neeneneneeneseneeneenewenene
seseeseweeseseseeseeese
nenwnwnwsenwswwenwnwnwnenenwnwnenenwnw
nwnwenenwnwnenwnwnenwnwnenew
senwwnwswenwenenewnene
nwnwnwenwswwnwenwwwswnwnwnwenwnwnw
swneswswweswwswswswswsweswwswswwwsw
nwnwnwnwswnenwnwnwnw
newswnenenwsesenenwsewswnesw
neswswnwnesesweswwwnwnwseeswnewnesw
nwenwneeneeneseneseneene
swwswswswsweewswenwswnwswwse
seseseseesweseseeeewnweneseseee
weneeneeeseneeeneeneeswnenwnenene
swnweenwnwnwnenwnwnwnwnwnewnwnwnwnwnw
swwnwwwewswwenwnwwwswwsee
swnwnwseeswswseswsweswswswswsw
nwswwnewewwwwsewwweneswwwsw
seneenewwswswswsenwwswnwswwswwne
swsweswnwswswswswswswswswwneswswswesw
wwswswwwswwwwwnwneswswswsewwnew
nwenweswneswneeswneenwneneneeeee
senewnenenesenenenewneseneswwnenenenenene
wsenwewseneswseswnenwswswnesewneeeese
eenwnwesweswnwsenwwnwnwswnewneswe
wnwsewwnwnwwnewsesewnwwewwswnese
nwswneswswswwswwsesewnwseswwswnwswesw
eweeneeeseeeswneeeseweeeneee
eswswseswseseseswswwnwswswsw
weswwswswseswswnwswnwnwseswnweswee
swswswnwswseswswseseswswswnesw
neneeneneneswneneneneneneneswnewnwswsene
enwswneneneneswnwsweswnesweswenwwsw
nwsenwnwswnwnwnwneenew
weseeswwnweneewsweeswseeneenwe
eswsesenwsweseswswswswswnwenwswwsw
neswnweseneenenwneneswnwnenenwnenenwnene
nwwseseneeseseseswseswwswwneseeesese
nwseswnwnwenwnewnwnwnwnwnwnwnwnwnwnwwnw
sweseeneenesesweneeenweeeenwneew
newnwweswesesenw
eseneesweesweseweeeeeewnesene
nesenwnwnenwnwnwnenwsewsenwnwnwnwnwnene
sewnenenenenweseenwneneswnwne
swswswswswswswwwwe
neneneneneseneenenenenewenee
newseeeeseseesweesewnweeeene
nwswewnenwnwnwnwneswwnwwnwneneenweee
swswnwwswnewwswsewwwesewnwwww
ewwwswwwwswwwwwwneswwseww
nweneseneenewneneenwwwnewnenenwnw
eseeewseswnwneseeneeew
nwseenwnwnwsenwsenwnwnwnwnwnwnwnwwnwwwne
neeeswnweneeswneneeneneeneeeswene
wswesenewswwwswswswnenwswwwswnwsw
sewnwnwwwnwnesenwwnwwsewwwsewnese
eeeeeesweeneeeeseweeneee
wnesewwewwnwnwnwwwwnwwwwnww
wwseswswswwwweswswwsesenwneswwnwsw
senwenenenenwwneneneneswnenesenenenenenew
seswswwseswneseswwswnesweswswswwswsw
wnwnwswwwwswseswwwsewe
nenenenwswnenewnwnenenenenwneneneenesene
eesenweweseeeseeeweeeswsee
swewnweeneeneenese
eeeeesweeneneweeeweeeee
nwsenesenenwwwsewnwneswenwnwnwnwsew
swswswswnwswseeswswsesewsenesw
swsewwswseeswneswsesweswseswswseswswsw
enewwswnwswneeseneneneenenwnese
eneewewseeeeeneeneenewe
neneeseswnwwnwseseeewwsesesesw
senwnesesewseseseseeswsesesesesesesesewse
nenenwnwnenwnesenwwnwnenenenwnwnenw
senwnwswesesewswseswseneswswsw
nenwnwenwnwwwnwnwnwnwnwnwwnwnwnwswsw
neseswswnwseswswswswswsw
seeeseeseseeeseswnwseenesenwsewswse
swswwswswswneswswseswswseswneswseswse
swwwswswwwwnwwewswsweseswnwswsww
wswnwenwwnweneseewnwnwswnwswswwnew
nweswnweeneswneseeeeneswsweswneese
seeeeeeeenwsweeeeesweneee
wseswneswswswewwwwswwwwswswswnwsw
swnesenenenwnwnwnene
nwnwnwnenwnenwnwewseenwnwswnwwnene
sesesenwnwsenwsewnesenwseeenwsw
ewneseswswswswweweswswseswwneswswse
eeseesenweenweeeeeneeewesese
sewnesenewneweneneneweneneeneneswnee
swswswneswswwsewneswnewsesweenwswswswsw
swswnwswweswseseswnwsewnwnwseneesenesesw
nwnwnwnwswnwwnwnwnwnwenenwnwnwnwnwenw
nwswnwwswnwnwnwsewnwwnwnwwnwwwnenwenw
neswwswseswneswsweswnwswwswswswwswswsww
wwneswwsewnewwnwnwwwwwwnwnww
nenwneenenewneswnenenenenenenenenenwe
seswseenwwnewwenenwneswnwseseswswsese
swwesesenwseseneseswswsesenwsesenesese
swsweseseswwsesweneeseswsewnwswnese
nwwenwwswwewwnweswnwsewswwww
nwnwnwnwnwnwnwwnwenwenwwenwnwnwswnw
wwwnwnwsenwsewwswwwnese
senwneneweseseeseesesw
neneeeenenweswenesweeeeeneee
eeeeeeeeesenw
nweenewwsenwwnwnwswnwseenwwwnwnww
neeneseneseneneenenenwwewsw
nweenwewenweeeesesweswnenenenene
enweswnwenwnwwseneswnenwnwwnesenenwnw
wwwwwwwwwswsenenewse
neswwwswswsweswswswwneeseswswnwesw
eseseeneeeseenwsenweesesesweesesw
nenenwnesewsenewnenenwnenenenenesenenesew
swwwswswwswswswseswsewnewwneswswwsw
neeswneeenewenesesweeneenweenee
enwnwewseseeseswswswsenenwswseswsesenwsw
eweeswesweenweseweneneeeesee
weneswswwwnewseeewswseseesewe
nwsewneneneswesewwsenwenwenesewesw
neseswswwsesenwsesesesesene
nwnwwwnenwsewnenwwnwnwswwwnww
wnwnwnwwnesesewnwnwwnwnwnwnwnwnwww
senwswwswnewwnewwwwswwwswnwsweww
wwswnwwswnwseeswswneswseneewswwesw
swsenwswswswenwwswnwsweswswwsweswswswe
eseeeeesenweeenweeeseeeneenw
nwswseswseswswnweswsese
esenwswweeeeswsesenee
neneswneswswwnwsenwswseswswnesweswsewwnw
nwnwsenwnwnwnenwnwnwne
enesesewseseseseseseseneseseseswsesee
neeseseseseseesenwswnwswswwswseseswswsw
wsenenwewswenenwwwenenwnwesewsw
seenewsesesesesesesesenwsenenwseseswsenw
swsenwswseseswseseenesesesesesesesese
neenenenenenenewenewsenenesesenewnene
eneneseeeeswnwneeeeeeweeeenwse
eeeswewsweeeswnenweeneewnwew
wswwswwnewwwswswswsewswwnwwnew
nesenwseseeseeswseswsenewenwseswnewsene
seeesenwswseseenweswnwnesweewnwsw
eneeeeeeeweee
sewewnweneswswnwnenewneswswseesee
seswnenesewwnwnesenwnewnenenenesewnenese
senenenenwnenenenenwnenwnenwne
nwnwwwwwwwwwwnwwenwsesenwnewww
wewnewwwwwwwwwnwseswnwswwse
wnweswswnenwneenenenewnwnenenwewnwsw
wwwswwwwweswnwwswwwenw
nwwnenenwenenwnwswswenenwnweneswnwnwsw
swneseswseseseswsweswnewswnesenenwnesw
swsesenesweswneeeeesweeenwnwesee
wnwwnwsenewwwnwwnwnwnwnwnwww
nwnwwnwnwnwnesenwnwsenwwnwnwnwnwneenw
neseeseesesewswswswseswneswsenw
wnwwnwwenwwwwwwnwnwnewwsenww
wnenenenwnenwnenenenenenenesee
newwsenesewweneseweseswneewwsenw
wswswwseseswswneswnwwnwswseswswwnw
neswnenenenenenenwnenenenesenwseneenenwne
nwwneswnweneneenwnwnwnene
neseswesewnewenew
wwwwwwwnwwsewwwwneww
wnwnenwewenwnwnwnwwsewnwnwwswwwnww
wwweewwewnenewsenwsenesenwswsw
swwswwwwnenweneneswsesenwnenewseseww
ewwnenenenenesewneneneswnenesesenene
swewwswswswseneswswswseneswneswwswswse
enenwswnwwneeswweswwewwwseesenw
swnenwswnwwsewnwswnwewesenenewwswww
neenwnenwnwnwwsewnwnwseswwnwenwnenese
ewesewesenwseseneswneweeswneneesw
neneswenenwweneeeneneneneeneneee
wnwnwnwwwewwneswwwnwewwswnwse
neesweeeneswnwwenewneeseeneseene
eneeeweseewe
sewswenweeneeeesenweeeeeeesw
nenenenwnenwnenenenwwsenenewenenwnenw
sesewwwwnewwswwnwsw
senewneneneneeneneenenenenenenewe
newnwnesenenenenenenenenesenenesenewne
swnwseneseseseseseswswswswneseseswswwse
nwneeenewswsweneeeswweseesesesenw
neeeewwnweseenesewneeneneswnenesw
neswenwseeseeseseneesewsenwnwseee
esweeenwnwsenwswswseswnweneseeesee
seeseseseeswwsewesewnesenenesesee
eseeeweswswseweneseeeenwsesenwse
nenenwneneswnwswnwswsenwnwnenwnwnwnwsew
wseswwwnwwewnwswwswwswwswswsenw
wnwseswwwwwwwsewwwwswne
weswswewnwnwwwwnewwwnwwwswwnw
senenenenwnewnenenene
wneenenwseneswnwnwwsenwnwnwnwnwnwnwnw
nenwsenenewneneesenwswnenwnwwwsenenene
swswweeeeneeneenweeeeewnwe
enewnwewswnwnwwesenwnwnenenenw
eeseswenwseseeeeesese
neeswseswswsesenwswnwseeswnwswseesew
sewwwswwwwwwsewneswwenewnese
swswswswswswwneseswswswswswswwnwsewsw
wwwewnwwwsewnwwwnesewwwswwnw
neeeneeeenenenenenwese
wwsenwnwsewnenewwwsewwwwswwne
eswwnenwseenwnwswnwswnwewsenwwnwwwe
eseeeseeewswneeesenesese
seeseseseseeseseseseseesesenwnweesw
seesesewseswseeseseenwnwsesesesesee
ewwswswnewswnewswswwswwnewwwsew
swswswswseswswswswwswneswswwswwswnesw
nwnesewwwwwwnewwnwwswnwwwnesw
seswnwesesesesenwswseseswswsenw
wswswswnwnenewsesenwewsesewswnwswwww
nwnenwseneswsenwneenewsesenenwnwewnwnwsw
nwseseeeseseseeseseeseee
swnesenwswsenwesewswwseseseswneneeswsw
weswnwnwnenwenwsenwwnwnw
seeeseseeeseeseswwsesesesesenwsese
seswnweswswswswenwseswseneeswswnwseswsw
wwwwwwenwwwwww
eswswsewnwseseswseseswseswwseeswsese
nenwnwnwnwneswswswnenewnenenese
wsewseswwwwnewwwwwneweneww
wseswswenwnwswwseswswneeseewneneenwne
eeswneenesenwsweenwseeswseeesee
swneswswswswewswswswwswwneswswwwnesw
eswwwseeeneneeswnwwnweneneeneesw
neswenenweseswnwneneswsweswnwsewnwne
nenewseseneseswneswnwnwwsewwseswnwe
eeneewwwwnesweeseesewsene
wwwweneneenwwwwsewsewsewwe
neswseseseneeswswswswseswsweswwsewswse
wwswwewewwwwnwswewwwwwww
seseseseneeeseseswswsesesesewnenenwsesw
swnwnwnwwnwnwnwnwnwwnesewnwesewwnw
swsweswswswnwneswswseswswnwswsweswwwsw
nwweeneswneswwwnw
wwnenwwnwsenwnwnwnwww
wwnwwwnwwwwwnwwwweewwwsw
esewwwnwnwnwwwseswnwseseswnewseswwne
nesesesesewseseesesesee
seseseswseweseseswseesesesewswswnwese"""

parseTileMoves : String -> TileMoves 
parseTileMoves s = 
  if String.isEmpty s then []
  else 
    case String.left 1 s of 
      "w" -> W :: parseTileMoves (String.dropLeft 1 s)
      "e" -> E :: parseTileMoves (String.dropLeft 1 s)
      _ -> 
        case String.left 2 s of 
          "nw" -> NW :: parseTileMoves (String.dropLeft 2 s)
          "ne" -> NE :: parseTileMoves (String.dropLeft 2 s)
          "sw" -> SW :: parseTileMoves (String.dropLeft 2 s)
          _ -> SE :: parseTileMoves (String.dropLeft 2 s)

initTileMovesList : DataSource -> List TileMoves
initTileMovesList dataSource = 
  let 
    data =
      case dataSource of 
        Sample -> sample
        Input -> input
  in 
    data |> String.split "\n" |> List.map parseTileMoves

initTile : Hecs 
initTile = (0, (0, 0))

initInitializationModel : DataSource -> InitializingModel 
initInitializationModel dataSource = 
  let 
    tilesToPlace = initTileMovesList dataSource 
  in 
    { currentTile = Nothing 
    , movesLeft = []
    , tilesToPlace = tilesToPlace
    , tilesPlaced = Set.empty
    , tickInterval = 50
    , paused = True 
    , finished = False
    , debug = "?" }

initModel : DataSource -> Model
initModel dataSource =
  let 
    state = Initializing (initInitializationModel dataSource)
  in 
    { dataSource = dataSource
    , state = state
    , debug = "?" }

init : () -> (Model, Cmd Msg)
init _ =
  let 
    model = initModel Input
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Faster 
  | Slower 
  | Step 
  | TogglePlay 
  | Reset 
  | UseDataSource DataSource

updateReset : Model -> Model
updateReset model = initModel model.dataSource

adjacent : Hecs -> Move -> Hecs
adjacent (a, (r, c)) move = 
  case move of 
    E -> (a, (r, c + 1))
    SE -> (1 - a, (r + a, c + a))
    SW -> (1 - a, (r + a, c - (1 - a)))
    W -> (a, (r, c - 1))
    NW -> (1 - a, (r - (1 - a), c - (1 - a)))
    NE -> (1 - a, (r - (1 - a), c + a))

toMoveStr move = 
  case move of 
    E -> "E"
    SE -> "SE"
    SW -> "SW"
    W -> "W"
    NW -> "NW"
    NE -> "NE"

updateInitializing : InitializingModel -> InitializingModel 
updateInitializing initializing = 
  case initializing.currentTile of 
    Nothing -> 
      case initializing.tilesToPlace of 
        [] -> -- Nothing more to do!
          { initializing | finished = True }
        nextMoves :: restList -> 
          { initializing | tilesToPlace = restList, movesLeft = nextMoves, currentTile = Just initTile, debug = "->" }
    Just tile -> 
      case initializing.movesLeft of 
        [] -> 
          let 
            currentlyPlaced = initializing.tilesPlaced
            nextPlaced = 
              if Set.member tile currentlyPlaced then 
                Set.remove tile currentlyPlaced
              else 
                Set.insert tile currentlyPlaced
          in 
            { initializing | tilesPlaced = nextPlaced, currentTile = Nothing }
        move :: restMoves -> 
          let 
            moved = adjacent tile move
            moveStr = toMoveStr move 
            debug = initializing.debug 
            dbg = if String.isEmpty debug then moveStr else debug ++ "-" ++ moveStr
          in 
            { initializing | currentTile = Just moved, movesLeft = restMoves, debug = dbg } 

-- let findWhiteTiles (blackTiles : Set<Hecs>) : Set<Hecs> = 
--     let neighbours = blackTiles |> Set.toList |> List.collect getAdjacentTiles |> Set.ofList 
--     Set.difference neighbours blackTiles 

-- let evolveStep (blackTiles : Set<Hecs>) = 
--     let whiteTiles = findWhiteTiles blackTiles 
--     let remainsBlack (t : Hecs) : bool = 
--         let count = getAdjacentTiles t |> List.filter (fun a -> Set.contains a blackTiles) |> List.length 
--         count = 1 || count = 2 
--     let becomesBlack (t : Hecs) : bool = 
--         let count = getAdjacentTiles t |> List.filter (fun a -> Set.contains a blackTiles) |> List.length 
--         count = 2 
--     let black1 = blackTiles |> Set.filter remainsBlack
--     let black2 = whiteTiles |> Set.filter becomesBlack
--     Set.union black1 black2 

findWhiteTiles : Set Hecs -> Set Hecs 
findWhiteTiles blackTiles = 
  let 
    neighbours = blackTiles |> Set.toList |> List.concatMap getAdjacentTiles |> Set.fromList 
  in 
    Set.diff neighbours blackTiles

remainsBlack : Set Hecs -> Hecs -> Bool 
remainsBlack blackTiles tile = 
  let 
    count = tile |> getAdjacentTiles |> List.filter (\t -> Set.member t blackTiles) |> List.length
  in 
    count == 1 || count == 2 

becomesBlack : Set Hecs -> Hecs -> Bool 
becomesBlack blackTiles tile = 
  let 
    count = tile |> getAdjacentTiles |> List.filter (\t -> Set.member t blackTiles) |> List.length
  in 
    count == 2 

evolveStep : Set Hecs -> Set Hecs 
evolveStep blackTiles = 
  let 
    whiteTiles = findWhiteTiles blackTiles 
    black1 = blackTiles |> Set.filter (remainsBlack blackTiles)
    black2 = whiteTiles |> Set.filter (becomesBlack blackTiles)
  in 
    Set.union black1 black2 

updateStep : Model -> Model
updateStep model = 
  case model.state of 
    Initializing initializing -> 
      { model | state = Initializing (updateInitializing initializing) }

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  case model.state of 
    Initializing initializing -> 
      let 
        paused = not initializing.paused 
      in 
        { model | state = Initializing { initializing | paused = paused } }

updateDataSource : DataSource -> Model -> Model
updateDataSource dataSource model = 
  initModel dataSource  

updateFaster model = 
  case model.state of 
    Initializing initializing -> 
      let 
        tickInterval = initializing.tickInterval / 2
      in 
        { model | state = Initializing { initializing | tickInterval = tickInterval } }

updateSlower model = 
  case model.state of 
    Initializing initializing -> 
      let 
        tickInterval = initializing.tickInterval * 2
      in 
        { model | state = Initializing { initializing | tickInterval = tickInterval } }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset -> 
      (updateReset model, Cmd.none)
    Tick ->
      (updateStep model, Cmd.none)
    Faster -> 
      (updateFaster model, Cmd.none)
    Slower -> 
      (updateSlower model, Cmd.none)
    Step ->
      (updateStep model, Cmd.none)
    TogglePlay -> 
      (updateTogglePlay model, Cmd.none)
    UseDataSource dataSource -> 
      (updateDataSource dataSource model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of 
    Initializing initializing -> 
      if initializing.paused then Sub.none else Time.every initializing.tickInterval (\_ -> Tick)

-- VIEW

getAdjacentTiles : Hecs -> List Hecs
getAdjacentTiles tile = 
  [ E, W, SE, SW, NE, NW ] |> List.map (adjacent tile)

getSPoint : Float -> (Float, Float) -> (Float, Float)
getSPoint size (x, y) =
  (x, y + size)

getSWPoint : Float -> (Float, Float) -> (Float, Float)
getSWPoint size (x, y) =
  let 
    w = size * sqrt 3
  in 
    (x - w / 2, y + size / 2)

getSEPoint : Float -> (Float, Float) -> (Float, Float)
getSEPoint size (x, y) =
  let 
    w = size * sqrt 3
  in 
    (x + w / 2, y + size / 2)

getNPoint : Float -> (Float, Float) -> (Float, Float)
getNPoint size (x, y) =
  (x, y - size)

getNWPoint : Float -> (Float, Float) -> (Float, Float)
getNWPoint size (x, y) =
  let 
    w = size * sqrt 3
  in 
    (x - w / 2, y - size / 2)

getNEPoint : Float -> (Float, Float) -> (Float, Float)
getNEPoint size (x, y) =
  let 
    w = size * sqrt 3
  in 
    (x + w / 2, y - size / 2)

f2s : Float -> String 
f2s = String.fromFloat

toHexagonElement : Bool -> Float -> Hecs -> Html Msg 
toHexagonElement placed size (a, (r, c)) = 
  let 
    wf = size * sqrt 3
    af = toFloat a * wf
    rf = toFloat r * wf
    cf = toFloat c * wf
    pos = (af / 2 + cf, sqrt 3 * (af / 2 + rf))
    pts = 
      [ getSWPoint size pos
      , getSPoint size pos
      , getSEPoint size pos 
      , getNEPoint size pos 
      , getNPoint size pos 
      , getNWPoint size pos ]
    ptsStr = 
      let 
        str (x, y) = (f2s x) ++ "," ++ (f2s y)
      in
        pts |> List.map str |> String.join " "
    attrs = 
      if placed then 
        [ stroke "none", fill "black", points ptsStr ] 
      else 
        [ stroke "black", strokeWidth "1px", fill "none", points ptsStr ]
  in 
    Svg.polygon attrs []

toInitializingElements : InitializingModel -> List (Html Msg) 
toInitializingElements initializing = 
  let
    currentTile = initializing.currentTile 
    currentElements = 
      case currentTile of 
      Nothing -> []
      Just tile -> [ toHexagonElement False 8 tile ]
    placedElements = initializing.tilesPlaced |> Set.toList |> List.map (toHexagonElement True 8)
  in 
    List.append placedElements currentElements

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    elements = 
      case model.state of 
        Initializing initializing -> toInitializingElements initializing
  in 
    svg
      [ viewBox "-300 -200 600 400"
      , width "600"
      , height "400"
      , Svg.Attributes.style "max-width: 100%; background-color:white"
      ]
      elements

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2020 | Day 24: Lobby Layout"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    count = 0 
    svg = toSvg model 
    playButtonText = 
      case model.state of 
        Initializing initializing -> 
          if initializing.paused then "Initialize" else "Pause"

    blackTilesCount = 
      case model.state of 
        Initializing initializing -> 
          initializing.tilesPlaced |> Set.size

    debugStr = 
      case model.state of 
        Initializing initializing -> 
          initializing.debug

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
              [ Html.div [] [Html.text "Advent of Code 2020" ]
              , Html.div [] [Html.text "Day 24: Lobby Layout" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2020/day/24" ] 
                [ Html.text "https://adventofcode.com/2020/day/24" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "16px" ]
              [ 
                Html.input 
                [ Html.Attributes.type_ "radio", onClick (UseDataSource Input), Html.Attributes.checked (model.dataSource == Input) ] 
                []
              , Html.label [] [ Html.text "Input" ]
              , Html.input 
                [ Html.Attributes.type_ "radio", onClick (UseDataSource Sample), Html.Attributes.checked (model.dataSource == Sample) ] 
                []
              , Html.label [] [ Html.text "Sample" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Reset ] 
                [ Html.text "Reset"]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Slower ] 
                [ Html.text "Slower" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ Html.text playButtonText ] 
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
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px" ] 
              [ 
                Html.div [] [ Html.text (String.fromInt blackTilesCount) ]
              -- , Html.div [] [ Html.text debugStr ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                svg
              ] ] 
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ Html.label [] [ Html.text model.debug ]
            ] ] ]
