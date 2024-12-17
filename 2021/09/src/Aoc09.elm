module Aoc09 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

delay : Float
delay = 500

cellSize : Int
cellSize = 6

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Position = (Int, Int)

type alias Basin = 
  { lowPoint : Position 
  , explorationPoints : List Position
  , filledPoints : Set Position }

type alias Model = 
  { floor : List (List Bool)
  , border : List Position
  , basins : List Basin
  , counter : Int 
  , riskLevelSum : Int 
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

isOpen : List (List Bool) -> Position -> Bool 
isOpen floor pos =
  case lookup2d pos floor of 
    Just border -> not border
    Nothing -> False

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

isBorderPoint : Position -> List (List Bool) -> Bool
isBorderPoint pos floor = 
  case lookup2d pos floor of 
    Just b -> b
    Nothing -> False

findBorderPoints : List (List Bool) -> List Position 
findBorderPoints depths = 
  depths 
  |> List.indexedMap (\y row -> row |> List.indexedMap (\x d -> if isBorderPoint (x, y) depths then Just (x, y) else Nothing))
  |> List.concat
  |> List.filterMap identity

createBasin : Position -> Basin 
createBasin pos = 
  { lowPoint = pos
  , explorationPoints = [pos]
  , filledPoints = Set.empty }

pos2str pos = 
  case pos of 
    (x, y) -> "(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")"

init : () -> (Model, Cmd Msg)
init _ =
  let 
    sample = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
    input = "5656921987125678979998760234965456789998768789239876323457896545467894567987232345679209876567998656\n4349899876234599568987654345894345999987656678945965212348965434356943459876101956798912987459876541\n5498789854345689467898765456789297898999736567899874323489654321248932398765219897987893498598765432\n6989598765459789356989878767896345987432125456789985434578965432357953499954398789376789569679876543\n9875459876569893249876989898965476976521012345699876645689876543456894689875679643234598998789987656\n8764345987678989139765393959796567965432543457789987887792987654569965678976798432123467899899998867\n9773239898789678999878212345697679876543656598999999998901298767678976789598987621012345943978929998\n9652198789894567989992101556789799989854567679659898999992399899789987893459996532136756964569919999\n8543999674923469879989232345899899998765678789798787899989989929896798932345987643245697897679897899\n7659896543212359768978943456789997889876789896987656789878679210945679651267898789976789989998786789\n8798789954563498957767894567899986678989898965432345678964567921234989743456989891988995679876545856\n9987679899874987843456965678979875567999987654321239789443459432345899954589876910999434678995432545\n9877578789989876532567899789765983458999898965410198999321598996476789875678965439876323789789541234\n8765467678999997673458978997654901269998769876322397898945976989987899986789876598765212345678930347\n4323234589989998789569769898769892978987654987439986787899865678998998997899989999874347456789821236\n4310123689878999897678958789898799899987743498598765696789987799569987898999998789985456789897642345\n6421234578956899998989345678987688789996532987699764545698998912499876949998989698976587899998759487\n7834545789345778999997236789876567678987653698789843234567899943989989234997678587989798968789898998\n8945656793234567899876345698765434589998974569898762126789999899879894349876583456899899655698997899\n9658789932123458999765458999654323678909895689987651015897998789965789498765432345699921234987846789\n8769894321012367899876567987721012789219789797896543123456789679874397679876321256789430349876734599\n9889965439143456789987679876432125699998679896987654234899896598765498989997442345678921467965325678\n3999877998956567897898999876543234789896598965498765856789965459876789899998656576789939569876566789\n4599989867898789956789789989854345699765497894349976767897654367987895698999767678997898978987977892\n5689998756999892347898698998768456789754346789234989878998796456798964787899898989445677899298998901\n6899899845890901239986567769877687897673235692123999989449987567899763456789999894323456910149989992\n7998789956791912398765436456998998987542124989239898793234598978999854667899898765212347891239878989\n9898598767989899459954324365679549099993235978998769654395989899898765678998769986323456789398765678\n8767459879876788969843210234568932198789549867989659875989876789769877889987653295434568996987654567\n9856345998765657899764321246789845987699998759878943989878965678954989998998854598546878945699543458\n9943235987654345678976532347899659876587899548767892398767954569893292677999765987657899234598754589\n8632047998775457899876545498998998765456798732455921019655343456789101456899876798768910123459897678\n7542123989896668999987756789787899654349986621434893198743212347893252367967987899979439294998998999\n9753239876998789998999887897656798743298765410123789987654323458954543478945598999989598989897659599\n7654449995979893987899998976545679854349874321245678999995934567895676569123499789999987678789343489\n8765998784569932396998769987437789967656985432356789987889895678976987878994987678999876545679212978\n9899887643458921985489954599545699878787898765467899996579789789987898989789986568998765432389343459\n1998798532467939874367893987657899989898979887598969875468678993098999997656542456989876546456954568\n0129654321256899765459932498768989692929567998989657965376578992129899876542101349876987687767897678\n9298999432345678976578943569879976541012468999876549874212456789234799997653212498765498898878998889\n8987678956756799698689654698998765432123567893986432987301234894345689998765323989874349999989329998\n7676567898767894598798765987969876865238678992195410196513346965576796989865439876543236799893213567\n8543458999879932989899878976845987974347889989954329987325457896678895678976545987754545698764301348\n3212345789989549878942989895434598765456998876899498765436567987989934568987656899865656789965492349\n2101676789397698767891098789423459876667897794768999887687678998999912347899767999876767899877989499\n3212567893219985456932987679554567987798976643456899998798989999129893456789879987987878999989878989\n4323456789998764387899876568965679698899765532345698779899294989098789567894989976598989789998757679\n5434567899899765678989875457976789549989654341234987665989123978987678998903496987439395679986545567\n6545678988789876789976982349897895434678943210129876534578939865654567899212345698921234598765432498\n8766989876578987898995431298789954323567954321239884325989097654523458989423566999890123459878521389\n9877899765467898967984310345698965212456895592398765456789198763012369878954879898791234567987632478\n9989978984358999549865321234567892101569986989989879877998999832125489767895998787679987678996543567\n9898867893267899432979832345698943452978998979878989989567899944345678956789987654567898989987654578\n8767456792125678921098765456789654567889999767467899895456798767856789345991986543456789993398765689\n7654346789334569432399886667896987698996987654348998754345679878969891239890995432545898942109876797\n8743234579456696543989987778975698999785698765459879543234789989878992349789894321234567893299987896\n9652125678967789654976599899654579987654569876767965432126567895989989498658789432356778954989999945\n8721014567898998769865456976543434976543456987879896943013478953997878987545696545467899899878901234\n9832123456789659978954348987632123987654568998998789892134567899876767897676789656578935698767892545\n8743234568995443989875467896541015699879878949998678789255778998765455698797898789678926989659789676\n7654345789654321295996568987432123456998989439876589678969899999654364759898919899799019976545689797\n8965676898743210134987699898943296587897998919887434567899989788963213345999323999898998965434568989\n9876789997654329245698789789894989799986897898776623456789876587892101296789454599987987654323679979\n4987995789975998956789895699789678989875766987654512345696567456789742679896567989876398766554789568\n3298954679899887967899934987654567878964345696543203456965435345897653498987979978964239987667895457\n9989763456798776798989429898543476567895256987653212569896321256789776567899898769879125698998912345\n8765432345987654789878998769452323479792123498765323456789432345899987678956789656998934569979101236\n9899321296998765699869987654321012345689234789765434767896543656789398789547897997987897698654312377\n0998910989899876986545699876542123467894345679876545678987656767891239895438456789876789798765425458\n1987899876789987899634569987643444578965467989987687989698767878932367994321387899965678969886534567\n9876798985678998998785678998754555678996879897698789094579978989973456789210998999864567954997545678\n6765987654589899219896899679865766789987998789999892123467989698764567894339899598765899543987659889\n5434599543456789995987954589976778997899987659899943234579692569875698965998789439976798762399767996\n4323498432345899884398943478987889656768999548789867455698543459876789879887678945797897653989888945\n5419987521236789765459654569998995443459998929689978996987656767987894998776569896898998769876999236\n6598996432345799896598767679989764312379876213567899989698967878998932989675456679999889899965210123\n7987987643457899989679898789876532101999954101377999877549878989999549876543234578998778989874321256\n9896599754598999978989969898989953219889893215456789765434989991987657987832124567989656578995452345\n8789459895689988767897654947898764598767789326767999874323497890198798998775012579876545456789678456\n5678968987899877658989932236789879987656698987978999985212646789259999659654323456987434368999989567\n6989879498998765434567890125678998798544567898989989432101239894349898745965434569874321239989998698\n7897989369987654323456921234578987654123456789999978999219398965498789659876756998765442398978999799\n8996593254598987312347892347689897543244789899878867878998997896987678967987987899876599976767899892\n9989432123459876201234989498996798798765679998765756767987876789876567898999898913987988765356789921\n9878941012367965312345678999345699899879789459854543459876565698985458799898789109899875434267896540\n9767892125679878323456789987656789999989897698763212598765434597654345689798678998756964320134589432\n8856999234569999474567898998967896798995979899854443679872123789986456895676569899549876534565678993\n7545898945678998765678987899898965987654168998766558789983234895497587894123456789434987655676799789\n6534767898799439976989876799789654496543054579878669893494545679398998973236789896545999768789897678\n5423456999896521989898765687678943219832123567989778912965876893219349765445678987659878978897934569\n7312346789995432398789954554567894398753234578999889999876987932101239876576789998798967989976545778\n6101237799989545986566893213458965989964545679432999889987898993299545987689899999987652397999758889\n4232345678978959875455799102348979877899656789599989678998929989988956798797999899876541656789767896\n5643458989769898765344568923456798765678997899987878567899019876567897899896789789987630345689878945\n6756567895456789654123579434568998654567989999996759456789198765458998998965675679898321234678999234\n7887678943379899763234678945689876543234878998875642345689987654349899987654324598765432345899899165\n8998789321234789854345789656999987632123569987984551234793498875456789998783213459876543656789678976\n9549895452375699965656798767899874321012478976543210159892109986767899989654302345998754567897567897\n0234976565467789798767899988998765434566567897865421767899212987898909876543213566789766678998486789\n1655989876988894569989910199019876545679788998976562345678924598999212987654424587899887889239345678"
    lines = String.lines input 
    numberLines = lines |> List.map (String.toList) |> List.map (List.map toDepthNumber)
    numberLinesStr = numberLines |> List.concatMap (List.map String.fromInt) |> String.join "."
    rowCount = List.length numberLines 
    colCount = 
        case List.head numberLines of 
          Nothing -> 0
          Just h -> List.length h 

    lowPoints = findLowPoints numberLines 
    floor = numberLines |> List.map (List.map (\d -> d == 9))
    borderPoints = findBorderPoints floor
    basins = lowPoints |> List.map createBasin
    lowPointDepths = lowPoints |> List.map (\pt -> lookup2d pt numberLines |> Maybe.withDefault 0)
    riskLevelSum = lowPointDepths |> List.map (\d -> d + 1) |> List.sum

    model = { floor = floor
            , border = borderPoints
            , basins = basins
            , counter = 0
            , riskLevelSum = riskLevelSum 
            , debug = "..."}
  in 
    (model, Cmd.none)


-- UPDATE

type Msg = Tick

updateBasin floor basin =
  if List.isEmpty basin.explorationPoints then 
    basin 
  else 
    let 
      explorationSet = basin.explorationPoints |> Set.fromList 
      filledPoints = explorationSet |> Set.union basin.filledPoints
      addedPoints = basin.filledPoints |> Set.diff filledPoints |> Set.toList
      exploreNext = addedPoints |> List.concatMap findNeighbourPositions |> Set.fromList |> Set.toList |> List.filter (isOpen floor)
    in 
      { lowPoint = basin.lowPoint 
      , explorationPoints = exploreNext
      , filledPoints = filledPoints }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      let
        updatedBasins = model.basins |> List.map (updateBasin model.floor)
        updatedModel = { model | basins = updatedBasins, counter = model.counter + 1 }
      in 
        (updatedModel, Cmd.none)

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
        xStr = String.fromInt (xVal * cellSize)
        yStr = String.fromInt (yVal * cellSize) 
      in 
        rect
          [ x xStr
          , y yStr
          , width (String.fromInt cellSize)
          , height (String.fromInt cellSize)
          , fill color
          ]
          []

findCenterPosition posList = 
  let 
    xSum = posList |> List.map (\(x, y) -> x) |> List.sum 
    ySum = posList |> List.map (\(x, y) -> y) |> List.sum 
    len = posList |> List.length 
  in 
    (xSum // len, ySum // len)

createBasinText basin = 
  let 
    (xVal, yVal) = basin.filledPoints |> Set.toList |> findCenterPosition
    xStr = String.fromInt (xVal * cellSize) 
    yStr = String.fromInt (yVal * cellSize + 12)
    count = basin.filledPoints |> Set.size
    countStr = String.fromInt count
  in 
    Svg.text_ [ x xStr, y yStr ] [ Svg.text countStr ]
    --Svg.text_ [ Svg.Attributes.textAnchor "middle", x xStr, y yStr ] [ Svg.text countStr ]
  

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    borderRects = 
      model.border 
      |> List.map (\pos -> toCellRect "black" pos)
    basinRects = 
       model.basins
       |> List.concatMap (\basin -> basin.filledPoints |> Set.toList |> List.map (toCellRect "lightblue"))
    basinTexts = 
       model.basins 
       |> List.filter (\basin -> List.isEmpty basin.explorationPoints)
       |> List.map createBasinText

    rects = borderRects ++ basinRects ++ basinTexts
  in 
    svg
      [ viewBox "0 0 600 600"
      , width "600"
      , height "600"
      ]
      rects

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2021 | Day 9: Smoke Basin"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    s = toSvg model
    riskLevelSumStr = "Risk level sum: " ++ String.fromInt model.riskLevelSum
    basinSizes = model.basins |> List.map (\b -> b.filledPoints |> Set.size)
    threeLargestBasins = basinSizes |> List.sort |> List.reverse |> List.take 3 
    threeLargestBasinsStr = threeLargestBasins |> List.map String.fromInt |> String.join " x "
    basinProduct = threeLargestBasins |> List.foldl (*) 1
    basinStr = "Basin product: " ++ threeLargestBasinsStr ++ " = " ++ String.fromInt basinProduct
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
              [ Html.div [] [Html.text "Advent of Code 2021" ]
              , Html.div [] [Html.text "Day 9: Smoke Basin" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2021/day/9" ] 
                [ text "https://adventofcode.com/2021/day/9" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "padding" "20px"] 
              [ Html.div [ Html.Attributes.align "center" ] [ s ] 
              , Html.div [] [ Html.text riskLevelSumStr ]
              , Html.div [] [ Html.text basinStr ]
              ] ] ]
