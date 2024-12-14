module Aoc12Plant exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

defaultTickInterval : Float
defaultTickInterval = 100

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Pos = (Int, Int)

type DataSource = Input | Sample | SampleXoXo | SampleLarger | SampleEShape | SampleAbba

type alias Plant = Char 

type alias Garden = Dict Pos Plant

type alias Plot = Set Pos 

type alias PlotSequence = List Plot 

type alias Line = 
  { startPos : Pos 
  , endPos : Pos }

type Border = Vertical Line | Horizontal Line 

type alias PlotInfo = 
  { complete : Plot  
  , sequence : PlotSequence
  , totalSteps : Int 
  , plant : Plant 
  , color : String
  , area : Int 
  , perimeter : Int
  , perimeterDiscount : Int
  , fenceCost : Int 
  , fenceCostDiscount : Int }

type alias Model = 
  { plotInfoList : List PlotInfo
  , rowCount : Int 
  , colCount : Int 
  , step : Int
  , maxSteps : Int
  , totalCost : Int 
  , totalCostDiscount : Int 
  , dataSource : DataSource
  , paused : Bool 
  , finished : Bool 
  , tickInterval : Float 
  , message : String }

sample = """AAAA
BBCD
BBCC
EEEC"""

sampleAbba = """AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA"""

sampleEShape = """EEEEE
EXXXX
EEEEE
EXXXX
EEEEE"""

sampleLarger = """RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"""

sampleXoXo = """OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"""

input = """LLLLLLLLLLLLLLLLLLLLLLLPBBBVVVVVPPPPPSSSSCCCCCCCCCCCCCCVVVVVVVVVJJJJJJJJJJJJJJYIYYYYYYYYYYKKKKKKKKKKKKKZQQQQQQQQQQQQQQQBBLLLLLLWWWWFFFWWWWWW
LLLLLLLLLLLLLLLLLLLLUPPPPBBVVVPPPPPPPSSSCCCCCCCCCCCCCCVVVVVVVVVCCCJJJJJJJJJJJJYYYYYYYYYYYYKKKKKKKKKKKKKKKQQQQQQQQQQQWCCLLLLLLLLWWWWWWWWWWWWW
LLLLLLLLLLLLLLLLLPPHPPPPPPPVVPPPPPPPPSSSCCCCCCCCCCCCCCCVVVVVVVVCCCCJJCJCJJJJJYYYYYYYYYYYYYYKKKKKKKKKKKQQKQQQQQQQQQQQWWWWWNLLWWWWWWWWWWWWWWWW
LLLLLLLLLLLLLLLLPPPPOOPPPWPVVVPPPPPPPPCCCCUCCCCCCCCCCCCCVVVVVVCCCCCCCCCCCCJJJJYYYYYYYYYYYYYYHKKKKKKKKYQQQQQQQQQQQQQQWWWWWWWLWWWWWWWWWWWWWWWW
LLLLLLLLLLLLLLULLPPPPPPPPPVVVVVPPPPPPPPPCCCCCCCCCICCVCCVVVVVVVCCCCCCCCCCJJJJJJJYYYYYYYYYYYYKKKKKKKKKKYQQQQQQQQQWQQQQQWWWWWWWWWWWWWWWWWWWWWWW
ZLLLLLLLLLZLLLULUUPPPPPPPPPPVVVPPPPPPPPPPSCCCCCCCCNVVVVVVVVVVCCCCCCCCCCCCJJJJJJJCYYYYYYYYYYYYYKKKKKKKYQQQQQQQQQWWWQWQWWWWWWWSSWWWWWWWWWWWWWW
ZLLLLLLLLZZLLLUUUUUUPPPPPPPPVPPPPPPPPPPPPPPCCCCCCNNVVVVVVTVVVCCCCCCCCCCCJJJJJJJJYYYYYYYYYYYYYYKKKKKDDQQQQQQQQQQQPWWWWWWRRRWWSSMMWWWWWWWWWWWW
ZLLLLLLLZZZZLLUUUUUHPPPPPPPPPPPPPPPPPPPPPPPVCCCOCTTVHTTTTTVVVVCCCCCCCCCCCJJJJJYYYIYYYYYYYYYYOOKKKDDDDQQQQQQQQQQQPPWWWWWRRSSIISSMWWWWWWWWWWWW
ZLLLLLLLZZZZZZUUUUUUPPPPPPPMMPPPPPPPPPPPPPPVVCKTTTTTHTTTTTVVVCCCCCCCCCCCCJJJJJJYYYTYYYYYYYYYYOKDDDDDDDQQQQQQQQQMWWWWWWWWWKSSSSSWWWWWWWWWWWWW
LLLLLZLGGZZZZZUUUUUUUUUUPPMMMPPPPPPPPPPPPPPPVTTTTTTTTTTTTTYVVWCCCCCCCCCCCJJJJJJJYYYYYYYYYYYOOOODDKKDDDDDDQQQQQMMMWWWWWWWWKSSSSSSWWWWWWWWWWWW
WLNNLZZZZZZZZZUUUUUUUUUUMMMMMMMPPPPPPPPPPVVVVVTTTTTTTTTTTTTEECCCCCCCCCCCJJJJJJJJPIYYYYYYRRRFFDDDDDKDDDDDYYQQQQMMMWWWWWWWWWSSSSSSSWWWWWWWWWWW
WLLLLLZZZZZZZZZUUUUUUUUUMMMMMMMPMPPPPPPPPVVVVVTTTTTTTTTTTTTECCLCCCCCCCHHHOJPPJJPPIIYYYYYRFFFFFDDDDKDKDDDDYQQQQMMMMMWWWWWWWSSSSSSSSWWWWWWWWXX
WWWLWWZZWZZZZZKKKUUUUUUUMMMMMMMPMPPPPPPTPVVTTTTTTTTTTTTTTEEEECCCCCIIHCCHHHHPPPPPPFIIYYYFFFFFFDDDDDKKKDDDDQQQMMMMWWWWWWWWBSSSSSSSSSSWWWWWWWBX
WWWWWWWWWZZZZKKKKUMUUMMUMMMMMLMMMMPDLDPTTTVTTTTTTTTTTTTTTTTEECCCCCCHHCCHHHHPPPPPPFFFYFFFFFFFFFDDDDKQQQQYYQMMMMMMMMWWWWWINSSSSSSSSSWWDWWWWBBX
WWWWWWWZZZZZGKKKKKMMMMQQQMMMMLLLLLLDLLLTTTTTTTTTTTTTTTTTTTTTEGGGCCOHHHHHHHHPPPPPFFFFFFFFFFFFFFFFFDDDQQQYQMMMMMMMMMNNNNNNNSSSSSSSSSSWWBWWBBBB
WWWWWWWWWCZDKKKKKKMMMMUQQQMMMLLLLLLLLLLTTTTTTTTTTTTTTTTTTTTGGGGGCHHHHHHHHHHKPPPPFFFFFFFFFFFFFFFFFFQQQQQYQMMMMMMMMMMMMNNNNNSSSSSSSSSWBBWBBBBB
WWWWWWWWCCZZKKKKKKMMMQQQQQDDMLLLLLLLLLLLTTTTTTTTTETTTTTTOTTTOGCCCHHHHHHHHHHKPPPFFFFFFFFFFFFFFFFFFFQQQQQQQMMMMMMMMNNMNNNNNNNNNSSSSSBBBBBBBBBB
NWWWWWWWWWIIKKKKKVMMMMMDQDDLMLLLLLLLLLFTTTTTTTTTTETTTTTOOOOOOGJCCOOOHHHHHHHKKKPPPPPPFFFFFFFFFFFFFNNQQQQQMMMMMMMMMNNMNNNNNNNNNSSSSSSBBBBBBBBB
WWWWWWWWWWIIIKIVVVVMDDDDDDLLLLLLLLLLLLLWWTTTTTTTTEETTTTTOOOOGGGOOOOHHHHHHHHHKKPPPBBBFFFFFFFFFFFFFNNQQQMMMMOOOOOOOOONNNNNNNNNSSSSSSSBBBBBBBBB
GGWWWWWWIIIIIIIIIVVWDDDWDDDLLLLLLLLLLLLWWWWTTEEEEEEEEEEOOOOOOOOOOOOHHHHHHHHHKPPPPPNFFFFFFFFFFFFFFFFYQQMMMMOOOOOOOOONNNNNNNNNSSSSSSSBBBBBBBBB
GGGGWWIWIIIIIIIIVVWWDDDDDLLLLLLLLLLLLLWWWWWTTEEEEEEEEEOOOOOKOOOOOOOHHHHHHHHHPPPPPPNNNNNNNFFFNFFFIIIIQMMMVVOOOOOOOOOONNNNNNNNSSSSSSSBBBBBBBBB
GGGGIIIIIIIIIIIIVVWWDDDLLLLLLLLLLLLLLLLLWWWTEEEEEEEEEOOOOOKKOOOOOOOHNHHHHHHPPPPPPPNNNNNNNNNNNFFFIIIISIIMIVOOOOOOOOOOONNNNNNNNSSSSSSSSBBBBBBB
GGJJIIIIIIIIIITIVWDDDDDWWEWLLLLLLLLLLLLLLEWEEEEEEEEEEOOKOOKKKOOOONNNNHNHHHHHPPPPPPPNNNNHNNNNSSFFIIIIIIIIIVOOOOOOOOOOONNNNNNNNNSSSSSSBBBWBBBB
GJJJJIIIIIIFFFFFVDDDDDDDDDDLLLLLLLLLLLLFEEEEEEEEEEEEEEEKKKKKOOOONNNNNNNHHHHHPPPPPJPDNNFJNNNNSSSSIIIIIIIIIIOOOOOOOOOOONNNNNNNNISISSSSBBBWBBLB
JJJJJJJIIIUFFFFFWDDDDDDDDDDOLLLLLLLLLLLFEEEEEEEEEEEEAEEKKKKKKOOONONNNNNNHHHOYYBBBJJDDJFJJNNSSSSIIIIIIIIIIIOOOOOOOOOONNNNIIIINIIIIISSBEBBLLLL
MJJJJJMIIIFFFFFFWDDDDDDDDDDOOLOLLLLLLEEEEEEEEEEEEEEEEEKKKKKKKOOONONNNNNNHHYYYYBBBJJJJJJJJNJSSSIIIIIIIIIIIIOOOOOOOOOONNNIIIIIIIIISSSSWEEWLLLL
JJJJJMMMMFFFFFFFWDDDDDDDDDDOOOOOLLLLLEEAAEEEEEEEEEEEEKKKKKKKKKKOOONCNNNNHHHYYYYBBBJJJJJJJJJSSIIIIIIIIIIIIVOOOOOOOOOOWIIIIIIIIIISSSSWWEEWLLWW
JJJJMMMMMMFFFFFWWDDDDDDDDDDOOOOOLLLUEEEEEEEEEEEZZKKKKKKKKKKKKKKEEEEEEEEENHYYYYYBBJJJJJJJJJJSSSSIIIIIIIIIVVOOOOOOOOOOWIIIIIIIIIIIIIIYWWWWWWWW
OXOJMWMMFFFFFFWWWWWWWWKDDDOOOUUUHLTUEEEEEEEEEEEZKIKKKKKKKKKKKKOEEEEEEEEEYYYYYYJJJJJJJJJJJJJJJJIIIIIIIILLLVVOOOOOOOOOIIIIIIIIIIIIIIIWWWWWWWWW
OOOMMWMFFFFFFFFWFFWWWWKDDDKOOUUUUWUUUEEEEEEEEEEEKKKKKKKKKKKKKOOEEEEEEEEEYYYYYYYJJJJJJJJJJJEENNINIIIIILLLLVVVIIIIIIWWWIIIIIIIIIIIIIIWWWWWWWWW
LLOWWWWFFFFFFFFFFFWKWKKDDDOOOUUUUUUUUFEEEEEEEFENKKKKKKKKKKKKKKOEEEEEEEEEYYYYYYYJJJJJJJJJJEEENNNNNIIJLLLLLVVVIIIIIIWWIIIIIIIIIIIIIIIWWWWWWWWW
OOOGWWVWFFFFFFFFFFOKKKKDDDKOUUUUUUUUUUEEEEEEEEXKKKKKKKKKKPPKPKOEEEEEEEEEYYYYYYYYYJJJJJJJEEEENNNNNIIJJJLLLLLVILIITHHWIIHIIIIIIIIIIIWWWWWWWWWW
OOOOOWWWFFFFFFFFFFKKKKKDDDOOKNKUUUUUUEEEEEEEEEEKIIIKKKKKKPPPPPPPPPPCYYYYYYYYYYYYYJJJJEEJJEEEEEJJJJJJJJLLLLLLLIIITHHHHHHHIIIIIIIIIIIIWWWWWWWW
OOOOOWWWWWFFFFFFFYKKKKKDDDKKKKKUUUUUUEEEEEEEEEWKIIIKKKKKKPNPPPPPPPYYYYYPYYYYYYYYYJJEEEEJEEEEEEJJJJJJJJQLLLLLLLLITHHHHHHHHIIIIIIIIIIWWWWWWWWW
OOOOOWWWWWFFFFFFYYKKKKKDDDKKKKKUUUUUUUEEEEEEEEEEIIIIIKSIKKNPPPPPPPPPYYYPYPYYYYYYYJYEEEEEEEEEJJJJJJJJLLLLLLLLLLLTTTHHHHCHHHHIHIIIDIIWWWWWWWWW
OOOOOWWWWWWFFYYYYYYKKKKDDDKKKKKUUUUUUUUUEEEEEEEIIIIIIKKIIKNNNPPPPPPPPPPPYPPYYYYYYYYEEEEEEEEEEJJJJSJJJLLLLLLLLLLLLHHHHHHHHHHHHHIDDIWWWWWWWWWW
OOOOOWWWWWWFFYYYYYYKKKKKKKKKKKKUUUUUUUUEEEEEEEEEIIIIIIIIIIINPPPPPPPPPPPPPPPPPPPYYYYEEEEEEEEEEEEJJJLLLLLLLLLLLLLHHHHHHHHHHHHHNNIWWWWWWWWWWWWW
OOOOOOOWWWWWWIIYYYYYKKKKKKKKUUKUUUUUUUUUEEEEEEEEEIIIIIIIIIIPPPPPPPPPPPPPPPPPPPPYYYYYEEEEEEEEEEEEGGLLLLLLLLLLLLLPPHHHHHHHHHHHNNWWWWWWWWWWWWWW
DOOOOOOWWWWWWIIYYYYYYKKJKKNUUUUUUUUUUUUUUEEEEEEEEIIIIIIIIIIEPPPPPPPPPPPPPPPPPPPYYYYEEEEEEEEEEEEEEGGLLLLLLLLLLLPPPPPHHHHHHHNNNNWWWWWWWWWWWWWW
DOOWOWWWWWWWWIIIYYYYYKKJJJUUUUUUUUUUUUUUUJEEEEEEEEIIIIIIIIIPPPPPPPPPPPPPPPPPPPPPYYYEEEEEEEEEEEEEGGSLLSLLLLLLLLLLHHHHHHHHHHHNNNNNWWWWWWNNWWWD
DOWWOWWWWWWWWIIIYYYBYYKBBBBBUULUUUUUUUUUUUEEEEEEEEIIIIIIIIIIPPPPPPPXPPPPPPPPPPPAYYYYYEEEEEEEEEEEESSLLSLLLLLLLLLEHHHHHHHHHHHNNNNNWWWWWNNNWWDD
WWWWOWWWWWWIWIIIIIIBBYBBBMBBULLUUUUUUQQQQXXXXEEEIIIIIIIIIIIIIBPPPXXXXXPPPPPPPUUUUUUUUEEYYYYYYEEESSSSSSLLLLLLLLLEHHHHHHHHHHHNNNNNNNWWNNRNWWGG
WWWWWWWWWWWIIIIIIIBBBBBBBMMLLLLLLUUUQQQQQPXXXEEEIIIIIIIIIIIIIAAAPXXXXXPPPPPPPUUUUUUUUYYYYYYYYYEEESSSSSSSSSSLLLLEHHHHHHHHRRRNNNNNNNNNNRRNWGGG
OOWWWWWWWWWIIIIIIIBBBBBBBBBLLLLXLUULWWWPPPPXXXXEVIIIIIIIIIIIIAAAAAXXXXXPPPPPPUUUUUUUUYYYYYYYYYEEESSSSSSWWSEAEEEEEHEHHHHHCRNNNNNNNNNRRRMNNNGC
OWWWWWWWWWWWIIIIIIBBBBBBBBBBLLLLLLLLLWWPPPPXXXVEVIIVIIIIFIIIAAAAAXXXXXPPPPPPHUUUUUUUUYYYYYYYYYYYSSSNNSSSSEEEEEEEEEEEHHHCCRNRRRRNNNNRRRNNNNGG
OOOWWWWWWWWWWWIIIIBBBBBBBPBBBLLLLLLWWWWPPPPPXVVVVVIVVVIIIIIIAAAAAXXXXXNPPPPPGUUUUUUUUYYYYYYYYYYYYSSYYSSSSEEEFEEEEEEHHEEEEENNRRRRRRRRRGGGGGGG
OOOOOWWWWWWWWWBEIIBBBBBBBBBLLLLLLLLLWWWWPPPPXXVVVVVVVVIIIIIIAAAAAVXXXXNNNNNNNUUUUUUUUGYYYYYYYYYYYYSYYSSSEEEEEEEEEEEEEEEEVVVNRRRRRRRRGGGGGGGG
ZOUUUUUUUUUUWBBBIBBBBBBBBBBBLLLLLLLLFWWWPPPPPXXVVVVVVVVVBIZZZAAAMVVXXNNNNNNNNUUUUUUUUYYYYYYYYYYYYYYYYYYYFEEEEEEEEEEEEEEEVVVVRRRRRRRRGGGGGGGG
ZZUUUUUUUUUUUUUUUIBBBBBBBBBBLLLLLLLKFFWWWPPPQVXVVVVVVVVVBIZZZZZAMVVXXNNNNNNNNUUUUUUUUYYYYYYYYYYYYYYYYYEYEEEEEEEEEEEEEEEEEVVVRRRRRRRRGGGGGGGG
ZZUUUUUUUUUUUUUUUBBQQBBBTBQBLQQQQQLFFFFFFPPPPVXVVVVVVVVVVZZZZZZAMVXXXNNNNNNNNUUUUUUUUYYYYYYYPYYPYYYYYEEEENEOEEEEEEEEVVVVVVVPRRRRRRRRRGGRRGNN
ZZUUUUUUUUUUUUUUUQQQQQBBBBQQQQQQQQQFFFFFFPPJVVVVVVVVVVVVVVZZZZZVVVVXXXNNNNNNNUUUUUUUUYYYYYYPPPPPEYEYEEEBEEEOEEEEEEEEEVVVVVVVRERRRRRRRRRRJJNJ
ZZUUUUUUUUUUUUUUUQQQQQQBBBQQQQKKKKKKKKFFFFFJVVVVVVVVVVVVVVVZZZZVVVVVXXXXNNNNAIIIGGGGGYYYYYBPPPPPEEEYEEEEEEEOOEEEEEEEEVVVVVVVVVVRRRRRRJJJJJJJ
ZZUUUUUUUUUUUUUUUQQQQQQQQQQQQXKKKKKKKKFFFFFFVVVVVVVVVVVVVVZZZVVVVVXXXXXIRNAAAIIIIIIIGGYYYYBPPPPEEEEEEEEEEEEOOEEEEEEEVVVVVVVVVVVRRRRRRJJJJJJJ
ZZUUUUUUUUUUUUUUUPPPQQQQQQQQQXKKKKKKKKFFFFFBBVVVVVVVVVVVVVZZVVVVVVXXXXXIRRIIIIIIIIIIIYYYYBBBBPPLEEEEEEEEEEEEOEEKEEEEVVVVVVVVVRRRRRRRJJJJJJJJ
ZZUUUUUUUUUUUUUUUPPQQQQQQQQQQKKKKKKKKKFFFBBBBBVVVVVVVVVVVVVVVVVVVVVVXXXIIIIIIIIIIIIIIHYEEBBEEEJEEEEEEEEEEEEEEWEKKKEEVKVVVVVVVRRRRRRRJJJJJJJJ
ZZUUUUUUUUUUUUUUUPPQQQQQQQQQKKKKKKKKKKFFBBBBBBBVIIVVVVVVVVVVVVVVVVVVVXXXIIIIIIIIIIIIIIIIEEEEJJJJEEEEEEEEEEEEEEKKKKEKKKKKKKVRRRRRRRRJJJJJJJJJ
ZZPPUUUUUUUUOPPPPPQQQQQQQQQXKKKKKKKKKKKKKKBBBBBIIIIVVVVVTTTVVVVVVVVVVXXIIIIIIIIIIIIIIIIIEEEEEEEEEEEEEEEEEEEEEEKKKKKKKKKWKKVRRRRRRRJJJJJJJJJJ
PPPPUUUUUUUUPPPPPQQQQQQQQXXXKKKKKKKKKKKKKKBBBBIIIIIIIVVVVTTVVVVVVVVVVVIIIIIIIIIIIIIIISEEEEEEEEEEEEEEEEEEEEEEEEKKKKKKKKKWKKVRRRRRRRJXJJJJJJJJ
PPPPUUUUUUUUNPPEQQQQQQUQQXXXKKKKKKKKKKKKKKBBBBIIIIIIWWWWWWWWICVVEVVVVCCIIIIIIIIIIIIIIIIEEEEEEEEEEEEEEEEEEDDEEDKKKDKWWWWWKKVRVRRRRXXXJJJJJJJJ
PDDPUUUUUUUURRREEQQQQQQQQQXXKKKKKKKKKKKKKKRRBRMIIIIIWWWWWWWWICVVVVYVVCCCIIIIIIIIIIIIIIEEEEEEEEEEEEEEEEEEEEDDDDDDDDKKKKWWWWWWVVRJSXXXJJJJJJJJ
DDDPUUUUUUUURRRREEQQQQQQQXXXKKKKKKKKKKKKKKRRRRRIIIIIWWWWWWWWIVVVVYYYCCCCCCIIIIIIIIIIIREEEEEEEEEEEEEEEDDEEDDDDDDDDDKKKKWWWWWWVSSSSSSSIJJJJJJJ
DDDPUUUUUUUURRRREEQQQQQXXXXXKKKKKKKKKKKKKKRRRRRRIIIIWWWWWWWWJJJJJCCCCCCCCCIIIIIIRRRXXXXXXXXEEEEEEEEEEDDDDDDDDDDDDWKKWWWWWWWSSSSSSSSSSJKJJJSJ
DDDDUUUUUUUURRRRRQQQQQQQQXXXXXXXKKKKKKKKKKRRRRRRIIIIWWWWWWWWWWWJJCCCCCCCCCCIIIIIIERXXXXXXXXEEEEEEEEEDDDDDDDDDDDDDWWKWWWWWWWSSSSSSSSSSJJJJJJJ
DDDDPPRRRRRRRRRRRQQQQQQQQQXXXXXXKKKKKKKKKKRRRRIIIIIIWWWWWWWWWWWWWWJCCCCCCCCIIIIIEERXXXXXXXXEEEEEEEEDDDDDDDDDDDDDDDWWWWWWWWWWKSSSSSSSSSSJJJSJ
DDDRRRRRRRRRRRRRRQQQQQQQQQQXXXXXKKKKKKKKKKRRRRVIQIIIWWWWWWWWWWWWWWWWVCCGGGGGIIIGRRRXXXXXXXXEEEEEEEEEDDDDDDDDDDDDDWWWWWWWWWWWSSSSSSSSSSSSSSSS
DDRRRRRRCRRRRRRRRQQQQQQQQQQXXXXXXKKKKKKRRRRRRRQQQQIIWWWWWWWWJJWWWWWWJJGGGGGGGGGGRRRXXXXXXXXEEEEEEEEEEDDDDDDDDDDDDDWWWWWWWWWWWSSSSSSSSSSSSSSS
DDDRRRRRCCCRRRRRPQQQQQQQQQQXXXSSSKKKKKKRRRRRRRMQQQIIWWWWWWWWJJWWWWWWJJJJGGGGZZZGRRRXXXXXXXXREEEEEEELMLLLDDDDDDDDDDDDWWWWWWWWWSSSSSSSSSSSSSSS
DDRRRRRRCCCCRCCPPPPPQQQQQQQMXXXSSKKKKKKRRRRRRRMQQQQIWWWWWWWWJJWWWWWWJJJJGGGZZZZZRRRXXXXXXXXREEEEEELLLLLLLIIIIIIIIIIDWWWWWWWWSSSSSSSSSSSSSSSS
DDRRRRRCCCCCCCPPPPPPPPQQQQQQXXSSSSSSSSRRRRRRRRQQQQQQQIIIIIJJJJWWWWWWJJJJGGZXXXXXXXXXXXXXXXXRRRRLLELLLLLDDIIIIIIIIIIDWWWWWWWWSSSSSSSSSSSSSSSS
DDDRDRRCCCCCCCPPPPPPPPQQQFQQNNNSSSSSSRRRRRRRRRQQQQQQQIIIIPJJJJWWWWWWJJJJGGZXXXXXXXXXXXXXXXXRRRRLLLLLLLLDDIIIIIIIIIILWWWWWWWWSSSSSSSSSSSSSSSS
DDDDDRRRCCCCCCPPPPPPPPQQFFNNNNNSSSSSSRRRRRRRRRRQQQQQQIIIGJJJJJJWWWWWJTJGGHZXXXXXXXXXRRJJRJJRRRRRLLLLLLLDDDDDDDDLLLLLWWLWWWWWNNSSSSSSSSSRRRRS
DDDDZRRZZRCCCCCYPYYPYPQQFNNNNNNSSSSSSSRRRRRRRRRQQQQQQQIGGJJJJJJWWWWWTTJGHHYXXXXXXXXXRRJJJJJJJJXXXXLLLLLHDDDHDDDLLLLLLWLWTWWNNNSSSNSSRRSRRRRR
DDDBZZZZCCCCCCCYYYYYYYQQFNNNNNNSSSSSSSSRRRRRRRRRQQQQQQQQQJJJJJJWWWJJJJXXXXXXXXXXXXXZJJJJJJJJJJXXXPPHLLLLLHHHHDMLLLLLLLLWTTANNNNNNNRRRRRRRRRR
DDBBBZZZCYCCCCYYYYYYCFQFFFNNNNSSSSSSSSSSGRGRGRRGQQQQQQHHSSLJJMJJJJJJJHXXXXXXXXXXXXXZZZJJJJJJJJPPPPYHLLHHHHHHHHLLLLLLLLLLTAANNNNRRNRRRRRRRRRL
BBBBBZZZZYYCYCYYYYFFFFQFFFFNNNSSSSSSSSSGGGGGGGGQQQQQQQQHSSSSMMMJJJMMMHXXXXXXXXXXXXXZZZZZJJJJJJJPPPHHHLHHHHHHHHLLLLLLLLLAAAAARRNRRRRRRRRRRRRL
BBBBBMYYYYYYYYYYYYVFFFFFFFFFFSXSSSSSSSGGGGDDDGGQQQQQQIQHSSXEMMMMMMMMUHXXXXXXXXXXXXXZZZZZYJJJJJJJPPHHHHHHHHHHHLLLLLLLLDAAAAAAARRRRRRRRRRRRRRL
UBBBYYYYYYYYYYYYYYYYYFFFFFFFSSSSSSSSFFGGDDDDDDGQQQQQQEEEESSEMMMMRRRRUUXXXXXXXXXXXXXZZHHYYJJJJJJJPYHHHHHHHHHHHLLLLLLLLAAAAAAAARRRRRRRRRRRRRLL
BBBBYYYYYYYYYYYYYYYYFFFFFFFFSSFFFFFFFFFGGDDDDDDDDYYEQEEEEEEEMMMRRRRRXUXXXXXXXXXXXXXYYYYYYJJJJJYYYYYHHHHHHHHHHHLLLLLLALLLAAAAAAAARRRRRRRRRRLL
BBBBBBYYYYYYYYYYYYYYYFFFFFFFFFFFFFFFFFGGGGDDDDDDDDYEEEEEEEEEEMMRRRRUUUXXXXXXXXXXXXXYYYYVVYYYJYYYYYYVVHHHHHHHHHHLLLLLLLAQAAAAAAAAAARRRRRRRRLL
BBBBYYYYYYYYYYYYYYYYYIFIFFFFFFFFFGGGFFGGGGDDDDDDDDYEEEEEEOWEOWRRRRRUUULUUUUUUZZZZZYYYYYIVYYYYYVVVYVVHHHHHHHHHHGFLLLLLLAAAAAAAAAAARRZRRYZZRRL
BBBBYYYYYYYYYYYYYYYYYYFIIIFFIIIFFFGGGGGGDDDDDDMDYYYYYEEEEWWWWWRRRRUUUULLUUUUZZZZZZZZYMYVVVVVVVVVVVVVHHHHHHHHHHGFLLLVVVVAAAAAAAAAAAAZZZZZZZLL
BBBBBYBBVRVYVVYYNYKYYYIIIIIIIIFFFFFGGGGGDDDDDDMDDYYYEEEEEWWWWWRRRRRRRULLLLUZZZZZZZZZZMYTTVVVVVVVVVVVVHHNHHHHHHGGULLLVVVAAAAAAAAAAAAAZZZZZJBB
BBBBBBBBVVVVVVVNNNIIIIIIIIIIIIIIFFFGGGGGDDDDDDMDDDDYYEEEWWWWWCWRRRRRRRLLLIGIZZZZZZZZZMMVVVVVVVVVVVVVHHHHHHHGGGGGGLLVVVVAAAAAAAAAAAAZZZZZZZBB
BBBBBBBBVVVVVVNNIIIIIIIIIIIIIIIFFFDGDDDDDDDDDDDDDDNDDREEWWWWWWWWWWIRRLLLIIIIIIIZZZZZZMMVVVVVVVVVVVVVVQQQQGGGGGGGGVVVDDDAAAAAAAAAAAAZZZZZZZBB
BBBBBBBVVVVVVVVVVWIIIIIIIIIIIIIUUDDDDDDDDDDDDDDDDDDDDDWWWWWWWWWWWWWRRLLLIIIIIIIIIIZZUIMVQVVVVVVVVVVVQQQQQQQGGGGGVVVVDDDDAAAAAAAAKKAAAAZZZZBB
BBBBBBBVVVVVVVVVWWWIEEIIIIIIIIIUUDDDDDDDDDDDDDDDDDDDWWWWWWWWWWWWWWWIIILLIIIIIIIIIIIIIIIQQVVVQQVVVVVQQQQQQQGGGGGGGVVVHDDDDHHHAAAKKKAKAAZZZZZZ
BBBBBBVVVVVVVVWVVWWIIEIIIIIIIIUUUUUDDDDDDDDDDDDDDDDWWWWWWWWWWWWWWWDDLIIIIIIIIIIIIIIIIIIQQQQQQVVVVVQQQQQQQGGGGGGGGGVVHHHHHHHKKKKKKKKKKKKKJJJJ
BBZBBBVVVVVVVWWVWWIIIIIIIIIIIIUUUUUUDDDDDDDDDDDDDOOOWWWWWWWWWWWWWWDDLLIIIIIIIIIIIIIIIIIIQQQQQQVVVVVQQQQQQQGGGGGGGVVHHHHHHHHKKKKKKKKKKKKKJJJJ
BBBBBBVVVVVVWWWWWKIIIIIIIIIIKIIUUUUUDDDDDDDDDDOOOOOOOWWWWWWWWWWWWLLDLLIIIIIIIIIIIIIIIIIIQQEQQQQQVQQQQQQQQQQGGGGGGVVVHHHHHHHKKKKKKKKKKKKXXJJJ
CCCBBBBBIVVWWWWWKKKIKIIIIIIIKKKDDUUDDDDDDDDDDDDDDOOOOOWWWWWWWWYWWLLLLLIIIIIIIIIIIIIIIIIIQQEEEQQQQQQQQNQQQQQQGGGGGVVVVHHHHKKKKKKKKKKKKKKXXJXJ
CCCCBBBBIIVVWWWWKKKKKKKKIIKKKKKDDDDDDDDDDDDDDDDSOOOOOOWWYWWYYYYYYLLLLLLIIIIIIIIIIIIIIIJIQQQEEQQQQQQQNNQQQQQQQGGGPVVHHHHHHHOKKKKKKKKKKXXXXXXJ
CCCCBBBIIKKKKKKKKKKKKKKKIKKKKKKKDDKKDDDDDDDDDDDDOOOOOOOWYYWYYYYLLLNLLLIIIIIIIIIIIIIIEEECEEEEEEEQQQQQQNQQQQQQGGGPPPPPPPPHHHHGKKKKKKKKKXXXXKXX
CCCCCCBKIKKKKKKKKKKKKKKKKKKKKUKKKKKKKDDADDADDDDOOOOOOOOWYYYYYYYLLLNLLLIIIIIIIIIIIIEEEEEEEEEEEEEQQQQQQNNQQNQQQQPPPPPUPPPHHHHGGGGKKKKKKXXXXXXS
CCCCCCKKKKKKKKKKKKKKXXKKKKKKUUUKUUUUKADAAAAAADDOOOOOOOOUYYYUUYYYYYOOOOIOOOIFFIIIIEEEEEEEEEEEEEEQQQQQQQNNNNQQQQQPPPPPPPGHHGHGGGGKKKKKKXXXXSSS
CCCCCCKKKKKKKKXXXXKXXKKKKKKUUUSUUUUUPUUAAAAAAOOOOOOOOOOUYYUUUUYYYOOOOOOOOOIFFIIIIEEEEEEEEEEEEEEEQEEEQNNNNPPPQQPPPPPPPPPPGGGGGRKKKKKKKFFXXXSS
CCCCCCKKKKKKKXXXXXXXXXXKKKKKUUUUUUUUUUUUUAAAOOOOOOOOJUOUURRRRRRYYOOOOOOOOOOOOOOIEEEEEEEEEEEEEEEEEEEEINNNNNPPPPPPPPXXXXXGGRRGGRRRKKFKFFXXXSSS
CCCCCCKKKKKKKXXXXXXXXXXKKKKUUUUUUUUUUUUUUAAAOOOOOOOOJUUUURRRRRROOOOOOOOOOOOOOOIIEEEEEEEEEEEEEEEEEEEIINNNNNNNZPPPPPXXXXXGRRRRGRRFFFFKFSSSSSSS
CCCCCCKKKKXXXXXXXXXXXXXKKUUUUUUUUUUUUUUAAAAAAOOOOOOOUUUUURRRRRROOOOOOOOOOOOOOOOIEEEEEEKKEKKEEEEEEEEINNNNNNNPPPPPPPXXXXXRRRRRRRRFFFFFFSSSSSSS
CCCCCCKKKKXXXXXXXXXXXXXXKSJJUUUUUUUUUUAAAAAAAOOOOOOOOUUUURRRRRROOOOOOOOOOOCOIOIIKELEWKKKKKAEEEEEEEENNNNNNNNPPPPPPPXXXXXRRRRRRRRRFFFFFFSSSSSG
CCCCCCKKKKKXXXXXXXXXXXXSSSJJURUUUUUUUUAAAAAOOVOOOOOOUUUUURRRRRROOXXOOOOOOOOOIIIIKKKKKKKKKKKEENENEENNNNNNNNNPPPPPPPXXXXXRRRRRRRRRRFFFFSSSSSGG
CLLLCLKLKKKKSYYYXXXXXXXXSSSSUPXUUUUUUUYAAAAOOOOOOWWUUUUUURRRRRRXXXXXXOOOOOOOOIIIIIIKKKKKKKKEENNNNENNNNONNNNPXXXXXXXXXXXRRRRRRRRRRFFFFFSSSSGG
CCLLLLLLLLSSSYYYXXXXXSXBSSSRPPXXXUUYYYYYVAOOOOOOOWOCUUUUURRRRRRXXXXXOOOOOOOOOIIIIIIKKKKKKKKEKNNNNNNNNNONONNPXXXXXXXXXXXRRRRRRRFFFFFFFFSGGSGG
CCLLLLLLLLLSYYYYYYYXXSSSSSSPPPXXXUUYVVHVVAAAOOOOOOOOMUUUURRRRRRXXXXXOOOOOOOIOIIIIIIKKKKKKKKKKNKNNNNNNOOOOOOOXXXXXXXXXXXXRRRRRRFFFFFFFLLGGGGG
CCCLLLLLLLLLYYYYYYYXXOOSISSPPPXVXVVVVVHVVAAUOOOOOOOOMMUUUUUURXXXXXXOOOOOOOIIIIIIIIIKKKKKKKKKKKKNNNNNNNXXXXXXXXXXXXXXXXXXXRRRRRFRFFLLLLGGGGIG
CCCLLPLLLLLGYYYYYYXXXPPPPPPPPPVVVVVVVVVVVVUUVOOOOONOOMUURRUURRXXXXXOOOOOOOIIIIIIIIKKKKKKKKKKKKKNNNNNNOXXXXXXXXXXXXXXXXXXXRRRRRRRRLLLLLLGIIII
CCCLCLLLLWYGYYYYYYXXPPPPPPPPPPPVVVVVVVVVVVKUVOOOOOOOOOOORRUURRRRRXXOOOOOOIIIIIIIIKKKKKKKKKKKKKKKMMMMMMXXXXXXXXXXXXXXXXXXXRRRRRRRRCLLLLCIIIII
CCCCCCCLWWYYYYYYYYPPPFFPPPPPPPVVVVVVVVVVVVUUUOOOOOOOOOOORRRRRRXXXXXXNNOOOOOIIIIIIKKKKKKKKKKKKKKKMMMMMMMMMEOEXXXXXXXXXXXXXUURRRRRRRJJJIIIIIII
CCCCGGGGGYYYYYYYYYPFFFPPPPPPPPPGVVVVVVVVVXUUUOOOOOOOOOOQOORRRRRRMMMMNMOMIIIIIIIIIKKKKKKKKKKKKKKKMMMMMMMMMEEEXXXXXXUUXXXXXPRRRRRRRRJJJJIIIIII
CCGGGGGGGYYFFYYYYYYFFFFPPPPPPPVGVVVVVVVVVUUUUUOOOOOOOOOOOOORRRRMMMMMMMMMMMMMIIMIIKKKKKKKKKKKKKKKMMMMMMMMMMMMXXXXXMUUXXXXXPRRRRPPJJJJJJIIIIII
CGGGGGGGGGFFFFFFFYFFFFFFPPPPPPVVVVVVVVVVUUUUUZOOOOOOOOOOORRRRZRMMMMMMMMMMMMMMIMMIIKKKKKKKKKKKKKKMMMMMMMMMMMMXXXXXMCUUUURRRRRRRPPPJPJJJIIIIII
CCGGGGGGGGGFFFFFFFFFFFFFPPPPPPPPVMMPPPVVUUUUUZZOOUUOOOORORRRRZZMMMMMMMMMMMMMMMMMMIIIKKKKKKKKKKKKMMMMMMMMMMMMMMMMMMDDDDPRRRRRRRPPPPPJJJIIIIII
CCGGGGGGGGGFFFFFFFFFFFFFPPPPPPPPVPPPPPVUUUUUUUUUUUOOOOORRRRZZZZZMMZMMMMMMMMMMMMMMIIIKVVKKKKKKKKKMMMMMMMMMMMMMMMMMMRRRRRRRRRPPPPPPPPJJJJJIIII
CCGGGGGGNSSSFFFFFFFFFFFFPPPCPPPCVTPPPPPUUUUUUUUURRRRORRRRRRZZZZZZZZMMMMMMMMMMMMMVIIVVVVVKKKKKKKKKKEEEMMMMMMMMRRRRRRRRRRRRRRPPPPPPPPPPJJJIIII
CCGIIIGGSSSSFFFFFFFPFFFFPPPCCCCCCCPGUUPUUUUUUUURRRRRRRRRRRZZZZZZZZZZZMWWWWWWWMMMVVVVVKKKKKKKKKKKKKKEEMMMMMMMMMMRRRRRRRRRRRRRRPPPMMPPPJJJMIII
CCGIISSSUSSSSFFFFFPPAAAFAPACCCCCCCTUUUUUUUUUUUUUURRRRRRRRRRZZZZZZZZZMMWWWWWWWWWWWWWVVVKQKKKKKKKKKKKEEMMMMMMMMMMRRRRRRRRRRRRRRPPPPMMMPJMMMIII
CBBBSSSSSSSSSSFFPPPPPAAAAAAAACCCCCCUUUUUUUUUUUUUURRRRRRRZZZZZZZZZZZZZMWWWWWWWWWWWWWVVVVVVKKKKKKKKKKKEMMMMMMMMMMRRRRRRRRRRRRRRPPPPMMMMMMMMIII
CBBBBBSSSSSSSSFFPPPPPAAAAAAAAACCCCGCOUUUUUUUUUUURRRRRRRZZZZZZZZZZZZCJJWWWWWWWWWWWWWVVVVVVVNKNKKKKKKKECCCEMMMMMMHHCCDRRRRRRRRRPPPMMMMMMMMMMII
CBBBBBBSSSSSSSPPPPPPPAAAAAAAAACCCCCCUUUUUUUUUUUURRRRRRRZZZZFZZZZZCCCCJJJMWWWWWWWWWSVVVVVVVNKNNNKKKKKKCCCHMMMMMMSHDCDRRRRRRRRRPPPSSMMMMMMMMMI
BBBBBBDSSSSSSSSSIIPPPPAAAAPPACCCCCCUUCUUUXUUUUUURRRRRRRRRRRFZZZZZCCCCJJZZWWWWWWWWWSVVVVVVNNNNNNNKKKKKKCCHMMMMMMHHDDDRRRRRRRRRPPPPPCMMMMMMMMM
BBBBBBBSSSSSSSSSIIIPPAAAAAPPPPCCCCCCCCUUSUUUUNNBBRRRRRRRRRRRRZZZZCCCCCJZZWWWWWWWWWSVVVVVVNNNNNNNNGKKKHCCHMMMMMMHHHDDRRRRRRRRRPPPCPCMMMMMMMMV
BBBBBBBBSSSSSSSSIIPPPAAPAAAPPPCCCCCCJNUUUUUUBBBBBRRLRRRRRRRRRZZZZCCCCCJZZDVSVVSSVVVVVVVVVNNNNNNNNGKKKHHHHMMMMMMHHHHDRRRRRRRRRCPPCCCCCMMMMMMV
BBBBBBBBSSSSSSSSIIPPPAPPPPPPPPCCCCNJJNNUCCUBBBBBBLLLRRRRRRRUULLZZCCCCCCLVVVVVVSSSVVVVVNNNNNNNNNNNGKKQHHHHHHHHHHHHHHHRRRRRRRRRCPPCCCCMMMMMMVV
BBBBBBBBBBSSSSIIIIIIPPPPPPPPPCCCCCNNNNNNCCBBBBBBBBLLLRRRRRRLLLLHCCCCCCLLVVVVVIVVSVVVVVNNNNNNNNNNNGSSQQQHHHHHHHHHHHHICCCCCCCCCCCCCCCCMMMMMMMV
BBBBBBBJBFFSSSVVVVPPPPPPPPPPPCCCPCNNNNNNCCCBBBBBBLLLLLRRRRLLLLLCCCCCCCCLVVVVVVVVVVVVVVNNNNNNNNNNNGGSQQHHHHHHHHHHHHCCCCCCCCCCCCCCCCCCCMMMMMMV
GBBBBGBBBFFSSSVVKVPPPPPPPPPPPPPCPPNNNNNCCCBBBBBBBBBLLLLRLRLLLLLCCCCCCNNVVVVVVVVAVVVVVNNNNNNNNNNNSSSSSSHHHHHHHHHHHFFCCCLCCCCCCMMCCCCCCMMMVVMV
GBBGGGBBBFVVGGVVVVVPPPPPPPPPPPPPPPNNNNNCCCBBBBBBBBBLLLLLLLLLLLLGCCCCCNEVEEVVVVVEEVVVVNNNNNNNNTTHSSSSSSSHHHHHHHHFFFFFCCCCCCCMMMMCCCCCCCCMVVVV
GBBBGGGVWVVVVVVVVVVVVPPPPPPPPPPPPNNNNNNCCCBBBBBBBBBBLLLLLLLLLLLCCCCCCNEEEEEVVEEEETTTTTTNNNNNNTTHHSSSSSSHHHHHHHHFFFFFCCCCCMCMMMMCCCCCCSCCSSVV
GGBBGGGVVVVVVVVVVVVVVVVWPPPPPPPPPNNNNNNCCBBBBBBBPBBBLLLLLLLLLLLLCCCCCEEEEEEVEEEEEEETTTTTTTNNTTHHHSSSSSDHHHDDHHHHFFFFCCCCCMMMMMMMCCCCCSSSSSVV
GGGGGGGGVVVVVVVVVVXVVWWWWPPPPPWPPPPPNCCCBBBBBBPPPPBBLLLLLLLLLLLLCCJCJJJEEEEEEEEEEEEETTTTTTTTTHHHPPSSSDDHHDDDHHFFFFFFFFCFMMMMMMCCCCCCCSSSSSSS
GGGGGGGGVVVVVVVVVVXWWWWWWWWWWWWWWPPPNNCCCCBSPPPPPPPPJLLLXLLLLBLJJJJJJJEEEEEEEEEEEEEETTTTTTHHHHHHHHHHSDDDDDDUUUFFFFFFFFFFFFMMMMCCCCCSSSSSSSSA
GGGGGGGGVVVVVVVVVVWWWWWWWWWWWWWWPPPPPCCCCCCPPPPPPPPJJLLLLLGLLBIJJJJJJJEEEEEEEEZEEEETTTTTTTHHHHHHHHHHDHDDDDDUUUUFFWFWWFFMMFMMMMMCCCIIISSSSSAA
GGGGGGGGQQQVVVVVVGGWWWWWWWWWWWWWPPPPQQQCCCPPPPPPPPNNNNLLLGGIIBIJJIIJEEEEGEEEEEZEEEETTTTTTHHHHHHHHHHHHHHHDUUUUUUSFWWXWWMMMMMMMCCCCIIIISSSSSSS
GGGGGGGGQQQVVVVVPPWWWWWWWWWWWWWWPIIQTQCCCPPPPPPNNNNNNNZZLIIIIIIIIIWEEEEEEEEZZEZZEEETTTTOOHHHHHHHHHHHHHUUUUUUUUUSWWWWWWMMMIIMIICCIXIISSSSSSSS
GGGGGGGGQQQQQQQPPPPPWWWWWWWWWWWWPIIQQQCCCPPPPNPNNNNZZNZZZZZZZIIIIIWWEEEEEEEZZZZQQTTTTTTTOOHHHHHHHHHHHHHUUUUUUUUSSWWAWWMNIIIIIIIIIIIIISSSSSSS
GGGGGGGGQQQQQQQPPPPPWWWWWWWWWWWBIIQQQQCQCCPPNNNNNNNZZZZZZZZZZIIIIIIIIYEEEGEZZZZQQQOTTTTTOOHHHHHHHHHHHHHUUKUUUSUSSSSWWIIIIIIIIICIIIIIISSSSSSS
GGGGGGGGGQQQQQQPPPPPWWWWWWWWWWWWIIQQQQQQQNNNNNNNNNNNZZZZZZZZZIIIIIIIIEEEEEEEZZZZZQOTOTTOOOHHHHHHHHHHHKKUUKKUSSTSNSVWWWIIIIIIIIIIIIIIYSSSSSSS
GGGGGGQQQQQQQPPPPPPPWWWWWWWWWIIIIQQQQQQQNNNNNNNNNNZZZZZZZZZZZZIIIIIZZEEEEEECZZZZZQOOOOOOOOHHHHHHHHHMMKKKKKSSSSSSNNWWWNIXIIIGIIIIIIIIYSSSSSSS
GGGGGGQQQQQQQPPPPPPPWWWWWWWWWIIIIIIQQQQQNNNNNNNNNNNNNZZZZZZZZZZZZZZZZEEEEEZZZZZZZZOVVOOOOOOHHHHHHHHHMKKKKKTSSSSSNNWWNNIIIIIIIIIIIIIIYYSSSSSG
GGGQQQQQQQQQQQPPPPRRWWWWWWWWWIIIIIIQQQQQNNNNNNNNNNNVNZZZZZZZZZZZZZZZZZZEZZZZZZZZZZOOOOOOOOOHHHHHHHHHHKKKKKTTSSSSNNNWNNIIIIIIIIIIIIIIIISSSSSG
GEGQQQQQQQQQQQQQPRRRWLLWWZIIIIIIIIIQQQQQNMNNNNNNNNNNNNNZZZZZZZZZZZZZZZZZZZZZZZZZZZOOOOOOOOHHHHHHHHHHHXKKKKTTTSSSNNNNNNIIIFIIIIIIIIIIILLSSSSG"""

read : DataSource -> String
read dataSource = 
  case dataSource of 
    Input -> input
    Sample -> sample
    SampleAbba -> sampleAbba
    SampleEShape -> sampleEShape
    SampleLarger -> sampleLarger
    SampleXoXo -> sampleXoXo

getAllPositions : Garden -> List Pos
getAllPositions garden = 
  garden |> Dict.keys 

tryGetPlantAtPos : Garden -> Pos -> Maybe Plant
tryGetPlantAtPos garden pos = 
  garden |> Dict.get pos

getNeighbourCandidates : Pos -> List Pos 
getNeighbourCandidates (x, y) = 
  [ ((x - 1), y), ((x + 1), y), (x, (y - 1)), (x, (y + 1)) ]

getBorders : Set Pos -> Plot -> Pos -> List Border 
getBorders possible plot (x, y) = 
  let 
    nb = getNeighbourCandidates (x, y) 
    notInPlot = nb |> List.filter (\p -> not (Set.member p plot))
    borderPositions = notInPlot |> List.filter (\p -> not (Set.member p possible))
  in 
    [ if borderPositions |> List.member (x, y - 1) then Just (Horizontal { startPos = (x, y), endPos = (x + 1, y) }) else Nothing  -- N
    , if borderPositions |> List.member (x - 1, y) then Just (Horizontal { startPos = (x, y), endPos = (x, y + 1) }) else Nothing -- W
    , if borderPositions |> List.member (x, y + 1) then Just (Horizontal { startPos = (x, y + 1), endPos = (x + 1, y + 1) }) else Nothing -- S
    , if borderPositions |> List.member (x + 1, y) then Just (Horizontal { startPos = (x + 1, y), endPos = (x + 1, y + 1) }) else Nothing -- E 
    ] |> List.filterMap identity

fillPlant : Set Pos -> Set Pos -> Plot -> List (Plot) -> List Border -> ((Plot, List (Plot), List Border), Set Pos)
fillPlant possiblePositionsLeft positionsToAdd plot seq borders = 
  if Set.isEmpty positionsToAdd then 
    ((plot, seq, borders), possiblePositionsLeft) 
  else 
    let 
      nextPlot = Set.union plot positionsToAdd
      nextSeq = nextPlot :: seq
      positionsToAddList = positionsToAdd |> Set.toList 
      nextPositionsToAdd = 
        positionsToAddList
        |> List.concatMap getNeighbourCandidates 
        |> Set.fromList 
        |> Set.filter (\p -> (Set.member p possiblePositionsLeft))
      newBorders = 
        positionsToAddList
        |> List.concatMap (getBorders possiblePositionsLeft plot)
      nextBorders = borders ++ newBorders 
      possible = Set.diff possiblePositionsLeft nextPositionsToAdd
    in 
      fillPlant possible nextPositionsToAdd nextPlot nextSeq nextBorders

fillPlantPlot : Set Pos -> Pos -> ((Plot, List (Plot), List Border), Set Pos)
fillPlantPlot plantPositions startPos = 
  let 
    positionsToAdd = Set.empty |> Set.insert startPos 
  in 
    fillPlant plantPositions positionsToAdd Set.empty [] []

findPlantPlotsLoop : Plant -> List Pos -> List (Plant, (Plot, PlotSequence, List Border)) -> List (Plant, (Plot, PlotSequence, List Border))
findPlantPlotsLoop plant posList plotInfoList = 
  case posList of 
    [] -> plotInfoList 
    pos :: remaining -> 
      let 
        remSet = Set.fromList remaining
        ((plot, seq, borders), updatedRemSet) = fillPlantPlot remSet pos 
      in 
        findPlantPlotsLoop plant (Set.toList updatedRemSet) ((plant, (plot, seq, borders)) :: plotInfoList)

findPlantPlots : Plant -> List Pos -> List (Plant, (Plot, PlotSequence, List Border)) 
findPlantPlots plant posList = 
  findPlantPlotsLoop plant posList [] 

findAllPlots : Garden -> List (Plant, (Plot, PlotSequence, List Border)) 
findAllPlots garden = 
  let 
    positionsWithPlant = garden |> getAllPositions |> List.map (\pos -> (pos, tryGetPlantAtPos garden pos |> Maybe.withDefault '?'))
    selectByPlant p = 
      positionsWithPlant |> List.filterMap (\(pos, plant) -> if plant == p then Just pos else Nothing)
    getPlots p = 
      p |> selectByPlant |> findPlantPlots p
    aPlots = getPlots 'A'
    bPlots = getPlots 'B'
    cPlots = getPlots 'C'
    dPlots = getPlots 'D'
    ePlots = getPlots 'E'
    fPlots = getPlots 'F'
    gPlots = getPlots 'G'
    hPlots = getPlots 'H'
    iPlots = getPlots 'I'
    jPlots = getPlots 'J'
    kPlots = getPlots 'K'
    lPlots = getPlots 'L'
    mPlots = getPlots 'M'
    nPlots = getPlots 'N'
    oPlots = getPlots 'O'
    pPlots = getPlots 'P'
    qPlots = getPlots 'Q'
    rPlots = getPlots 'R'
    sPlots = getPlots 'S'
    tPlots = getPlots 'T'
    uPlots = getPlots 'U'
    vPlots = getPlots 'V'
    wPlots = getPlots 'W'
    xPlots = getPlots 'X'
    yPlots = getPlots 'Y'
    zPlots = getPlots 'Z'
  in 
    [ aPlots
    , bPlots 
    , cPlots
    , dPlots
    , ePlots
    , fPlots
    , gPlots
    , hPlots
    , iPlots
    , jPlots
    , kPlots
    , lPlots
    , mPlots
    , nPlots
    , oPlots
    , pPlots
    , qPlots
    , rPlots
    , sPlots
    , tPlots
    , uPlots
    , vPlots
    , wPlots
    , xPlots
    , yPlots
    , zPlots ] |> List.concat

toPlotColor : Int -> String 
toPlotColor index = 
  let 
    hue = (index * 79) |> modBy 255
    saturation = 90
    lightness = 90
    hueStr = String.fromInt hue 
    satStr = (String.fromInt saturation) ++ "%"
    lgtStr = (String.fromInt lightness) ++ "%"
  in 
    "hsl(" ++ hueStr ++ ", " ++ satStr ++ ", " ++ lgtStr ++ ")"

getPlantColor : Plant -> String 
getPlantColor plant = 
  plant |> Char.toCode |> toPlotColor

toPlotInfo : Int -> (Plant, (Plot, PlotSequence, List Border)) -> PlotInfo 
toPlotInfo index (plant, (plot, seq, borders)) = 
  let 
    totalSteps = List.length seq 
    area = Set.size plot
    plantColor = getPlantColor plant
  in 
    { complete = plot  
    , sequence = seq |> List.reverse
    , totalSteps = List.length seq 
    , plant = plant 
    , color = plantColor
    , area = area  
    , perimeter = 0 
    , perimeterDiscount = 0 
    , fenceCost = 0  
    , fenceCostDiscount = 0 }

initModel : DataSource -> Model 
initModel dataSource = 
  let 
    data = read dataSource
    rows = data |> String.split "\n"
    numberOfRows = rows |> List.length 
    numberOfCols = rows |> List.head |> Maybe.withDefault "?" |> String.length 
    createRowTuples y rowStr = 
      rowStr |> String.toList |> List.indexedMap (\x ch -> ((x, y), ch)) 
    garden = rows |> List.indexedMap createRowTuples |> List.concat |> Dict.fromList
    plotList = findAllPlots garden 
    plotInfoList = plotList |> List.indexedMap toPlotInfo 
    maxSteps = plotInfoList |> List.map (\pi -> pi.totalSteps) |> List.maximum |> Maybe.withDefault 0
  in 
    { plotInfoList = plotInfoList  
    , rowCount = numberOfRows
    , colCount = numberOfCols
    , step = 0 
    , maxSteps = maxSteps
    , totalCost = 0 
    , totalCostDiscount = 0  
    , dataSource = dataSource
    , paused = True
    , finished = False  
    , tickInterval = defaultTickInterval 
    , message = "?" }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel SampleLarger, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | PrevStep 
  | NextStep 
  | TogglePlay 
  | Clear 
  | UseInput
  | UseSample
  | UseXoXo
  | UseLarger
  | UseEShape
  | UseAbba

updateClear : Model -> Model
updateClear model = 
  initModel model.dataSource

updatePrevStep : Model -> Model 
updatePrevStep model =
  let 
    prevStep = Basics.max 0 (model.step - 1)
  in 
    { model | step = prevStep, finished = False, message = "Prev step."}

updateNextStep : Model -> Model
updateNextStep model =
  let 
    nextStep = Basics.min model.maxSteps (model.step + 1)
    finished = nextStep == model.maxSteps 
    paused = model.paused || finished
    -- paused = if model.paused then model.paused else nextStep == model.maxSteps 
  in 
    { model | step = nextStep, paused = paused, finished = finished, message = "Next step : " ++ String.fromInt nextStep }

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  if model.finished then 
    let 
      m = initModel model.dataSource
    in 
      {m | paused = False }
  else 
    { model | paused = not model.paused }

updateUseInput : Model -> Model
updateUseInput model = 
  initModel Input 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> 
      (updateClear model, Cmd.none)
    Tick ->
      (updateNextStep model, Cmd.none)
    PrevStep ->
      (updatePrevStep model, Cmd.none)
    NextStep ->
      (updateNextStep model, Cmd.none)
    TogglePlay -> 
      (updateTogglePlay model, Cmd.none)
    UseInput -> 
      (initModel Input, Cmd.none)
    UseSample -> 
      (initModel Sample, Cmd.none)
    UseXoXo -> 
      (initModel SampleXoXo, Cmd.none)
    UseLarger -> 
      (initModel SampleLarger, Cmd.none)
    UseEShape -> 
      (initModel SampleEShape, Cmd.none)
    UseAbba -> 
      (initModel SampleAbba, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

-- VIEW

toPlantElement : Int -> Int -> String -> Pos -> Svg Msg 
toPlantElement plantWidth plantHeight plantStr (xInt, yInt) = 
  let 
    xStr = String.fromInt ((plantWidth // 4) + xInt * plantWidth)
    yStr = String.fromInt (plantHeight + yInt * plantHeight)
  in 
    Svg.text_ [ x xStr, y yStr ] [ Svg.text plantStr ]

toFilledElement : Int -> Int -> String -> Pos -> Svg Msg 
toFilledElement plantWidth plantHeight colorStr (xInt, yInt) = 
  let 
    xStr = String.fromInt ((plantWidth // 6) + xInt * plantWidth)
    yStr = String.fromInt ((plantWidth // 6) + yInt * plantHeight)
  in 
    rect
          [ x xStr
          , y yStr
          , width (String.fromInt plantWidth)
          , height (String.fromInt plantHeight)
          , fill colorStr
          ]
          []

toPlotSvgElements : Int -> Int -> Int -> PlotInfo -> List (Svg Msg)
toPlotSvgElements step plantWidth plantHeight plotInfo = 
  let 
    posList = plotInfo.complete |> Set.toList
    colorStr = plotInfo.color
    plantStr = String.fromChar plotInfo.plant 
    plantElements = posList |> List.map (toPlantElement plantWidth plantHeight plantStr)
    filled = 
      if step == 0 then [] 
      else 
        case plotInfo.sequence |> List.drop (step - 1) |> List.head of 
          Just plot -> plot |> Set.toList 
          Nothing -> posList
    filledElements = filled |> List.map (toFilledElement plantWidth plantHeight colorStr)
  in 
    filledElements ++ plantElements

toSvg : Model -> Html Msg 
toSvg model = 
  let 
    (fontSize, plantWidth, plantHeight) = 
      case model.dataSource of 
        Input -> ("8px", 6, 8)
        _ -> ("16px", 12, 16)
    svgWidth = (4 + plantWidth * model.colCount) |> String.fromInt 
    svgHeight = (4 + plantHeight * model.rowCount) |> String.fromInt 
    step = model.step 
    plotInfoList = model.plotInfoList 
    elements = model.plotInfoList |> List.concatMap (toPlotSvgElements step plantWidth plantHeight)
    rects = []
    viewBoxStr = [ "0", "0", svgWidth, svgHeight ] |> String.join " "
    fontFamilyAttr = "font-family:Source Code Pro,monospace"
    fontSizeAttr = "font-size:" ++ fontSize
    styles = fontFamilyAttr ++ "; " ++ fontSizeAttr 
  in 
    svg
      [ viewBox viewBoxStr
      , width svgWidth
      , height svgHeight
      -- , Svg.Attributes.style "background-color:lightgreen; font-family:Source Code Pro,monospace"
      , Svg.Attributes.style styles
      ]
      elements

view : Model -> Html Msg
view model =
  let
    elements = []
    textFontSize = "9px"
    s = toSvg model 
    message = model.plotInfoList |> List.length |> String.fromInt
  in 
    Html.table 
      [ Html.Attributes.style "width" "1080px"]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "40px"
              , Html.Attributes.style "padding" "20px"]
              [ Html.div [] [Html.text "Advent of Code 2024" ]
              , Html.div [] [Html.text "Day 12: Garden Groups" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ 
                Html.input 
                [ Html.Attributes.type_ "radio", onClick UseInput, Html.Attributes.checked (model.dataSource == Input) ] 
                []
              , Html.label [] [ Html.text "Input" ]
              , 
                Html.input 
                [ Html.Attributes.type_ "radio", onClick UseSample, Html.Attributes.checked (model.dataSource == Sample) ] 
                []
              , Html.label [] [ Html.text "Sample" ]
              , Html.input 
                [ Html.Attributes.type_ "radio", onClick UseXoXo, Html.Attributes.checked (model.dataSource == SampleXoXo) ] 
                []
              , Html.label [] [ Html.text "XOXO" ]
              , Html.input 
                [ Html.Attributes.type_ "radio", onClick UseLarger, Html.Attributes.checked (model.dataSource == SampleLarger) ] 
                []
              , Html.label [] [ Html.text "Larger" ]
              , Html.input 
                [ Html.Attributes.type_ "radio", onClick UseEShape, Html.Attributes.checked (model.dataSource == SampleEShape) ] 
                []
              , Html.label [] [ Html.text "E-shape" ]
              , Html.input 
                [ Html.Attributes.type_ "radio", onClick UseAbba, Html.Attributes.checked (model.dataSource == SampleAbba) ] 
                []
              , Html.label [] [ Html.text "ABBA" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Clear ] 
                [ Html.text "Clear" ] 
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px"
                , Html.Attributes.disabled (model.step == 0)
                , onClick PrevStep ] 
                [ Html.text "Prev" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick TogglePlay ] 
                [ if model.paused then text "Play" else text "Pause" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px"
                , Html.Attributes.disabled (model.step == model.maxSteps)
                , onClick NextStep ] 
                [ Html.text "Next" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] [ Html.text (String.fromInt model.step) ]
              , Html.div [] [ Html.text message ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "padding" "20px"] 
              [ Html.div [ Html.Attributes.align "center" ] [ s ] 
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
