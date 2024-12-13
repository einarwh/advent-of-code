module Aoc12 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Set exposing (Set)
import Array2D exposing (Array2D)
-- import Html exposing (text)
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

type alias Plot = Set Pos 

type alias PlotSequence = List Plot 

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

getAllPositions : Array2D Plant -> List Pos
getAllPositions board = 
  let
    ys = List.range 0 (Array2D.rows board - 1)
    xs = List.range 0 (Array2D.columns board - 1)
  in 
    ys |> List.concatMap (\y -> xs |> List.map (\x -> (x, y)))

inGardenBounds : Array2D Plant -> Pos -> Bool 
inGardenBounds garden (x, y) = 
  let 
    insideRows = y >= 0 && y < Array2D.rows garden
    insideCols = x >= 0 && x < Array2D.columns garden
  in 
    insideRows && insideCols 

tryGetPlantAtPos : Array2D Plant -> Pos -> Maybe Char
tryGetPlantAtPos garden (x, y) = 
  garden |> Array2D.get y x 

neighbours : Pos -> List Pos 
neighbours (x, y) = 
  [ ((x - 1), y), ((x + 1), y), (x, (y - 1)), (x, (y + 1)) ]

fillLoop : Array2D Plant -> Plant -> List Pos -> (Plot, PlotSequence) -> (Plot, PlotSequence)
fillLoop garden plant candidates (plot, seq) = 
  let 
    -- inside = List.filter (inGardenBounds garden) candidates
    unseen = List.filter (\p -> not <| Set.member p plot) candidates 
    verified = List.filter (\p -> plant == ((tryGetPlantAtPos garden p) |> Maybe.withDefault '?')) unseen
    nextCandidates = List.concatMap neighbours verified |> Set.fromList |> Set.toList
  in 
    if List.length nextCandidates == 0 then 
      (plot, seq)
    else 
      let 
        nextPlot = Set.union plot (Set.fromList verified) 
        nextSeq = nextPlot :: seq 
      in 
        fillLoop garden plant nextCandidates (nextPlot, nextSeq)

fillPlot : Array2D Plant -> Pos -> Maybe (Plant, Plot, PlotSequence) 
fillPlot garden pos = 
  case tryGetPlantAtPos garden pos of 
    Nothing -> Nothing 
    Just plant -> 
      let 
        (plot, seq) = fillLoop garden plant [pos] (Set.empty, []) 
      in 
        Just (plant, plot, seq |> List.reverse)

findPlotsLoop : Array2D Plant -> List (Plant, Plot, PlotSequence) -> Set Pos -> List Pos -> List (Plant, Plot, PlotSequence) 
findPlotsLoop garden plotList visited positions = 
  case positions of 
    [] -> plotList
    (pos :: remaining) -> 
      if visited |> Set.member pos then 
        findPlotsLoop garden plotList visited remaining
      else 
        case fillPlot garden pos of 
          Just (plant, plot, seq) -> 
            findPlotsLoop garden ((plant, plot, seq) :: plotList) (Set.union visited plot) remaining
          Nothing -> 
            []

findPlots : Array2D Plant -> List (Plant, Plot, PlotSequence)
findPlots garden = 
  garden |> getAllPositions |> findPlotsLoop garden [] Set.empty

toPlotColor : Int -> String 
toPlotColor index = 
  let 
    hue = (index * 71) |> modBy 255
    saturation = 90
    lightness = 90
    hueStr = String.fromInt hue 
    satStr = (String.fromInt saturation) ++ "%"
    lgtStr = (String.fromInt lightness) ++ "%"
  in 
    "hsl(" ++ hueStr ++ ", " ++ satStr ++ ", " ++ lgtStr ++ ")"

toPlotInfo : Int -> (Plant, Plot, PlotSequence) -> PlotInfo 
toPlotInfo index (plant, plot, seq) = 
  let 
    totalSteps = List.length seq 
    area = Set.size plot
  in 
    { complete = plot  
    , sequence = seq
    , totalSteps = List.length seq 
    , plant = plant 
    , color = toPlotColor index
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
    garden = rows |> List.map (String.toList) |> Array2D.fromList
    plotList = findPlots garden 
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
  | Faster 
  | Slower 
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
    xStr = String.fromInt (2 + xInt * plantWidth)
    yStr = String.fromInt (2 + yInt * plantHeight)
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
    plantWidth = 12
    plantHeight = 16  
    svgWidth = (4 + plantWidth * model.colCount) |> String.fromInt 
    svgHeight = (4 + plantHeight * model.rowCount) |> String.fromInt 
    step = model.step 
    plotInfoList = model.plotInfoList 
    elements = model.plotInfoList |> List.concatMap (toPlotSvgElements step plantWidth plantHeight)
    rects = []
    viewBoxStr = [ "0", "0", svgWidth, svgHeight ] |> String.join " "
  in 
    svg
      [ viewBox viewBoxStr
      , width svgWidth
      , height svgHeight
      -- , Svg.Attributes.style "background-color:lightgreen; font-family:Source Code Pro,monospace"
      , Svg.Attributes.style "font-family:Source Code Pro,monospace"
      ]
      elements

view : Model -> Html Msg
view model =
  let
    elements = []
    textFontSize = "9px"
    s = toSvg model 
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
              --   Html.input 
              --   [ Html.Attributes.type_ "radio", onClick UseInput, Html.Attributes.checked (model.dataSource == Input) ] 
              --   []
              -- , Html.label [] [ Html.text "Input" ]
              -- , 
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
                [ Html.Attributes.style "width" "80px", onClick PrevStep ] 
                [ Html.text "Prev" ]
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
                [ Html.Attributes.style "width" "80px", onClick NextStep ] 
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
              , Html.div [] [ Html.text model.message ]
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
