module Aoc12 exposing (..)

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

defaultTickInterval : Float
defaultTickInterval = 10

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type alias Pos = (Int, Int)

type Dir = N | W | S | E

type Move = Turn | Forward

type Visit = Vertical | Horizontal | Both 

type Cell = Highlight Char | Plain Char 

type DataSource = Input | Sample | SampleXo | SampleLarger | SampleE | SampleAbba

type alias Plot = 
  { complete : Set Pos 
  , sequence : List (Set Pos)
  , totalSteps : Int 
  , plant : String 
  , color : String
  , area : Int 
  , perimeter : Int
  , perimeterDiscount : Int
  , fenceCost : Int 
  , fenceCostDiscount : Int }

type alias Model = 
  { plots : List Plot 
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

sampleE = """EEEEE
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

sampleXo = """OOOOO
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

--data |> String.split "\n" |> List.map (String.toList) |> Array2D.fromList

read : DataSource -> String
read dataSource = 
  case dataSource of 
    Input -> input
    Sample -> sample
    SampleAbba -> sampleAbba
    SampleE -> sampleE 
    SampleLarger -> sampleLarger
    SampleXo -> sampleXo

initModel : DataSource -> Model 
initModel dataSource = 
  let 
    data = read dataSource
    garden = String.split "\n" |> List.map (String.toList) |> Array2D.fromList
    plots = []
  in 
    { plots = plots 
    , step = 0 
    , maxSteps = 0
    , totalCost = 0 
    , totalCostDiscount = 0  
    , dataSource = dataSource
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
  | PrevStep 
  | NextStep 
  | TogglePlay 
  | Faster 
  | Slower 
  | Clear 
  | ChangeDataSource 

getAllPositions : Array2D Char -> List Pos
getAllPositions board = 
  let
    ys = List.range 0 (Array2D.rows board - 1)
    xs = List.range 0 (Array2D.columns board - 1)
  in 
    ys |> List.concatMap (\y -> xs |> List.map (\x -> (x, y)))

updateClear : Model -> Model
updateClear model = 
  initModel model.dataSource

updatePrevStep : Model -> Model 
updatePrevStep model = model 

updateNextStep : Model -> Model
updateNextStep model = model 

updateTogglePlay : Model -> Model
updateTogglePlay model = 
  if model.finished then 
    let 
      m = initModel model.dataSource
    in 
      {m | paused = False }
  else 
    { model | paused = not model.paused }

updateToggleSample : Model -> Model
updateToggleSample model = 
  let
    dataSource = model.dataSource
  in
    initModel dataSource 

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
    ChangeDataSource -> 
      (updateToggleSample model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let 
    tickSub = if model.paused then Sub.none else Time.every model.tickInterval (\_ -> Tick)
  in 
    tickSub

-- VIEW

toCharElement : Array2D Cell -> Pos -> Html Msg 
toCharElement vizBoard (x, y) = 
    case Array2D.get y x vizBoard of 
      Nothing -> Html.text "?"
      Just cell -> 
        case cell of 
          Highlight ch -> 
            (Html.span [Html.Attributes.style "background-color" "#CCCCCC" ] [ Html.text (String.fromChar ch) ]) 
          Plain ch -> 
            Html.text (String.fromChar ch)

view : Model -> Html Msg
view model =
  let
    elements = []
    textFontSize = "9px"
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
              , Html.div [] [Html.text "Day 12: Garden Groups." ] ] ]
      -- , Html.tr 
      --     []
      --     [ Html.td 
      --         [ Html.Attributes.align "center" ]
      --         [ Html.input 
      --           [ Html.Attributes.type_ "radio", onClick EnablePart1, Html.Attributes.checked (model.mode == Part1) ] 
      --           []
      --         , Html.label [] [ Html.text "Part 1" ]
      --         , Html.input 
      --           [ Html.Attributes.type_ "radio", onClick EnablePart2, Html.Attributes.checked (model.mode == Part2) ] 
      --           []
      --         , Html.label [] [ Html.text "Part 2" ]
      --       ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick ChangeDataSource ] 
                [ Html.text "Input?" ]
              , Html.button 
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
                Html.div [] [ Html.text "foo" ]
              , Html.div [] [ Html.text "bar" ]
              ] ]
      -- , Html.tr 
      --     []
      --     [ Html.td 
      --         [ Html.Attributes.align "center"
      --         , Html.Attributes.style "background-color" "white" 
      --         , Html.Attributes.style "font-family" "Courier New"
      --         , Html.Attributes.style "font-size" "24px"
      --         , Html.Attributes.style "width" "200px" ] 
      --         [ 
      --           Html.div [] [ Html.text model.message ]
      --         ] ]
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
