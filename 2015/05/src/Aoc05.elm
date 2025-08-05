module Aoc05 exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Set exposing (Set)
import Html exposing (text)

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type DataSource = Input | Sample1 | Sample2

type Report = Unchecked String | Safe String | Unsafe String

type alias Model = 
  { safeReports : Int 
  , dataSource : DataSource 
  , useNewRules : Bool 
  , reports : List Report
  , lastCommandText : String
  , counter : Int 
  , debug : String }

containsThreeVowels : String -> Bool 
containsThreeVowels str = 
    let 
        vowels = "aeiou" |> String.toList |> Set.fromList
        count = str |> String.toList |> List.filter (\ch -> Set.member ch vowels) |> List.length 
    in 
        count >= 3

pairwise : List a -> List (a, a)
pairwise xs =
  case xs of 
  [] -> [] 
  _ :: t -> 
    List.map2 Tuple.pair xs t

containsLetterPair : String -> Bool 
containsLetterPair str = 
  str |> String.toList |> pairwise |> List.any (\(a, b) -> a == b)

containsDisallowedSubstring : String -> Bool 
containsDisallowedSubstring str = 
  let
    disallowed = ["ab", "cd", "pq", "xy"]
  in 
    disallowed |> List.any (\s -> String.contains s str)

doesNotContainDisallowedSubstring : String -> Bool
doesNotContainDisallowedSubstring str = 
  str |> containsDisallowedSubstring |> not 

isNiceOldRules : String -> Bool 
isNiceOldRules str = 
    containsThreeVowels str && containsLetterPair str && doesNotContainDisallowedSubstring str 

containsPairWithoutOverlap : String -> Bool
containsPairWithoutOverlap str = 
  if String.length str < 2 then False 
  else 
    let 
      two = String.left 2 str 
      rest = String.dropLeft 2 str 
      found = String.contains two rest 
    in  
      found || containsPairWithoutOverlap (String.dropLeft 1 str)

containsRepeatingLetterLoop : List Char -> Bool
containsRepeatingLetterLoop chars = 
  case chars of 
    a :: b :: c :: rest -> 
      a == c || containsRepeatingLetterLoop (b :: c :: rest)
    _ -> False 

containsRepeatingLetter : String -> Bool
containsRepeatingLetter str = 
    str |> String.toList |> containsRepeatingLetterLoop

isNiceNewRules : String -> Bool 
isNiceNewRules str = 
    containsPairWithoutOverlap str && containsRepeatingLetter str 

isNice : Bool -> String -> Bool 
isNice useNewRules str = 
    if useNewRules then 
        isNiceNewRules str 
    else 
        isNiceOldRules str 

checkReport : Bool -> Report -> Report 
checkReport useNewRules report = 
  case report of 
    Unchecked str -> 
      if isNice useNewRules str then Safe str else Unsafe str
    _ -> report 

initReports : DataSource -> List Report
initReports dataSource = 
  let 
    sample1 = """ugknbfddgicrmopn
aaa
jchzalrnumimnmhp
haegwjzuvuyypxyu
dvszwmarrgswjxmb"""
    sample2 = """qjhvhtzxzqqjkmpb
xxyxx
uurcxstgmygtbstg
ieodomkazucvgmuy"""
    input = """zgsnvdmlfuplrubt
vlhagaovgqjmgvwq
ffumlmqwfcsyqpss
zztdcqzqddaazdjp
eavfzjajkjesnlsb
urrvucyrzzzooxhx
xdwduffwgcptfwad
orbryxwrmvkrsxsr
jzfeybjlgqikjcow
mayoqiswqqryvqdi
iiyrkoujhgpgkcvx
egcgupjkqwfiwsjl
zbgtglaqqolttgng
eytquncjituzzhsx
dtfkgggvqadhqbwb
zettygjpcoedwyio
rwgwbwzebsnjmtln
esbplxhvzzgawctn
vnvshqgmbotvoine
wflxwmvbhflkqxvo
twdjikcgtpvlctte
minfkyocskvgubvm
sfxhhdhaopajbzof
sofkjdtalvhgwpql
uqfpeauqzumccnrc
tdflsbtiiepijanf
dhfespzrhecigzqb
xobfthcuuzhvhzpn
olgjglxaotocvrhw
jhkzpfcskutwlwge
zurkakkkpchzxjhq
hekxiofhalvmmkdl
azvxuwwfmjdpjskj
arsvmfznblsqngvb
ldhkzhejofreaucc
adrphwlkehqkrdmo
wmveqrezfkaivvaw
iyphmphgntinfezg
blomkvgslfnvspem
cgpaqjvzhbumckwo
ydhqjcuotkeyurpx
sbtzboxypnmdaefr
vxrkhvglynljgqrg
ttgrkjjrxnxherxd
hinyfrjdiwytetkw
sufltffwqbugmozk
tohmqlzxxqzinwxr
jbqkhxfokaljgrlg
fvjeprbxyjemyvuq
gmlondgqmlselwah
ubpwixgxdloqnvjp
lxjfhihcsajxtomj
qouairhvrgpjorgh
nloszcwcxgullvxb
myhsndsttanohnjn
zjvivcgtjwenyilz
qaqlyoyouotsmamm
tadsdceadifqthag
mafgrbmdhpnlbnks
aohjxahenxaermrq
ovvqestjhbuhrwlr
lnakerdnvequfnqb
agwpwsgjrtcjjikz
lhlysrshsmzryzes
xopwzoaqtlukwwdu
xsmfrfteyddrqufn
ohnxbykuvvlbbxpf
bbdlivmchvzfuhoc
vtacidimfcfyobhf
tinyzzddgcnmiabd
tcjzxftqcqrivqhn
vgnduqyfpokbmzim
revkvaxnsxospyow
ydpgwxxoxlywxcgi
wzuxupbzlpzmikel
nscghlafavnsycjh
xorwbquzmgmcapon
asmtiycegeobfxrn
eqjzvgkxgtlyuxok
mmjrskloposgjoqu
gceqosugbkvytfto
khivvoxkvhrgwzjl
qtmejuxbafroifjt
ttmukbmpoagthtfl
bxqkvuzdbehtduwv
gvblrpzjylanoggj
cltewhyjxdbmbtqj
fbkgedqvomdipklj
uxvuplhenqawfcjt
fkdjmayiawdkycva
gnloqfgbnibzyidh
kyzorvtopjiyyyqg
drckpekhpgrioblt
tvhrkmbnpmkkrtki
khaldwntissbijiz
aoojqakosnaxosom
xfptccznbgnpfyqw
moqdwobwhjxhtrow
chfwivedutskovri
gprkyalfnpljcrmi
pwyshpwjndasykst
xuejivogihttzimd
bugepxgpgahtsttl
zufmkmuujavcskpq
urybkdyvsrosrfro
isjxqmlxwtqmulbg
pxctldxgqjqhulgz
hclsekryiwhqqhir
hbuihpalwuidjpcq
ejyqcxmfczqfhbxa
xljdvbucuxnnaysv
irqceqtqwemostbb
anfziqtpqzqdttnz
cgfklbljeneeqfub
zudyqkuqqtdcpmuo
iuvhylvznmhbkbgg
mpgppmgfdzihulnd
argwmgcvqqkxkrdi
pdhrfvdldkfihlou
cbvqnjrvrsnqzfob
lkvovtsqanohzcmm
vxoxjdyoylqcnyzt
kurdpaqiaagiwjle
gwklwnazaxfkuekn
rbaamufphjsjhbdl
tzbrvaqvizhsisbd
pbcqlbfjvlideiub
hiwoetbfywaeddtx
fjirczxtuupfywyf
omeoegeyyospreem
ozbbpupqpsskvrjh
pzvcxkvjdiyeyhxa
odclumkenabcsfzr
npdyqezqdjqaszvm
yodkwzmrhtexfrqa
rjcmmggjtactfrxz
mioxfingsfoimual
aqskaxjjborspfaa
wientdsttkevjtkf
tdaswkzckmxnfnct
voucjhzvkkhuwoqk
boaaruhalgaamqmh
iufzxutxymorltvb
pfbyvbayvnrpijpo
obztirulgyfthgcg
ntrenvhwxypgtjwy
ephlkipjfnjfjrns
pkjhurzbmobhszpx
gqbnjvienzqfbzvj
wjelolsrbginwnno
votanpqpccxqricj
bxyuyiglnmbtvehi
qyophcjfknbcbjrb
anoqkkbcdropskhj
tcnyqaczcfffkrtl
rsvqimuqbuddozrf
meppxdrenexxksdt
tyfhfiynzwadcord
wayrnykevdmywycf
mhowloqnppswyzbu
tserychksuwrgkxz
xycjvvsuaxsbrqal
fkrdsgaoqdcqwlpn
vrabcmlhuktigecp
xgxtdsvpaymzhurx
ciabcqymnchhsxkc
eqxadalcxzocsgtr
tsligrgsjtrnzrex
qeqgmwipbspkbbfq
vzkzsjujltnqwliw
ldrohvodgbxokjxz
jkoricsxhipcibrq
qzquxawqmupeujrr
mizpuwqyzkdbahvk
suupfxbtoojqvdca
ywfmuogvicpywpwm
uevmznxmsxozhobl
vjbyhsemwfwdxfxk
iyouatgejvecmtin
tcchwpuouypllcxe
lgnacnphdiobdsef
uoxjfzmdrmpojgbf
lqbxsxbqqhpjhfxj
knpwpcnnimyjlsyz
fezotpoicsrshfnh
dkiwkgpmhudghyhk
yzptxekgldksridv
pckmzqzyiyzdbcts
oqshafncvftvwvsi
yynihvdywxupqmbt
iwmbeunfiuhjaaic
pkpkrqjvgocvaxjs
ieqspassuvquvlyz
xshhahjaxjoqsjtl
fxrrnaxlqezdcdvd
pksrohfwlaqzpkdd
ravytrdnbxvnnoyy
atkwaifeobgztbgo
inkcabgfdobyeeom
ywpfwectajohqizp
amcgorhxjcybbisv
mbbwmnznhafsofvr
wofcubucymnhuhrv
mrsamnwvftzqcgta
tlfyqoxmsiyzyvgv
ydceguvgotylwtea
btyvcjqhsygunvle
usquiquspcdppqeq
kifnymikhhehgote
ybvkayvtdpgxfpyn
oulxagvbavzmewnx
tvvpekhnbhjskzpj
azzxtstaevxurboa
nfmwtfgrggmqyhdf
ynyzypdmysfwyxgr
iaobtgubrcyqrgmk
uyxcauvpyzabbzgv
fbasfnwiguasoedc
mgmjoalkbvtljilq
szgkxiqkufdvtksb
xgfzborpavdmhiuj
hmuiwnsonvfgcrva
zolcffdtobfntifb
mvzgcsortkugvqjr
pbbpgraaldqvzwhs
zvsxegchksgnhpuv
kdpdboaxsuxfswhx
jdfggigejfupabth
tpeddioybqemyvqz
mxsntwuesonybjby
tzltdsiojfvocige
ubtdrneozoejiqrv
fusyucnhncoxqzql
nlifgomoftdvkpby
pyikzbxoapffbqjw
hzballplvzcsgjug
ymjyigsfehmdsvgz
vpqgyxknniunksko
ffkmaqsjxgzclsnq
jcuxthbedplxhslk
ymlevgofmharicfs
nyhbejkndhqcoisy
rjntxasfjhnlizgm
oqlnuxtzhyiwzeto
tntthdowhewszitu
rmxyoceuwhsvfcua
qpgsjzwenzbxyfgw
sumguxpdkocyagpu
ymfrbxwrawejkduu
hetgrtmojolbmsuf
qzqizpiyfasgttex
qnmoemcpuckzsshx
ddyqiihagcmnxccu
oirwxyfxxyktgheo
phpaoozbdogbushy
uctjdavsimsrnvjn
aurbbphvjtzipnuh
hpbtrubopljmltep
pyyvkthqfsxqhrxg
jdxaiqzkepxbfejk
ukgnwbnysrzvqzlw
lfkatkvcssnlpthd
ucsyecgshklhqmsc
rwdcbdchuahkvmga
rxkgqakawgpwokum
hbuyxeylddfgorgu
tbllspqozaqzglkz
rqfwizjlbwngdvvi
xuxduyzscovachew
kouiuxckkvmetvdy
ycyejrpwxyrweppd
trctlytzwiisjamx
vtvpjceydunjdbez
gmtlejdsrbfofgqy
jgfbgtkzavcjlffj
tyudxlpgraxzchdk
gyecxacqitgozzgd
rxaocylfabmmjcvt
tornfzkzhjyofzqa
kocjcrqcsvagmfqv
zfrswnskuupivzxb
cunkuvhbepztpdug
pmpfnmklqhcmrtmf
tfebzovjwxzumxap
xpsxgaswavnzkzye
lmwijdothmxclqbr
upqxhmctbltxkarl
axspehytmyicthmq
xdwrhwtuooikehbk
tpggalqsytvmwerj
jodysbwnymloeqjf
rxbazvwuvudqlydn
ibizqysweiezhlqa
uexgmotsqjfauhzp
ldymyvumyhyamopg
vbxvlvthgzgnkxnf
pyvbrwlnatxigbrp
azxynqididtrwokb
lwafybyhpfvoawto
ogqoivurfcgspytw
cinrzzradwymqcgu
sgruxdvrewgpmypu
snfnsbywuczrshtd
xfzbyqtyxuxdutpw
fmpvjwbulmncykbo
ljnwoslktrrnffwo
ceaouqquvvienszn
yjomrunrxjyljyge
xpmjsapbnsdnbkdi
uetoytptktkmewre
eixsvzegkadkfbua
afaefrwhcosurprw
bwzmmvkuaxiymzwc
gejyqhhzqgsrybni
gjriqsfrhyguoiiw
gtfyomppzsruhuac
ogemfvmsdqqkfymr
jgzbipsygirsnydh
zghvlhpjnvqmocgr
ngvssuwrbtoxtrka
ietahyupkbuisekn
gqxqwjizescbufvl
eiprekzrygkncxzl
igxfnxtwpyaamkxf
soqjdkxcupevbren
fspypobyzdwstxak
qstcgawvqwtyyidf
gsccjacboqvezxvd
bfsblokjvrqzphmc
srezeptvjmncqkec
opmopgyabjjjoygt
msvbufqexfrtecbf
uiaqweyjiulplelu
pbkwhjsibtwjvswi
xwwzstmozqarurrq
nytptwddwivtbgyq
ejxvsufbzwhzpabr
jouozvzuwlfqzdgh
gfgugjihbklbenrk
lwmnnhiuxqsfvthv
bzvwbknfmaeahzhi
cgyqswikclozyvnu
udmkpvrljsjiagzi
zzuhqokgmisguyna
ekwcdnjzuctsdoua
eueqkdrnzqcaecyd
lnibwxmokbxhlris
fdrbftgjljpzwhea
iabvuhhjsxmqfwld
qgogzkynrgejakta
mfcqftytemgnpupp
klvhlhuqhosvjuqk
gdokmxcgoqvzvaup
juududyojcazzgvr
fyszciheodgmnotg
yfpngnofceqfvtfs
cahndkfehjumwavc
dxsvscqukljxcqyi
cqukcjtucxwrusji
vevmmqlehvgebmid
ahswsogfrumzdofy
ftasbklvdquaxhxb
tsdeumygukferuif
ybfgbwxaaitpwryg
djyaoycbymezglio
trzrgxdjqnmlnzpn
rumwchfihhihpqui
ffrvnsgrnzemksif
oizlksxineqknwzd
cirqcprftpjzrxhk
zrhemeqegmzrpufd
kqgatudhxgzlgkey
syjugymeajlzffhq
nlildhmgnwlopohp
flcszztfbesqhnyz
ohzicmqsajyqptrw
ebyszucgozsjbelq
enxbgvvcuqeloxud
ubwnvecbsmhkxwuk
noifliyxvlkqphbo
hazlqpetgugxxsiz
ihdzoerqwqhgajzb
ivrdwdquxzhdrzar
synwycdvrupablib
mqkdjkntblnmtvxj
qmmvoylxymyovrnq
pjtuxskkowutltlq
gchrqtloggkrjciz
namzqovvsdipazae
yfokqhkmakyjzmys
iapxlbuoiwqfnozm
fbcmlcekgfdurqxe
ednzgtczbplwxjlq
gdvsltzpywffelsp
oaitrrmpqdvduqej
gseupzwowmuuibjo
dfzsffsqpaqoixhh
tclhzqpcvbshxmgx
cfqkptjrulxiabgo
iraiysmwcpmtklhf
znwjlzodhktjqwlm
lcietjndlbgxzjht
gdkcluwjhtaaprfo
vbksxrfznjzwvmmt
vpfftxjfkeltcojl
thrmzmeplpdespnh
yafopikiqswafsit
xxbqgeblfruklnhs
qiufjijzbcpfdgig
ikksmllfyvhyydmi
sknufchjdvccccta
wpdcrramajdoisxr
grnqkjfxofpwjmji
lkffhxonjskyccoh
npnzshnoaqayhpmb
fqpvaamqbrnatjia
oljkoldhfggkfnfc
ihpralzpqfrijynm
gvaxadkuyzgbjpod
onchdguuhrhhspen
uefjmufwlioenaus
thifdypigyihgnzo
ugqblsonqaxycvkg
yevmbiyrqdqrmlbw
bvpvwrhoyneorcmm
gbyjqzcsheaxnyib
knhsmdjssycvuoqf
nizjxiwdakpfttyh
nwrkbhorhfqqoliz
ynsqwvwuwzqpzzwp
yitscrgexjfclwwh
dhajwxqdbtrfltzz
bmrfylxhthiaozpv
frvatcvgknjhcndw
xlvtdmpvkpcnmhya
pxpemuzuqzjlmtoc
dijdacfteteypkoq
knrcdkrvywagglnf
fviuajtspnvnptia
xvlqzukmwbcjgwho
bazlsjdsjoeuvgoz
nslzmlhosrjarndj
menvuwiuymknunwm
uavfnvyrjeiwqmuu
yrfowuvasupngckz
taevqhlrcohlnwye
skcudnogbncusorn
omtnmkqnqedsajfv
yqmgsqdgsuysqcts
odsnbtyimikkbmdd
vuryaohxdvjllieb
dhaxldeywwsfamlo
opobvtchezqnxpak
pzfnegouvsrfgvro
rzkcgpxdslzrdktu
ksztdtqzxvhuryam
ctnqnhkcooqipgkh
pyqbbvrzdittqbgm
koennvmolejeftij
rvzlreqikqlgyczj
xrnujfoyhonzkdgd
mmsmhkxaiqupfjil
ypjwoemqizddvyfd
qgugcxnbhvgahykj
cviodlsrtimbkgmy
xbfbbechhmrjxhnw
psuipaoucfczfxkp
hdhwcpeuptgqqvim
gsxlruhjeaareilr
vgyqonnljuznyrhk
eewezahlumervpyu
iiolebrxfadtnigy
tdadlrodykrdfscn
ocvdtzjxrhtjurpo
gidljbuvuovkhhrf
qwfcpilbjwzboohd
xzohxonlezuiupbg
vslpbkkqgvgbcbix
pivzqrzfxosbstzn
fyqcfboevcqmbhhs
yqsrneacnlxswojx
heicqpxxyrwcbsjz
yzynmnnoumkmlbeh
bncadbjdvvmczylw
hlnjskgfzbgmigfn
fphpszymugpcykka
zbifcktanxpmufvy
saklpkhoyfeqbguy
nqtqfcfxmpivnjyo
locygrwerxlsvzqm
qqflecydqvlogjme
njklmixvgkzpgppf
ugzkpjwjflaswyma
lriousvkbeftslcy
nsvsauxzfbbotgmh
tblcpuhjyybrlica
hqwshxcilwtmxrsf
xojwroydfeoqupup
tikuzsrogpnohpib
layenyqgxdfggloc
nqsvjvbrpuxkqvmq
ivchgxkdlfjdzxmk
uoghiuosiiwiwdws
twsgsfzyszsfinlc
waixcmadmhtqvcmd
zkgitozgrqehtjkw
xbkmyxkzqyktmpfi
qlyapfmlybmatwxn
ntawlvcpuaebuypf
clhebxqdkcyndyof
nrcxuceywiklpemc
lmurgiminxpapzmq
obalwqlkykzflxou
huvcudpiryefbcye
zlxbddpnyuyapach
gqfwzfislmwzyegy
jhynkjtxedmemlob
hmrnvjodnsfiukex
pstmikjykzyavfef
wuwpnscrwzsyalyt
hksvadripgdgwynm
tvpfthzjleqfxwkh
xpmrxxepkrosnrco
qjkqecsnevlhqsly
jjnrfsxzzwkhnwdm
pehmzrzsjngccale
bsnansnfxduritrr
ejzxkefwmzmbxhlb
pceatehnizeujfrs
jtidrtgxopyeslzl
sytaoidnamfwtqcr
iabjnikomkgmyirr
eitavndozoezojsi
wtsbhaftgrbqfsmm
vvusvrivsmhtfild
qifbtzszfyzsjzyx
ifhhjpaqatpbxzau
etjqdimpyjxiuhty
fvllmbdbsjozxrip
tjtgkadqkdtdlkpi
xnydmjleowezrecn
vhcbhxqalroaryfn
scgvfqsangfbhtay
lbufpduxwvdkwhmb
tshipehzspkhmdoi
gtszsebsulyajcfl
dlrzswhxajcivlgg
kgjruggcikrfrkrw
xxupctxtmryersbn
hljjqfjrubzozxts
giaxjhcwazrenjzs
tyffxtpufpxylpye
jfugdxxyfwkzqmgv
kbgufbosjghahacw
xpbhhssgegmthwxb
npefofiharjypyzk
velxsseyxuhrpycy
sglslryxsiwwqzfw
susohnlpelojhklv
lfnpqfvptqhogdmk
vtcrzetlekguqyle
jlyggqdtamcjiuxn
olxxqfgizjmvigvl
cyypypveppxxxfuq
hewmxtlzfqoqznwd
jzgxxybfeqfyzsmp
xzvvndrhuejnzesx
esiripjpvtqqwjkv
xnhrwhjtactofwrd
knuzpuogbzplofqx
tihycsdwqggxntqk
xkfywvvugkdalehs
cztwdivxagtqjjel
dsaslcagopsbfioy
gmowqtkgrlqjimbl
ctcomvdbiatdvbsd
gujyrnpsssxmqjhz
nygeovliqjfauhjf
mmgmcvnuppkbnonz
bhipnkoxhzcotwel
wkwpgedgxvpltqid
mliajvpdocyzcbot
kqjhsipuibyjuref
zqdczykothbgxwsy
koirtljkuqzxioaz
audpjvhmqzvhzqas
cxyhxlhntyidldfx
iasgocejboxjgtkx
abehujmqotwcufxp
fmlrzqmazajxeedl
knswpkekbacuxfby
yvyalnvrxgstqhxm
sjnrljfrfuyqfwuw
ssaqruwarlvxrqzm
iaxbpeqqzlcwfqjz
uwyxshjutkanvvsc
uxwrlwbblcianvnb
nodtifgrxdojhneh
mloxjfusriktxrms
lkfzrwulbctupggc
gcrjljatfhitcgfj
tkdfxeanwskaivqs
ypyjxqtmitwubbgt
ssxbygzbjsltedjj
zdrsnoorwqfalnha
xlgmissaiqmowppd
azhbwhiopwpguiuo
fydlahgxtekbweet
qtaveuqpifprdoiy
kpubqyepxqleucem
wlqrgqmnupwiuory
rwyocktuqkuhdwxz
abzjfsdevoygctqv
zsofhaqqghncmzuw
lqbjwjqxqbfgdckc
bkhyxjkrqbbunido
yepxfjnnhldidsjb
builayfduxbppafc
wedllowzeuswkuez
gverfowxwtnvgrmo
tpxycfumxdqgntwf
lqzokaoglwnfcolw
yqsksyheyspmcdqt
vufvchcjjcltwddl
saeatqmuvnoacddt
dxjngeydvsjbobjs
ucrcxoakevhsgcep
cajgwjsfxkasbayt
hknzmteafsfemwuv
xxwhxwiinchqqudr
usfenmavvuevevgr
kxcobcwhsgyizjok
vhqnydeboeunnvyk
bgxbwbxypnxvaacw
bwjzdypacwgervgk
rrioqjluawwwnjcr
fiaeyggmgijnasot
xizotjsoqmkvhbzm
uzphtrpxwfnaiidz
kihppzgvgyoncptg
hfbkfrxwejdeuwbz
zgqthtuaqyrxicdy
zitqdjnnwhznftze
jnzlplsrwovxlqsn
bmwrobuhwnwivpca
uuwsvcdnoyovxuhn
nmfvoqgoppoyosaj
hxjkcppaisezygpe
icvnysgixapvtoos
vbvzajjgrmjygkhu
jinptbqkyqredaos
dpmknzhkhleawfvz
ouwwkfhcedsgqqxe
owroouiyptrijzgv
bewnckpmnbrmhfyu
evdqxevdacsbfbjb
catppmrovqavxstn
dqsbjibugjkhgazg
mkcldhjochtnvvne
sblkmhtifwtfnmsx
lynnaujghehmpfpt
vrseaozoheawffoq
ytysdzbpbazorqes
sezawbudymfvziff
vrlfhledogbgxbau
bipdlplesdezbldn
ermaenjunjtbekeo
eyaedubkthdecxjq
gbzurepoojlwucuy
rsiaqiiipjlouecx
beqjhvroixhiemtw
buzlowghhqbcbdwv
ldexambveeosaimo
fpyjzachgrhxcvnx
komgvqejojpnykol
fxebehjoxdujwmfu
jnfgvheocgtvmvkx
qmcclxxgnclkuspx
rsbelzrfdblatmzu
vexzwqjqrsenlrhm
tnfbkclwetommqmh
lzoskleonvmprdri
nnahplxqscvtgfwi
ubqdsflhnmiayzrp
xtiyqxhfyqonqzrn
omdtmjeqhmlfojfr
cnimgkdbxkkcnmkb
tapyijgmxzbmqnks
byacsxavjboovukk
awugnhcrygaoppjq
yxcnwrvhojpuxehg
btjdudofhxmgqbao
nzqlfygiysfuilou
nubwfjdxavunrliq
vqxmmhsbmhlewceh
ygavmcybepzfevrp
kgflmrqsvxprkqgq
iaqyqmcaedscmakk
cvbojnbfmrawxzkh
jjjrprbnlijzatuw
lcsudrrfnnggbrmk
qzgxbiavunawfibc
gnnalgfvefdfdwwg
nokmiitzrigxavsc
etzoxwzxqkkhvais
urxxfacgjccieufi
lqrioqhuvgcotuec
dydbaeyoypsbftra
hhrotenctylggzaf
evctqvzjnozpdxzu
tbpvithmorujxlcp
pllbtcbrtkfpvxcw
fzyxdqilyvqreowv
xdleeddxwvqjfmmt
fcldzthqqpbswoin
sgomzrpjfmvgwlzi
axjyskmtdjbxpwoz
hcvaevqxsmabvswh
lfdlsfcwkwicizfk
isjbwpzdognhoxvm
oqnexibqxlyxpluh
zqfbgodsfzwgcwuf
kvmnwruwsjllbldz
kghazimdyiyhmokj
uiktgpsxpoahofxn
zkdwawxargcmidct
ftbixlyiprshrjup
nofhmbxififwroeg
mcdaqrhplffxrcdt
fbjxnwojcvlawmlb
rizoftvwfdhiwyac
eduogrtyhxfwyars
zoikunqxgjwfqqwr
zxwbbpmvctzezaqh
nghujwyeabwdqnop
vcxamijpoyyksogn
jnckdbuteoqlsdae
jurfqqawafmsiqwv
inepmztrzehfafie
tznzkyvzodbrtscf
xewbavjeppflwscl
ucndzsorexjlnplo
jpxbctscngxgusvu
mfmygcllauzuoaok
oibkuxhjmhxhhzby
zjkslwagmeoisunw
avnnxmopdgvmukuu
jmaargejcwboqhkt
yacmpeosarsrfkrv
iqhgupookcaovwgh
ebjkdnxwtikqzufc
imdhbarytcscbsvb
ifyibukeffkbqvcr
aloighmyvwybtxhx
yszqwrutbkiwkxjg
xyholyzlltjhsuhp
gykhmrwucneoxcrf
badkdgqrpjzbabet
sunaucaucykwtkjj
pumqkglgfdhneero
usgtyuestahlydxq
xmfhflphzeudjsjm
knywgmclisgpootg
mtojnyrnvxtweuzb
uuxufbwfegysabww
vobhwwocqttlbsik
yuydfezeqgqxqmnd
wbqgqkwbibiilhzc
sfdmgxsbuzsawush
ilhbxcfgordyxwvp
ahqoavuysblnqaeg
plwgtvpgotskmsey
ewjcmzkcnautrrmp
tyekgzbznlikcyqj
bqzctiuaxpriuiga
bimvbfjkiupyqiys
mpqtbcxfhwymxncw
htemlptvqhharjgb
mqbsmsruwzzxgcxc
zjyedjwhnvteuaid
pzoelkoidwglpttc
efydnsvlfimvwxhx
gfyhgoeiyjcgfyze
deqtomhwopmzvjlt
casafubtkoopuaju
yylsfarntbucfulg
mgjwsormkjsrrxan
lkkenpupgmjpnqqd
tegweszyohsoluot
lihsfdwxmxvwdxna
rrefrjjxerphejwb
guuazonjoebhymtm
ysofqzmfmyneziki
lmjgaliatcpduoal
qzthcpjwtgahbebr
wvakvephyukmpemm
simxacxxzfoaeddw
aetgqmiqzxbvbviz
jxlmhdmqggevrxes
mmuglnjmuddzgaik
svopsqhtrslgycgc
xnvcsiiqrcjkvecn
kkvumxtvashxcops
bduflsdyeectvcgl
vfrxbwmmytjvqnsj
eeqtdneiyiaiofxw
crtbgknfacjtwkfl
uuutuoxdsxolpbhd
lcrztwzreaswovtn
htorkvnvujmjdqzj
wttzuzvrzlyhfzyf
oraewznfwgdsnhuk
rctlkqqvkwbgrcgk
cfehrsrqhzyiwtmz
kbvxwcumjkhvjpui
xxlocexbmniiakfo
gtknkkzvykmlqghl
kcjuxvkuimhwqrtk
vohekwkuyuoacuww
vorctgughscysyfo
zmjevqplngzswxyq
qhswdrhrijnatkyo
joakcwpfggtitizs
juzlwjijcmtswdtq
icbyaqohpkemhkip
rpdxgpzxncedmvzh
rozkmimbqhbhcddv
wkkypomlvyglpfpf
jcaqyaqvsefwtaya
ghvmtecoxlebdwnf
lqrcyiykkkpkxvqt
eqlarfazchmzotev
vqwndafvmpguggef
dbfxzrdkkrusmdke
cmjpjjgndozcmefj
hbrdcwjuyxapyhlo
mmforetykbosdwce
zynfntqwblbnfqik
sodwujfwlasznaiz
yyvrivjiqnxzqkfp
uldbskmmjbqllpnm
fyhhrmrsukeptynl
hpfjekktvdkgdkzl
bozhkoekcxzeorob
uvpptyfrzkvmtoky
hkhfprmjdpjvfkcb
igxzwktwsqhsivqu
qceomwysgkcylipb
cglateoynluyeqgc
xcsdfkpeguxgvpfh
owjhxlcncdgkqyia
rpbmrpcesiakqpna
lueszxiourxsmezb
zelvsowimzkxliwc
vzxbttoobtvdtkca
pfxvzphzwscqkzsi
edsjorainowytbzu
ipsegdaluoiphmnz
mkhueokfpemywvuw
urxdnumhylpafdlc
ggluurzavsxkvwkl
ctclphidqgteakox
tfobosynxsktajuk
jzrmemhxqmzhllif
eemwekimdfvqslsx
yjkwpzrbanoaajgq
rlxghzanuyeimfhx
hozbgdoorhthlqpv
obkbmflhyanxilnx
xojrippyxjmpzmsz
ukykmbfheixuviue
qivlmdexwucqkres
rmyxxipqkarpjmox
fgaftctbvcvnrror
raawxozucfqvasru
dinpjbdfjfizexdh
gybxubwnnbuyvjcr
qrqitdvyoneqyxcg
jqzcfggayzyoqteo
cikqpvxizpdbmppm
stfpldgyhfmucjjv
slzbcuihmimpduri
aufajwfrsorqqsnl
iylmzraibygmgmqj
lcdyfpcqlktudfmu
pmomzzsdpvgkkliw
zpplirgtscfhbrkj
mvhyerxfiljlotjl
ofkvrorwwhusyxjx
xngzmvcgkqfltjpe
yxfxaqipmysahqqq
sdqafdzgfdjuabup
qcqajmerahcdgxfv
xqimrqtupbapawro
qfvkqwidzzrehsbl
himixxvueksiqfdf
vgtfqpuzxxmhrvvd
adiioqeiejguaost
jnzxuycjxvxehbvm
xedbpxdhphamoodk
jsrioscmwlsfuxrg
mtsynnfxunuohbnf
enamqzfzjunnnkpe
uwcvfecunobyhces
ciygixtgbsccpftq
ewjgcronizkcsfjy
wztjkoipxsikoimv
jrgalyvfelwxforw
imylyalawbqwkrwb
yflwqfnuuvgjsgcj
wkysyzusldlojoue
zopllxnidcffcuau
bscgwxuprxaerskj
zvnvprxxjkhnkkpq
nejwxbhjxxdbenid
chryiccsebdbcnkc
guoeefaeafhlgvxh
nzapxrfrrqhsingx
mkzvquzvqvwsejqs
kozmlmbchydtxeeo
keylygnoqhmfzrfp
srwzoxccndoxylxe
uqjzalppoorosxxo
potmkinyuqxsfdfw
qkkwrhpbhypxhiun
wgfvnogarjmdbxyh
gkidtvepcvxopzuf
atwhvmmdvmewhzty
pybxizvuiwwngqej
zfumwnazxwwxtiry
keboraqttctosemx
vtlzxaqdetbhclib
wjiecykptzexuayl
ejatfnyjjdawepyk
mpcrobansyssvmju
gqukndzganeueabm
ukzscvomorucdnqd
wfydhtbzehgwfazx
mtwqdzlephqvxqmx
dltmlfxbjopefibh
atcfrowdflluqtbi
vowawlophlxaqonw
vblgdjzvwnocdipw
uzerzksmkvnlvlhm
ytjwhpaylohorvxd
siprvfxvnxcdgofz
cbhjupewcyjhvtgs
apqtozaofusmfqli
tmssrtlxfouowqnr
ntutrvwnzzgmokes
zrsgpwdzokztdpis
nrobvmsxtfmrqdhv
kadkaftffaziqdze
yrovbgcyqtlsnoux
modheiwuhntdecqs
gzhjypwddizemnys
gaputpwpcsvzxjho
bgmouxwoajgaozau
oxuapfrjcpyakiwt
kntwbvhuaahdixzj
epqjdjbnkxdnaccx
dspltdvznhypykri
tdrgqmbnagrxdwtt
njfqawzjggmemtbg
chpemsgwpzjpdnkk
fpsrobmbqbmigmwk
flxptsrqaazmprnl
nzdunrxlcbfklshm
miuwljvtkgzdlbnn
xbhjakklmbhsdmdt
xwxhsbnrwnegwcov
pwosflhodjaiexwq
fhgepuluczttfvqh
tldxcacbvxyamvkt
gffxatrjglkcehim
tzotkdrpxkucsdps
wxheftdepysvmzbe
qfooyczdzoewrmku
rvlwikuqdbpjuvoo
bcbrnbtfrdgijtzt
vaxqmvuogsxonlgq
ibsolflngegravgo
txntccjmqakcoorp
vrrbmqaxfbarmlmc
dzspqmttgsuhczto
pikcscjunxlwqtiw
lwzyogwxqitqfqlv
gsgjsuaqejtzglym
feyeqguxbgmcmgpp
gmttebyebdwvprkn
mzuuwbhzdjfdryxu
fganrbnplymqbzjx
cvsrbdcvhtxxdmro
scmgkjlkqukoamyp
fkgrqbyqpqcworqc
hjsrvkdibdjarxxb
sztzziuqroeidcus
pxdfvcpvwaddrzwv
phdqqxleqdjfgfbg
cqfikbgxvjmnfncy""" 
    data = 
      case dataSource of 
        Sample1 -> sample1
        Sample2 -> sample2
        Input -> input 
  in 
    data |> String.split "\n" |> List.map Unchecked

init : () -> (Model, Cmd Msg)
init _ =
  let 
    dataSource = Input
    reports = initReports dataSource
    model = { safeReports = 0
            , reports = reports
            , lastCommandText = "press play to start"
            , dataSource = dataSource
            , useNewRules = False
            , counter = 0
            , debug = "" }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Clear | Solve | ToggleNewRules | UseSample1 | UseSample2 | UseInput

updateClear : Model -> Model
updateClear model = { model | reports = initReports model.dataSource, safeReports = 0 } 

countAsSafe : Report -> Int 
countAsSafe report = 
  case report of 
    Safe _ -> 1 
    Unchecked _ -> 0 
    Unsafe _ -> 0

countSafe : List Report -> Int 
countSafe reports = 
  reports |> List.map countAsSafe |> List.sum 

updateSolve : Model -> Model
updateSolve model = 
  let
    reports = 
        model.reports |> List.map (checkReport model.useNewRules)
    found = countSafe reports 
  in
    { model | reports = reports, safeReports = found }

updateToggleNewRules : Model -> Model
updateToggleNewRules model = 
  let
    useNewRules = not model.useNewRules
  in
    { model | useNewRules = useNewRules, reports = initReports model.dataSource, safeReports = 0 } 

updateDataSource : DataSource -> Model -> Model
updateDataSource dataSource model = 
  { model | dataSource = dataSource, safeReports = 0, reports = initReports dataSource } 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> 
      (updateClear model, Cmd.none)
    Solve -> 
      (updateSolve model, Cmd.none)
    ToggleNewRules -> 
      (updateToggleNewRules model, Cmd.none)
    UseSample1 -> 
      (updateDataSource Sample1 model, Cmd.none)
    UseSample2 -> 
      (updateDataSource Sample2 model, Cmd.none)
    UseInput -> 
      (updateDataSource Input model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

toUncheckedHtmlElement : String -> List (Html Msg) 
toUncheckedHtmlElement str =
  let 
    textElement = Html.text str
  in 
    [ textElement, Html.br [] [] ]

toUnsafeHtmlElement : String -> List (Html Msg) 
toUnsafeHtmlElement str =
  let 
    textElement = Html.text str 
    spanElement = Html.span [ Html.Attributes.style "background-color" "#FAA0A0" ] [ textElement ]
  in 
    [ spanElement, Html.br [] [] ]

toSafeHtmlElement : String -> List (Html Msg)
toSafeHtmlElement str =
  let 
    textElement = Html.text str 
    spanElement = Html.span [ Html.Attributes.style "background-color" "#AFE1AF" ] [ textElement ]
  in 
    [ spanElement, Html.br [] [] ]

toReportHtmlElement : Report -> List (Html Msg)  
toReportHtmlElement report = 
  case report of 
    Unchecked str -> toUncheckedHtmlElement str 
    Unsafe str -> toUnsafeHtmlElement str
    Safe str -> toSafeHtmlElement str

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2015 | Day 5: Doesn't He Have Intern-Elves For This?"
  , body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
  let
    commandsStr = ""
    textFontSize = 
      case model.dataSource of 
        Input -> "24px" 
        Sample1 -> "24px" 
        Sample2 -> "24px" 
    elements = model.reports |> List.concatMap toReportHtmlElement
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
              [ Html.div [] [Html.text "Advent of Code 2015" ]
              , Html.div [] [Html.text "Day 5: Doesn't He Have Intern-Elves For This?" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2015/day/5" ] 
                [ Html.text "https://adventofcode.com/2015/day/5" ]
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
                [ Html.Attributes.type_ "radio", onClick UseSample1, Html.Attributes.checked (model.dataSource == Sample1) ] 
                []
              , Html.label [] [ Html.text "Sample1" ]
              , Html.input 
                [ Html.Attributes.type_ "radio", onClick UseSample2, Html.Attributes.checked (model.dataSource == Sample2) ] 
                []
              , Html.label [] [ Html.text "Sample2" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Solve ] 
                [ Html.text "Solve" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Clear ] 
                [ Html.text "Clear" ] 
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleNewRules, Html.Attributes.checked model.useNewRules ] 
                []
              , Html.label [] [ Html.text " Use new rules" ]
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
                Html.div [] [ Html.text (String.fromInt model.safeReports) ]
              , Html.div [] [ Html.text commandsStr ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" textFontSize
              , Html.Attributes.style "padding" "10px"
              , Html.Attributes.style "width" "200px" ] 
              [ 
                Html.div [] elements
              ] ] ]
