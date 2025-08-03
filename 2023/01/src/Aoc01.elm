module Aoc01 exposing (..)

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

type alias Line = 
  { content : String 
  , firstDigit : Int 
  , firstDigitSegment : (Int, Int) 
  , lastDigit : Int 
  , lastDigitSegment : (Int, Int) }

type Data = Solved (List Line) | Unsolved (List String)

type alias Model = 
  { dataSource : DataSource 
  , data : Data 
  , includeLetters : Bool 
  , lastCommandText : String
  , counter : Int 
  , debug : String }

initData : DataSource -> Data
initData dataSource = 
  let 
    sample1 = """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""
    sample2 = """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"""
    input = """rhqrpdxsqhgxzknr2foursnrcfthree
2bmckl
four95qvkvveight5
2tqbxgrrpmxqfglsqjkqthree6nhjvbxpflhr1eightwohr
7two68
nine7twoslseven4sfoursix
fivemnjxbrnsvl3
3qcfxgzsevenone1rv
9four6dk7gvv
nine91threepdcthjkmrthreeeightwonsg
fivetglzqdfthreergnseight2lpphhbd
fourtwohldlr6294
qkvc7pvsv6rvsxlqzpjdjkd1eightthree
onefourm5qpfvdnbs
ql8jbzzjpsdgmrjtngrkfdmcsix6eightsix
8nl2
5lfzqhgeightnine
three313eight2zcjnnmtnh
4three77dgjzfj4five
ntr5sixfourznnljvqdr5one
threedlmnd98nineeighteight7
pzcnnbjjthreefmlf9znfnkdrjs
flbdjkseven7338qxzbcsx6
six43fourthree2
one24hndgmz37mjqqm1
4fiverlgtbr
7hqtlxgngd15qkfl2three
mtcztwo46one
3onefivechmlkgp87clrmmhseven
tvsctqdlns1hhhctpn34cztkqzztpcgtzhgrtt
four8flptk
6zkb
7eight9fivesix75hclgfphhvv
ct53qdjpnkdpxdhvpqqcx8
144six
kbjtmgfrx3mpmjhncfl78nine
7sgnlbdfivecxz
oneqrbbnrdxgbbfl3
8ndmrfggfz1six87
hjkfb8vhrhnlmbhbl59rxplvmgzspfour
3sixjhdn4hckqsnvseven
zmkgmlpfsixxhmv25bqlgm5
three48eighttwo
rgfzfourbmpxzrh6dfjcdkhqhcdkpfpk
bpccbcqmlstwos8threenineeightg8
nbcpd2prckbshrbvsmrmlhxdkq
6flfsxv
84xqeightseven
1xf8five8rgplhdvteightseven9
frtwone34btgz4bxlqbf
three5eight
8xtcsggc
9gtrcffqmxpvccjpsfqsixlcmdfkfgjqc
5svskptp1twothreersjrlnfdbxttzrkkqz
tsdtcfsj5sixfivesxvxxsgxhkhngzl
dgfcrtck2twosxkdq
fivektlxgmeighttwo1
9661ldh
twobqcfnxbgnd68
pbsnjqtqhzfrjjtkninexqjbsvrcnnine983
bfcndjjsqseight81
rgvxddcs82one62
oneeightcgseven7
fivexhvmhjhmzscjpxjgnxpd37
onefourdxsgvbkttj2zcpvnrcbpnbfonesix9
four65mrbfjsix
tgbgnqcdrsixone2hsznmgtgtp
3five3
3vzg7
five94eight1xrbcgnnq7rknvvfnrgv
mjsmnineone7phrfqseven7
njtk1one895
jrpbp7n62zgnrnxlzdvtlt8vqm
135sslqzn
9ninenineeightsix4ljnj1twoner
6nine44mgmvrfhvh4four3
gvqxreight6
sqrfkncdk3
5nine7qkjsdscrvsevenfive
qbxmone9twoninesljzz
qhss364lsscxxdk96six
35bxgcfive8x
eightcmzzdxc853sixsixprnnjknfp
one23pxdgrsbsonegfive8
41three4
eightkzpscshdstmnklzthreeqzkftmng46
2two66
threeeightninefvmbcfjf2xjsixlrpr
2269qhvrrfp
527
2onevb
nine7ninefour7threeqsblhrgxvjtwonelg
fourvjdqxnn84szdc
kbgtfcrr6twotwoeightthreeone
5cjlvqhbfivetwo49
knzgl5cnftwovmffkrgblnjmhhb
6glbpbn7
fiverfourfivefive1three
121
eight3one
gxlvnmkpkksixfour6
k8pmlfgsixninetwo
ldlhsix56one
h1
5vlcktk
139nine9gtgqkddsixeightcgqv
fivesixsixeight3nsnhcqcfoursix
pfdksfpmonejrbbninethreeqsjsfrv8njfdh
nzltgfsbqpbzcrgvpj5
eighthtfive5gzmqxhbcdmseven7
nxfvdgxhxninethreegfqlzqmlc4three6
hfs524zfive
5fourtflone
4threenndghtkh41
54bkrhrt2rrptrvcszrskc97
spfone69nlpgftmslthlqdf
qgtf2pz1seven53nfscpnrf
r4fivehkpthtn
gvd2qshbjjsevenone
47397
gtzqvvsntvqnhrkxqb6twonebt
3sixninefivekjkgpjvtcqcsix
six7ninefourseventwosix7eight
6fivemgdj
dvbbjrbfjsxffnjlhfdqthree51oneighttsp
seven784mxkxr219lf
6ddjspgcmtkqd4752
7xlvdtztq
8sevencjsfivethreefour
8rqzng
ninejttfnfzrffnzzbtrbzhsix9fivesix
jhgtblzxbs9klzzztwoxlpsfbvhcl
9ninetwohnvpkkbzklh8
7threen2zfkphngfkjhvkkz
gqldlljv5
nineskmdqcqxj16
9pgbdlfcmbonesixthreep8s1
4dbjktxcbqh8cvxsnrkdgvjrghcfhx
sevensixsevenhtbhnpcpnqjtpfsix686
5hkvjlrjbgtsevenhrlone7sfrztpcqjpdn
52nine7ninethree
three1251
1ghlmpvmmseven1threeghgfrsch
3sixseven7fsdssixdckfvhzrvq8
jtwoninesevenfour73tspxfxcd
2lxlsninelvnzjvd
nine1zfx
gsmfnbkzqqkczkpgg522
zoneightone9zlxxrtbxzrh73sixthreesix
twotwoztxzhrlbnlhtqmlzph6g4eight2
fkqrmjrhmjjnthdt281sevenfour
twodzpjpvx1eightmmkhxtlrfzsevenrnhthree
one72ptwoljccvmmkscflxqv
9nine9kbmlcvn
3ninefivevgvcssvfqhrfvxc6hhlmcllsxpzp
1four7
vhvlxg5six
sgcdxjqvkrfive3three
sixfivesixvgtrtxs5
4zsevenfour2
2onenqjkjxjxcknc52fivethree8
sixzqtgtdkqm1sksnfgbhtwoseven33seven
4twobq2
twocxxl6htqlcrlsdskmone6
731
5sevendfrgsdjhdnrhvdp11fgpxknxvfvv
pdktwo2eight824tlpmsmflg1
71sxtsjtff
ninesixeight5
eightsix3xzrhclnl
twotwo6tmslnxnfdfbdvffgt668
four54xclbszc
tt6fourzlbcldmffour
fourvnxqfngsz7zkh74fourtwo
twoeightl8five1scfnnbh
one4bjzzckgseven
kcb9five
7five6lgdlmjqblvtfprrfht2
4fivetwopcnlntseventhreenqrdgrjg
twobkbfkmzdlkcjbrfvnl65onesix5three
73eight7
nine52jrlrthreez
six8tmxhjxgjm6six3
635jksvjvndtxbkksznrbnine
zncthreeninefive4seven4pdckvp
one1four
xndfpkzeight5
7msxkdfzvtkrsdtqz
4onejglhdmnjx5
1seven1three2
4nvkv
hd3
9zbgttprszjmpv
fourzcxeightthree6
dkkvxvzzktr5vrztgv1
797cqxvfhtlthdbhvcmrktwo6
four383twoeighttwo
1lnkzjrlzdtfvgckczfour
onefivesix3ncll3hcrpsdmtpxvnine
3fiversgksrncxh46mzvpzv94
4eightmzsgftrxjsix637pdjhsds
gz4tgghpmtseveneighteightsix
9sevenmfourfkfscq
hjmbkzznjflhtd3one
eighttwoone5gfzxmptfivemlkfzpnxr
one4txtzkrtzmqtfbzdgnfouronenkcmqdtdgfnjvhc
kljssprljm5nine
z5
lrmgjltmfive79lhqj9
vdgvlvhptpprbx16fourgph5one3
fivenine9two3
ninemglpslone6
988seven3seven
twonine6zjhhxsdlglsmpmvmvvztbhhxklzponeeight
3ghmsrggblfngvtt
gdftwone1six
sixxtxkgzqvldsixzzqtfq5bvhlxkxrthreeoneeight
3oneseven
4three6one19five
sixfourseventwo2threetwoseven7
1fiverltmfpdkkj3nine5
6mzrdnpnfivebfgbbvtmmj4rqssl
vxjncmpg3twoeightone
fivefiveztghmbgd1
6sevenljbnlsfjgpsix38
threeskseven1rqtfrmkkxkkkkbqrqknsfsrfoursix
jh5one
3145hsxsrthree56
hdxxdz6threenpq68zpqhvrt
qbfxrcfpeight5lqpcfd
seven4hldkmhfbeightonenine
seven9threeeightwocvb
qtdp6grfdn
fourhqp93
three1tmgpxlqjninenine
mtbvfjkmxr941bct
one61one
ninesqtf872six526
ncdfhxmgsjseventhree3xvpq8
1sevenjjnvfjzd1twovpkrsvrdbninestsgzdjfgmfxnc
36sixmxnhfkneightwov
threeeight5sixfive241four
four76nine
oneddcxxggx7tvg
7gstvsclkjsixtwoseveng6four1
vttsixsrqgmkptdsqbnhvdmhmzxcstxtx3eight
5qckzltb5
nine943four
hdgbvjtjkc3onetwo238
q97gghxdgcqdkeight3onethree
sixrsdrdvqlht2sixonexn216
4nxkbsvninenineqjphghzjqnjqfvhnine
21fivejpqsgktsjczkdps79three
onesixst8
5ctxhzsvllbseventwo91zhznxqeightrlblc
1178fzjfsevenseven9
2j5dp
cpxeightwoonethreehgnldljndt8onetwo
thltnonefive8twoeight
cfsthmmztroneqmtwo6
threethreesixcsfvf7
dnsskccxfour3
bdgcdzdvbjninefourhgtwoone1nine
6sixeightfive24
sixcgkfour5
ninekbtkninefsbmjgppfive2
96cgstvtnsix
4two4qqflzdeight4
1pctd7zmpndjhljt
78two
hjfbdjggrkjbxjtlmcnnfngrgmfsevendtqjsmqx2
58eightkpxlqmneight
eightninefoursix44txgslt2vvp
5fivefive1one6qskqbldeight
1six3threenine
5fourqzjhzsgbqkczhvhrvr2two
7seventhreeeighttwo4lmkdgkftftwofour
threeeight21fjfxvrcnfive
vkzgjvdjzdpdxdsfiverbgeight3
878
9zcthreerhvxsp
hninepjjvvzmfns1pjtbtkoneonenine
lbjxxthreehkcbjv7
1682seven
3xhbltveightfivethreeln
vdtktf9threesix544
81sixfournineqshtzqhcp
ninetwomdvdkfm815
6eight7ninetwosszxkvqmjpdcz9
twofive3
ninetwo1vdhjdjd
qpkgszmrndvgn11seventwo
crspcbvbone8h
48one27754seven
5dcfivedrpmdlcbvlbjdthree634
1fivefour21mxfzlkbjft
7btfbb9twokldmpeighteight
98qpzgfourfivefivefive9
eight2sixfmsxhvcshrqhpmxj
threetwosix126
fourfivesix1foursgbzzfnggoneighttbk
2twojnine5ktbjbcgbsdfivefour
njbxmqkn3tdthreeeight58
4zltkpphfiveqmlteightjbklpkmgbhmzt
fourxx3
qkgs3eightgnzvnhcdjzjflfsczdonejqbnine6
3twogqdhbgvqsfour
618
plknlddeightjhpmhfleightsnszjmsixczhzq6nlrk
onesix1sggfxdtxvlnpjllm21
91mfcztmvnqrrxqftwoeight7five
2xdthsq
9vlvsrmdsevensix5sxxn8
jprfqddhp97tbvbtt
jzdpqqrhsmnhtfhbrjdp2
225ln
three18eight99three
rjsbbkts5zftbxzttrl1
1hvvhtbgvm95gtcprjgvxcf4
8sjmrkxxbht
dgcgzff9fivethree9
8sixkgtfhppcvp562
kptdxhmdlfrlfml2
2lkssldqgt
grfkn89ptp36two
sixfhddpztzntwofive3
kxxcmtseven2
1lbdhrzdnd89qphjhxqntxnine
grqgmqzfhvksrxgqkb25rjmbllsvzk
eight4fivefivesix1zkbcmhsql16
dhdpvclbmnine2sixeightchcz
6ninetwonine
onepjonehbzsclh83
4xxzjsqddfxbc29four
zhdrrgdqkx9hns7three
1zqfkdhhninebstlhseven
5zxnn9eight1fourvjqsvpn
one2kdmrrbjhxddxfjqjlxr3one
nqjghpdmssjbv47zdgbm7
bvktwonetwothreefive97skqmlv
nd4onedlvthnsrxfxm8
eight6pcntqp2oneeight79eight
6vjzqcnvksn91khthreesixtwo
kxncx932mmmfqf
93546ctxfbhpgxxnlnx
five52
ninevtxblcsbqfzqdqz921
2lqkgmkpcjhsvtnj
v2sixmjktdbtpqgbkcbhvm
rxhvtwo2sixfive
3one7dffhm9
1twofoureighteightjczpmtfpmxf1tfpm
123
16eightshkfbchsvrrqhjpj
sevenfoursevennvxthree49tworzzxtvghkn
one1qzskdmrc
two86cqnhvpnsfourtwo1one
lhxcbcqbrnqvjdlt3znmcbjlfh6
six8289
three9onetjzqjzdck1jkfive
eight15jrdrjrs5xtdvm
two9fourcnjlvxeightvflxm7eight
pd3nine
746twoshbngs
eight28
pjztfhmvtsixmbbjxfvmpstfpttfive1
fivevnncxmbfbtkdqrnmdpdxsqbzprsx2seven
oneljtwo5fivefivethreefour3
hhzoneightsevenfour5seven9sv
xkhjlkrr6zcnchhmrjh6hngjjzhgq4eightqdpqf
3pxcvxkzxlttpsgpk4six
lpfhfhjshfive96six3
6rkpkhgjjxrkgldqzvhxmkmfivenine1eight
9djnfrqzcldeightoneightkxn
7fourone
threethreefourr1four
2gcmft
37four8
7eightbshnvzbzpxeighteight2three
two48pfkmcrkvtfpnine62
qsix8rfmnine425
dhssxzlonesixsevennine9three
eightzxdpbdjzfx4pseven6
threesxgsncfmthreetwos867
four6lkgjqpvqtvvlkmg
8fiveseven
threevhlkdd3
twofdgdlhmglf2gjrrnhhhfxpkqjxhspclbh
fivecjcgbdct4
3mlnbbcgpnvscmg61csix5
nksxgppxsixsvzxhrqzs9threesix
kghb787jdzpjr68
ninegpbqfq6ninethreeeightsixseven
3eightblxrqctk1llv
llqzqvmr4dcqtv26threethreeseven5
6nhzbxeight971fivezmvkvnspj
gvhvninefiveseventwo1bfnvzdc
tz3lhtpftvlgkr7zxqjczcblqcxnbxbvp8
eightppnmdmnlvcqsmlsvvvhk9onetwo5
48fourthreesevenbjj1eight8
5ghflmtwo
nineninethreecprnvsvg3dhxhk
sevenqmmlpdplcmgjtseight5
sevenksdpnqxfn7vzqg11ninefrrtmncl
162hsvvpjlg76ksbbrxpqnoneightk
2733
vlhzdjxd55
three48nine
6mtkftqtmbf
4three3eight3eight4one3
1glmnzhgzzvfn235four
5seven4hqsjsft9cdfhfccfmbfhteight
bv7eight1skmkdtbx
524blmgfivesixhhpv
57eight79fpmznfjjdckxkjqvk8one
six3ttcnzvqhkltbnrrjm
dpttv5nine4one
three3threejpcpzshhrr31one2njhjb
hlhlclcjnnjfourtgxqsxqtgrprvcslsm2
6seven257eight
8ksixfflvxzvhmfninepgq3five
lncgmjnx14fourrnpfhfmhqkgkr5five
sixsixseven1seventhreegkjfkznkqhlhk
sevenzbcpkjkcjq2
smsdpfourgpxnine8five
fxhdt3deight
7htrrjvtworninebtgkdnbt2five
qphmdxgzfiveonemxkx3
1drgktkjcq7mnnxnsixkfhkrf
two7c
43eighttdcnpqldvd
4onevnfd
fltqvxksjrn7
six4p5nvfsjdxbfszpxmttcqgqghcczmlz
8twojseven4
7mcgkthree4nvqqnjkzcqjt
nine5onepjmghlc3one7sixone
4hdone2onezrpnt6
qcdttnqdtdt6
tkrztgn1four4eight66
djvbrczzqj9nine7vfcctvjqdnmsixkeight
44htbnmgvxtznzlzfhtwo93
3xpmx4
6sixnine
5hqqspvrzrxqr2eight
ninemgkhcdtfpz2
3threetwofive4
6njcmnlkrcf
gnfxzcxhvbxpc4
pd41seven
91jvrhxgzrzq4fxcqsixdtnnr
fivehspkhfour4threefourseven
8fiveninegcgkpkone4
6jrk
4one7sevennrtrdltwo
thhsjklkq81eightthreeseven
ltzdlltggeight5eightzrbvrh
sixninem48onespjtjrszk
7hrnvkdslhb
76sixvfcdjhgqpccsksfour2hfjccbplzthree
437ndgtxtwo
5xv5eight
npsmnhzghsrpkrm7
9four94
sixpcgj2nine1frhfn8
51
sdq8cgphzvqffsljtxlxlc5txqnsfdrmn
ninelxdnzqlsevenj8sixtpxrpfive
7hcczrfthqkthr39six
fivesixdkqdnfq9
5n9threertlbqxtj
5threeseven
mqdptrrhxz9threefour
two2two6
vlkzqbztvnbprdc94one4one9
xxcfhgkbss94four3nineseven
jlnhmvgczmninefives8
eightoneeightjsixfhkxphlj57
vpg3fourtwo1
lnineone31jsmeight
45srsljgdfivezvrdmntmb
three6onelrnmnmqpcn4
twozcjvvcdv7xdql993
71fourtwonejpm
one4one56bnhf
twohssdrcsklone8six
7tcjkcchbpggpk1rddpdgvpgzm5
7eight7fmqpzrjlcctjvhrdcjgm2mgkqrbdcmzlzngbkqlj1
fvb4four
rvqhqhb7kdsrmcc
pdklstjsevenfive6
gsglqtpj5lmbcfxchmjnbvhpcvbssx
hgjlmklngdk4zpkfbhmzcmzcjsmfkghdttfb
586ninesixsixvfbveight
hrvthreeninesix58
pxmg4threevhrrsbfrck654
1p5eightonesixhbjbxgjb
four6eight2sevenbtgfkkzcm8lxstnvfbz
c4bqc
12mmrlfnsxtpfncc3
five7eightkvdghfvcrgsevenfivenngqmzh4
5onefourtwopgnhsfrpfourtwonineqfhrz
sixzpzk1
five45
gjfpvjvqhpfrrjpddqfbvdpjj8twoqfvkfkvpqzcjvrqn
czqxtqbxpjt9fourqdcdcfourseven2six
8gmgfsktkxmfourqfshsqvqjx
qjdfggfhtfnine38sevenrvslgljqrpeight
7five6pxhrxcvzsixmcqvjp31nine
drvdnptcltnmxxtjvslq98
fiveqttmkpkrxzngddzsncjlsevenone6
crlldnmx9rpx3
8tqcfour2vbznhzzzl
9vdtxfbgrqzsix16
7sixrjpcfdv9twothree9dqjlgkbmb
eightfourvzxckjqbm6tdbthhsnjr
9bnqfmkxlkj4
62tskjsixtwonine
foureight6gqkdstfivef
seventhreesevencqj12
rqfq6rtcvjjqnmk9gvvttbpnbgvjcmmph8
5zngdplj93seventqlknninesix
fourgnqdcrgsznk8xknsrmd8qpxxm
vgslbcnlpk74dfskqgsp
qmbnjllvvs1fbjslc4onegthree
z82sdgvgktzfive
fourdmxcqmstvj7jgzjkfxjntt5cxhchtglmfive9
npnfivep2three1tlcctmfp
mptzvjmfourfive4three
onelkcmzninefbmgnfghsix3cgpzd
jbshqrc5nbjhktdqmtwotwoxpffrlzxbsix3
9threefive1threenclhfgrzxl9mnlmcckvnd
4231
1366
one9blcprdq
two9two2tdmhtcq13gspmmkrb
eighteightfiveeightjthjbgjm5hpqtbtrfour7
jsjninevmtdscxftwolzfqpbx2gdnflhpvlvtt
fivegjxkbzdtj6four5
3oneone
8gkmzrfhvchfs78jxbtbvgj4
5two89fzrvnkmkgt
53four884seven
five5hbxqmfccfivefourfourfour3
onepbsevenbsgvmgf78875
4fivempxjjpjdgzfkqhqdrrz
7vqj
twovnnknngstnine8
four7twofourqddvxkzvvgzrrvlthreekflqh
vcbmfspdq1jone2two1
eightsixjgfjrsszgmvbdnbqrpzpps6
6eightwomm
hgjjltvnsd39nbxdgfpgjl
jfour16xmrdzq
1jgrkhmjbeight
lhsdll94kccmdhtwozxlfkrmpxksixthreetwo
zmtdfour8
9eightqcpmp8threesevenfive
sftwonenrzbrvmqjp6br3
tgqoneighteightsevenonethreesevennpk4qsffx
dkhqbdnchvfiveeighttwo2fourfthgrgl
brfivefive1mlfxznfourfourfive
dlpqtdpgsevenvzkzljds4three
tzcvrf1srrjxlhszsixsix89l
blbnntbjzsixthree44one
rmppdzntgjtdone1shsevenfive
dpfdbjtv8
two19nhtbktfk
4hbkvbrnpxsevensix
jccqhkonetwo6twotlmbfmvbz
4fouronegbx9
6xhfxfnmmoneightp
lnmmkfntxdsghsxzxkf279
xjdg92ninethreepglp221
seventhree4xmzxp21bdssnzqnrthreefive
196
rgd4
vlmfjrrmpd2two7sevenqrzcrhhvgpvksqh85
71sixfour
sixnine5jfconesevenfournine
5fourfour96hqphnjrmfhsix
nshzgrhveight8oneightrhg
two5c9
dgkvzfzd1
888ninedvdslkfkf
fxmd1qfjnpbhrkhptrp
lnmh48sevenfiveclptstdd5
zgjbxpgpnlseven6ffjcjjfck
seven2rmcdrsix
jggjjpj4eightczcngxm3nineone
tworrgbm31mbhkfhndvb
szkfqfbknv54fiveninesixsevenseven
three3six27
rf376
7clbr1999
xlmfbvseven71nhddnq
nmpgsvzq2jkfoureight6fourhzhzxkseven
5344two1bkxdtwo
six8dnql4zjd641
4fourmngrflsix2two
2tthglxfourfivenpvrqcsrzkfkcqq
njrfive4
633qdvsmlsv2
seven79oneceightljnkxdsvxrgg
onefourzlrvctmc663
27jznqcz861
sixone6twoeight
9fivethree75ffldxglvtone
68onesxzcksgm6
rpphzspvq8zznrthcdsmvmfbgninefour6six
5drvndprrvh178
5threeone3eightthreedmtkpjtmq
two9fzfxseveneightthree
6xbqvhlbeightsgsjbtqcg
seven9twofiveseven
sixtwo87pnltrpkndfttxthpkhjninetxqtcdfxkg
fivethree2five4688seven
2fqmcqdbdzfzjn8sixfournrclzsseven6
9sevensevenxbpljmfsevenone
five98seven3
threexpnxsveightone5nqqcd
27eight8
eight1six2one2htb
twoeight4bhlvvksqqgqone2eight
lbroneightthree3ninefournine
4ctwo7
fhjpvtwo981foureight
pjffszkdkdg3sevencbh4twothreenine
12vljtwo
bldmronetwo33four
jjmcmjmf9fbffcpnp9ptxfptz6vjmbrx
snine9kxkxpkk2two
4eightfive1ftqzsfqgmdcnmrdpthreefivevsdfjt
lfjnvmm9
ninethpqntwothreeeight4
8qcplzvqzrlzlznjlvltcbslthree
eighttgqrvm5twoksr37nine
2fourprchr53three
qnmkxnhm5ninetwotwoksbnvtqs
fourdngcgnnfiveonethree8four9
two3kzmrhsdqtfivefourns48eight
grclfxj4nblzccrfrcctwo83
llvcskkmhzrbzvfd1five
8sixnine3
16kpsgmcffhdmrqpnmthree
1qkkpbptccqg5fivethree99
four5zslbrzjfvcl9trcnkfdkvrthree
kv2
5pxtjhltvxmczx25dtktone
rsjxvln27cvkk
five88three2fourbhmpcthreehxk
one8six
sevensix5sixlqthreenzsphnxbjtksk
lmq8fivefiveseven8fvt
lrshhmlgcxdtq87
fivexmgsixsix282
4sczfdpsix54four
2vxonehqfcvdvxrr
9seven7five
27xkkmp5qone3four
jdjjdjr6lgpgszz
two87sevensixtwofive7qktfvch
86fiveninetwo37
zjxlcjthree3ninefive76fiveeight
9rczvmthplvpxhxt1vgf2fzgg
4nineplxfourtwoseven1lzh6
twothreelmvmhkndcrlvggsqfq2xzksfhgsix1
njfknspt14hfpsxdpninerdzklfg8
qsbeightwolczzd9vblksgxrxdseven9two
8fourseven8five6cnfrcvpnhvttq
nlstwonevgqvzg3fdllsqktqf7cgsvfjhzmfffntvkln
glvmzeightonesix85xrfvt2
5cmlnbrtwo
one6mp82twoeightfour3
six11nqvlrrpxhr
hj95dtrpdkgsevencccfxqlxfour
7frxlq4two4seven6
four6cpkgghfxgfour9fivefour
lsix1sixthreeeight1cxnnqqhxxk
zkntsdftthreejk16
nine2c
9czbtsl
5jqxkskkkzdvznine
7two9
lfssrfourv363vtbjnxj
9fourdpcrxtxnvklltlnsheighttwoqzhnrgheight
twotwo2jh4431nine
57nine19xj11
8sevencone5seven
xz4sixtnnklcvjfdq
15nine
kqtsqznbctwo7vhzfsf987six
6fournine6rsbdsp2qrrnnvqreight2
4fr
hpfllcmjjbbtbfive2sevenmgfvrgbhxmjbxrh
8nrsix
mxrgrhtsfvtzxqlt8vspfive9fourgpkpnhsj7
f6892fourzjgk7three
xzxnlvgbgninenine49two
eightzjtdkvnmnvone2fzhkqzzthree6
six259mtjninerfourkmlg
sixthreepflrvthree5dsrppdzgb
one882three
mtmb8ninenpxtfkchcztwompfcjkmnh
128sevenbeightsixphngd
qx2
1flktj7
six4nine76kcc68one
48lvjfsfmgnine
fmkmvmrrcgeight32three535dl
one7one
5jlscrmvnvtwonine
3ls
eight86
xphtwonefourspczfive9seven6
four9g
tmjtfgrddj9seveneightsixninekzqtwo7
sxvgv3
qvrsvlfvpg99
22qztqxlbjtznfdgcngprx3eight
2fiveseven
76jbmlnine1fourtwohsptzfour
p6ffvh6vzqdglghnfgpdd
sixone9
five6grzlghcztrtjqpncr3lpjstpmcvfvxpnthree
8zkgnine
onedlnmzkzh46sixsix
hcmqknkmzrfnchs5vrjbdqvshvtwonev
twoqnzpmzbgxm6
89nmgdvvzgmvsfxpbc155six7
dhqnjmtzh6bhjnrktjh
one4sdcnk
qkrsblxng61fzqfcjrjnfndzg
8fsklvmcsixgdqvkgcqd
onefiveeightpt2
395lqvmp1
7htxhxzxkonesixdlpvnfbcrrgmx4lzpnjxj
xdf8seven3223
cmsfivebtn9vtwojmcsix
92mntfpfseven
2sixdsdlgvrzbseightqzmdghdvc6
fourgkncjh8onethreemzpnpx
five9pfmbbffcfc9threepdkjxtone
2lrpmpfzceightzrglddshdfour
xhgxmvclgvone2
dlcrxnsix9onesixszqbqbxqfgjgsgsix
939one4ppp
5onessszzgf9hpzmpjctql
9ninesix79
szbrmlbzxp8cqsgkqnz2four
vbddlztfkqzb8fivethreeone9
7four5
2zccllppptfour6
2twotwosevennine6fbccsix
7txcxone4onercgzkskvhlfffive
six4ninexzlnfivekgqggbmkcmqeight
27fourone
12ljvnmlzbgfkccmqmgkrlccfgk
twovgvzxzcfxjdrxbj96bbxvnnnine
363
six1vkcbcpmxtvlksxxjz2btzscsct
5ninetdrhxk6qgfive7
srlvrshx8one
2oneightg
pvflfr73eighteightprthree
nmeightwo6
3three858rvdfxvrmpxhn
five8fourtwonineseventnjgmnrjdj
two9ffcstwo5pq9nine3
79onecscxpbrfoursix5five
fivemlfbzxczbsixkjgrxjxfourtwo6
nmtqf7onescmbq
jflnldgfknsxhmmvmtvmgprsbrdglmpxsdn5
hnb4lxnineeightoneeight
ttshkjxgsdqzjvsvq2fivertgmqplkone
bvknfivethree4ninemztfvcszdhdghgc
91sbfbpxhkkndhnhdjsevenonehxglqqfxdkjp3
qzbqpxvtwo5eight
frcgeight5onefivesix78xltkhzx
248sbtpeighttwozncmmp1vtmndj
mczxcone918ninetwo1two
nvhmdc1prjgfknkc5five
7seven9mdbcjbgx
5fivephqnc
seven1gthg
9xbzmrth
xxhzgnmtv891two2npv
eight171
9rnjvzmsix3rkjbmdv
93oneightxtk
mkmbfour8
twoxbnonefiverx6mjkkfktdxx
ninehrvrcpxfxbzvrtwo1zhbzone
three8tps19mqcmchone
m466zcczxcghqzqlshhzhbkxkm6
vroneight4eight9seven4two46
seven5sevensixtwoseveneighttwo
snxjjttqxkcs6jvlb
5six8
pcqpzp6six51
six2twothree4zjgcsgmjhjsix
tcks16
4jgprfrphdhp52nssrhkfgzhzbvddbtctk9xv
fivesix8one
6onermrhxdqbbcdh
jst7ldbfkdkzkvzpqbtxvvcfvkzsrf9eight6
nine3three9
krmjjfrsixsixfqp2
7gkdlgbdln9jqvzfjq
eighthfslbhxgcbqbldxn9
1fivesevenj9
9trldgsznqlkthreetdmfbxbzhdcskv6kbfour
556qvdmncnkfccnqnkxlslmv
shc27
knmhrbkpseventhree715vtnrnpmrnb
tsnlnmxg5cnxshnmffive
4fourllsqvbfive254
stfivezzjhltsfsix83nineq
six4five
four13
89qcgfqtsdcmktmctwo2seven9tqlbffgrjg
8five6
4three3
59twosplbcrzmgtdrjmrhmhthreetwo4
dlm538nine
pgsixtdtxsrteightnine9fjcvpnxzgfcjggjgmr
9rksix3seven4twoeight
7bzvgmkr5fivezrhpknpkdfournine
cbsdlrqsixtwofour1twonffm
59ndhbj
gblgkrdtffkcsdthreejbbggninesevenzkrsnm5
kftmd5
fourdnvkx84clrnbpftqj6rkdvt
fournine76bzf59
threevhrkzrvbqkfqn26
one9sevensix32bbpvvvt
8zhnptsl16vkv
vslkvkffs2fdcvtngkgtdvzzgplvs11
twoqmtnnnftnhqb2sixzqzqfmbrqj6
nineoneggqtf1l72
7264eightsrjdmhhlfthmglmvgpthree
eightsixsevenone3
9rlkvhq1threevnqzkpfive4
16three
cksqzvzct63fvbcqtr
xlq4three6vfxn8eightwoq
seven3fbhrdgft23onesixpffgmptzxj
fiveone9fmkgxz6thb2
5vsksrdnrtzqzxkhjxfcsnine
eightfivesix77tnlktpb
threeeight11lhc6fivex
1seven36
ljhbgqdb9cmpjzzct
sevensdnfvp1
bzvfsgdhg7p
8z8fivefivefrlfeighttwo
fivelqkmsjstqn93foursnkqthreetwo
bjbnjkpr83
5sixseven
843lxtfgzrg
19xzqmptgslgfxdlvone8
qvs9ninetwotfivetwofivefour
3two7177gchxqndqb2
pclgmslngmhninegktmlrvmvbrb1five
twosix7fivekcstvdpksn6
one8lgrcj987sixthree5
3fnjpnhxh
ninefive8slfz4
threetwonnxdmnpftn9cjkglxp97
4one5two8
8h672smmqm6nine
qsddxqjbxvjlrmlsfxqjptp8twofourkddx
eightxdlqtltjbccnqkcsgvbcpvrltjfourone8nine
zvhfiveeightpxmhfsixmfjnsrfncl1
891
twospnzhxsix5twopsgxhcpgkdthree8
twosix4vvznbz34xbglgczpcd3
bkhgn2
kggbsfour2kcrfbxcscd9lgmthree2
sixxbzmknzt7fourhmhnmgfivezclrhtgshtwo
8txhbgvfjfhsjfivegxshndv3nine1
jeightwosix46splstbtjtthree
55tmflknjjqmrvjtwo
oneqmsxzx8threenine72
1three8zjdpnclmfive
mvqeight93one1one
qqxvlr5fourthreeeight2
six82seven
seveneightseven5one3
4nine59onefivetwonep
fourghdmclp7
11nineeight
3qx74fxsxdpd
8rlvfhchthree68
hbtwone2vgxlzdfsthreesixdxmxrhcone
ssgfcpxgmtwoeightzmtqlhqfive15
sixtwonpgvtn9ninemq
nine3one524
smleightwoccnpgzqxg2qzvzmnineseven1sshrbzzqmt9
vzbbxr9fivetwolkzqp
642fivepkkpljsmtwo
2qhvjljhxlxlg8plckhqhpbc
fivesix8xqzfhrkfvqr1eight
eight2onekmfjqzhgthree
sixfiveeight7
fournine6hhxtwotwo
bncgbbfrfktzrhjkfltn9fourtwonercp
twosixtwojqtcnznfoureight5
kphpbmlgvxhcqctksjvkrcfsbthree9gfhtkbs
eightvfkbm3pvtwotworjqxrx8
2kgfcrvtfive1sgk2nbz8six
6zgdkmzfr44
x7qvmtdnhclnn68foursdkdcvrfrfour
lsrbdckq1
one7xzmhknhgbfks
hnmkmltwonine729hdf1
j2fivehttgmfhgzstrcdnvdspxzcxkt
7fourvhrhqtconefive
sevensixfour2ccprddhlxdvxtzshnbqtgmp38
9twolpshzrpgqonercvlqjglchfour
rcbqcxzlgjlprbcqmjrfourldkgxhsnlb74
sixtwo6mdrfflbkrone
rxcsh1
mcnine4sixrloneightspv
qkdpnztwoxjcrfnsmhj4eight1gzzzgkcznine
mkglfvb2vmvrnrfourklnj76onethree
nrqxpfxcgg2fiveceight6
6three4xsknpdhgchq6hdgblxszbpnnt
xvmnsdvqfrgkb1pmqtjfgdsix
78threexvllvmdgjpck4
threermvn16nineone5seven
sixjkxjmfl4
fdvhjb16gvpz
9hnqsix2eight5fiveeight
szxzqzkhbf9lgxfzvnsxjfhhgc
qxlbhrtjlkvsixbhvqdvd91
6sixfour3
6two2c528ldpscl9
6rgceight7five931
2gkprbninefgn
9vbzhldtxsgmcbst
dbmqtfkst4eight76
2sevenoneqkhrxdhbkhcbxbhnjxzsevennine
foureight2nine
five9ninethreeseven
one8threefourthreexrrhnpdqcf
mgvq6vrsprdcvjs
vronedb876four53
2cpv4tbk
jbvrn2
81blgc8five295
4threefour7eighteightdcmrckqxqmnpf
nxcbtwo5four66
5seven3
hlcrfjjkjqrvsevenbtdkvzqvxgrjdcmhggcqrr7fiveeight
dcjszvn3phczz34qdgfive
fivefourgrkthree2three1
three28nine8
eightt4
sixfour8
cptkxpzxk138five1sevenone
gsxjbkkg3foursix78fnkvhsddqvxs6
htkvspn4fourfourtwo
52rlnsspjhrgfnckg
9six27sdgmz
g6ninezrnine
four6eightnjzpmninetkfxqgcqnb
2five8ffgdtrhzqntjzjssckdggqcvbskpdsrclv
3sixninecxghbrnpcx3vj3
9hpm4qtdjpdnbqknine4
9twosevenlnjfz
5prpkmbkfq6
bsltmrpklzhrldlvpj1eightkbnxnfrlc71three
z8
2sevenlzxone
9l5pxeight6
ninexhskkhdkgjgvjhrqhrfj9bnrfbtxpp
sixsixsix3twosevenfivekjdkpxch
jkpvm1567seven
dvjdfnqgbsixeightsixqdkfpbc3lcjz
7twonineninetwo
zjtsjvhfldplt33qpgnlflhj
eight9mkqpbddonejtc78eight6
797ninetwotwo
dfjnzxtlnine9five
gmjknnzrnrpcbcngzqgseven8
vqplmsqninenbsjvctjfk8one29zrqb
3ljdlfldqtlqxrmxone4
qbsixfour6six89pqxspnr8
6vxhzrmcbvthree
four563pllcfonebvnbltn
dxbtmg2three21
xtfzzr73
fivessix4
sixone485pvzxbd
18cfour
xjtctnllkp57seven2jgbjmjbxnpfgone
eight5gmbzbqtxrr27dtgfdbmtc7
twoggvcnfmtrseven4dx
ssevenhcltwoseven2cxrmxxcr""" 
    data = 
      case dataSource of 
        Sample1 -> sample1
        Sample2 -> sample2
        Input -> input 
  in 
    data |> String.split "\n" |> Unsolved

init : () -> (Model, Cmd Msg)
init _ =
  let 
    dataSource = Input
    data = initData dataSource
    model = { data = data
            , lastCommandText = "press play to start"
            , dataSource = dataSource
            , includeLetters = False
            , counter = 0
            , debug = "" }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Clear | Solve | ToggleIncludeLetters | UseSample1 | UseSample2 | UseInput

tryParseInt : Char -> Maybe Int 
tryParseInt ch = 
  ch |> String.fromChar |> String.toInt

tryFindNum1 : String -> Maybe (Int, String) 
tryFindNum1 s  = 
  let 
    s0 = String.left 1 s 
  in 
    s0 |> String.toInt |> Maybe.map (\d -> (d, s0))

tryFindNum2 : String -> Maybe (Int, String) 
tryFindNum2 s  = 
  let 
    s0 = String.left 1 s 
  in 
    case String.toInt s0 of 
      Just d -> 
        Just (d, s0) 
      Nothing -> 
        if String.startsWith "one" s then Just (1, "one")
        else if String.startsWith "two" s then Just (2, "two")
        else if String.startsWith "three" s then Just (3, "three")
        else if String.startsWith "four" s then Just (4, "four")
        else if String.startsWith "five" s then Just (5, "five")
        else if String.startsWith "six" s then Just (6, "six")
        else if String.startsWith "seven" s then Just (7, "seven")
        else if String.startsWith "eight" s then Just (8, "eight")
        else if String.startsWith "nine" s then Just (9, "nine")
        else Nothing

substrings : String -> List String 
substrings s = 
  if String.isEmpty s then []
  else s :: substrings (String.dropLeft 1 s)

digitFinder : (String -> Maybe (Int, String)) -> String -> List (Int, (Int, String)) 
digitFinder parser str = 
  let 
    subs = substrings str
    pairs = subs |> List.indexedMap Tuple.pair 
  in 
    pairs |> List.filterMap (\(ix, s) -> parser s |> Maybe.map (\(d, ds) -> (ix, (d, ds))))

parseLine : (String -> Maybe (Int, String)) -> String -> Line
parseLine parser s = 
  let 
    digits = digitFinder parser s 
    (firstIndex, (firstDigit, firstDigitStr)) = digits |> List.head |> Maybe.withDefault (0, (0, "0")) 
    (lastIndex, (lastDigit, lastDigitStr)) = digits |> List.reverse |> List.head |> Maybe.withDefault (0, (0, "0"))
    firstDigitSegment = (firstIndex, String.length firstDigitStr)
    lastDigitSegment = (lastIndex, String.length lastDigitStr)
  in  
    { content = s
    , firstDigit = firstDigit
    , firstDigitSegment = firstDigitSegment
    , lastDigit = lastDigit
    , lastDigitSegment = lastDigitSegment
    }

updateClear : Model -> Model
updateClear model = { model | data = initData model.dataSource } 

updateSolve : Model -> Model
updateSolve model =
  case model.data of 
    Unsolved unsolved -> 
      let 
        parser = if model.includeLetters then tryFindNum2 else tryFindNum1 
        data = unsolved |> List.map (parseLine parser) |> Solved 
      in 
        { model | data = data }
    Solved _ -> model 

updateToggleIncludeLetters : Model -> Model
updateToggleIncludeLetters model = 
  let
    includeLetters = not model.includeLetters
  in
    { model | includeLetters = includeLetters, data = initData model.dataSource} 

updateDataSource : DataSource -> Model -> Model
updateDataSource dataSource model = 
  { model | dataSource = dataSource, data = initData dataSource } 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> 
      (updateClear model, Cmd.none)
    Solve -> 
      (updateSolve model, Cmd.none)
    ToggleIncludeLetters -> 
      (updateToggleIncludeLetters model, Cmd.none)
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

toUnsolvedLineHtmlElements : String -> List (Html Msg) 
toUnsolvedLineHtmlElements s =
  let 
    textElement = Html.text s 
  in 
    [ textElement, Html.br [] [] ]

toUnsolvedElements : List String -> List (Html Msg)
toUnsolvedElements strings = 
  strings |> List.concatMap toUnsolvedLineHtmlElements

maybePlainSegment : String -> Maybe (Html Msg)
maybePlainSegment s = 
  if String.isEmpty s then Nothing 
  else 
    Just (Html.text s)

digitSegment : String -> Html Msg
digitSegment s = 
  let 
    textElement = Html.text s 
    spanElement = Html.span [ Html.Attributes.style "font-weight" "bold" ] [ textElement ]
    -- spanElement = Html.span [ Html.Attributes.style "text-decoration-line" "underline" ] [ textElement ]
  in 
    spanElement

break : Html Msg 
break = Html.br [] []

toSolvedLineHtmlElements : Line -> List (Html Msg) 
toSolvedLineHtmlElements line =
  let 
    s = line.content 
    (firstStartIndex, firstLength) = line.firstDigitSegment
    (lastStartIndex, lastLength) = line.lastDigitSegment 
    firstEndIndex = firstStartIndex + firstLength - 1
    lastEndIndex = lastStartIndex + lastLength - 1
    firstSegment = String.left firstStartIndex s 
    lastSegment = String.dropLeft (lastEndIndex + 1) s 
    firstDigitStr = s |> String.slice firstStartIndex (firstEndIndex + 1)
    lastDigitStr = s |> String.slice lastStartIndex (lastEndIndex + 1)
  in 
    if lastStartIndex == firstStartIndex then 
      [ maybePlainSegment firstSegment
      , Just (digitSegment firstDigitStr)
      , maybePlainSegment lastSegment
      , Just break ]
      |> List.filterMap identity
    else if lastStartIndex > firstEndIndex then 
      let 
        midSegment = String.slice (firstEndIndex + 1) lastStartIndex s 
      in 
        [ maybePlainSegment firstSegment
        , Just (digitSegment firstDigitStr)
        , maybePlainSegment midSegment
        , Just (digitSegment lastDigitStr)
        , maybePlainSegment lastSegment
        , Just break ]
        |> List.filterMap identity
    else 
      [ Html.text "it's complicated", break ]

toSolvedElements : List Line -> List (Html Msg)
toSolvedElements strings = 
  strings |> List.concatMap toSolvedLineHtmlElements

view : Model -> Document Msg
view model = 
  { title = "Advent of Code 2023| Day 1: Trebuchet?!"
  , body = [ viewBody model ] }

lineToString : Line -> String 
lineToString line =
  let 
    content = line.content 
    firstDigit = line.firstDigit
    (firstIndex, firstLength) = line.firstDigitSegment
    lastDigit = line.lastDigit
    (lastIndex, lastLength) = line.lastDigitSegment
  in 
    [ "content: " ++ content 
    , "firstDigit: " ++ String.fromInt firstDigit
    , "firstIndex: " ++ String.fromInt firstIndex
    , "firstLength: " ++ String.fromInt firstLength
    , "lastDigit: " ++ String.fromInt lastDigit
    , "lastIndex: " ++ String.fromInt lastIndex
    , "lastLength: " ++ String.fromInt lastLength ] 
    |> String.join "\n"

getLineScore : Line -> Int 
getLineScore line = 
  line.firstDigit * 10 + line.lastDigit 

getTotalScore : List Line -> Int
getTotalScore lines = 
  lines |> List.map getLineScore |> List.sum  

viewBody : Model -> Html Msg
viewBody model =
  let
    commandsStr = "?"
    textFontSize = 
      case model.dataSource of 
        Input -> "16px" 
        Sample1 -> "24px" 
        Sample2 -> "24px" 
    elements = 
      case model.data of 
        Solved solved -> toSolvedElements solved 
        Unsolved unsolved -> toUnsolvedElements unsolved
    scoreStr = 
      case model.data of 
        Solved solved -> getTotalScore solved |> String.fromInt 
        _ -> "?"
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
              [ Html.div [] [Html.text "Advent of Code 2023" ]
              , Html.div [] [Html.text "Day 1: Trebuchet?!" ] ] ]
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
                [ Html.Attributes.href "https://adventofcode.com/2023/day/1" ] 
                [ Html.text "https://adventofcode.com/2023/day/1" ]
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
                [ Html.Attributes.type_ "checkbox", onClick ToggleIncludeLetters, Html.Attributes.checked model.includeLetters ] 
                []
              , Html.label [] [ Html.text " Include letters" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding-top" "10px" ] 
              [ 
                Html.div [] [ Html.text scoreStr ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" textFontSize
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [] elements
              ] ] ]
