module Aoc03 exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

defaultDelay : Float
defaultDelay = 10

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

-- MODEL

type Segment = Corrupted String | Instruction String

type ParseState = 
  Start 
  | AfterM 
  | AfterU 
  | AfterL 
  | AfterMulOpen
  | ReadingFirstNumber
  | AfterComma
  | ReadingSecondNumber
  | AfterD
  | AfterO
  | AfterN
  | AfterQuot
  | AfterT
  | AfterCondOpen

type alias Model = 
  { value : Int 
  , state : ParseState
  , unscanned : String
  , scanning : String 
  , junk : String 
  , scanned : List Segment 
  , lastCommandText : String
  , highlightFromPosition : Maybe Int
  , position : Int
  , delay : Float
  , paused : Bool  
  , conditionals : Bool 
  , counter : Int 
  , debug : String }

splitInput : String -> (String, String)
splitInput input = 
  case input |> String.split "\n\n" of 
    a::b::_ -> (a, b)
    _ -> ("", "")

parseStackIdLine : String -> List Int 
parseStackIdLine s = 
  s |> String.words |> List.map (String.toInt) |> List.map (Maybe.withDefault 0)

tryReadCrate : Int -> String -> Maybe String 
tryReadCrate index str = 
  case str |> String.slice index (index + 1) of 
    "" -> Nothing 
    " " -> Nothing 
    s -> Just s
  
init : () -> (Model, Cmd Msg)
init _ =
  let 
    sample1 = """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"""
    sample2 = """xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"""
    input = """}mul(417,770)%why(){/':mul(187,313)<@*select()mul(908,713)who()$mul(156,598)#from()* from()^-mul(233,874)&when()mul(346,616)what()(select(),when()where()when()when()&}mul(814,171)+;how() ?)when()?mul(68,188),[select(336,87)^%when(108,692)#mul(621,712){}when()how(69,755)mul(273,27)+:'?( /what()who()from()mul(135,680)-]{:;mul(7,723)where()@{mul(626,107))how()//when(),~+mul(834,113)&!where()$mul(951,76) /why(767,720)mul(107,588)mul(632,977)mul(55,47))[select()']what()mul(803,299)&%#mul(684,214)]}{-}[mul(806,141),mul(166,98)(select()<what():{^mul(98,724)(]from(),what()#,(mul(71,173)from(192,627)&}~when()/mul(142,594)what()<from(594,583)/[~@mul(48,200)why()]^[%/mul(153,486)~:why(),'/mul(555,221): ]+mul(964,152)]/-;why()mul(928,59)+ who(345,334)where()where()mul(953,755))^from()<from()>[*'mul(619,823)mul(633,178)-how()from()?from()*#what()'mul(806,961)what()!?mul(529,544)?+[@$where()mul(498,66)?where()why()who()when()why()'from(742,433)mul(623,31)what()&{>]when()who()$/don't()-^!)where()select()mul(761,938) }}mul(799,944)]~what()mul(872,175) &]-@[mul(59,669)-:,:[{mul(457,618)&),(-[< / mul(490,407)>who()$do()who()!+^mul(579,603);>mul(365,610)!#<^;mul(802,476)}')select()what()don't()$%]}'*what(661,765)>+mul(506,360)*#&!mul(484,725),#~how()~select()don't()?what()mul(16,825)mul(732,513)##mul(990,859) <*,what()%$*?<mul(294,346)@){from()why();don't();how()select()select(){mul(816,810)[})select()what(476,548)/mul(486,109)%!)mul(842,255)^where()<+$mul(164,109)who()}when()[?*}!;]mul(165,28)],#@+select(){([mul(464,924)(mul(308,463)~from(60,280)!,[mul(238,689)@[who()mul(886,862)[when()&))~-@mul(642,606)^what()what()what()@from()]-mul(510,370))/mul(910,462)?!mul(150,133)mul(94,74)?+who()mul(447,476) select()mul(703,286)!#from()[select()select(172,829)^)/where()mul(34,98)^(when()why()#!+~when(){don't()where()&when()from()&~/&^mul(402,788)mul(207,722)';mul(857,627))#?-')why()$[mul(491,994)how();why():{how()mul(913,768))&*<<$(mul(429,856)%>;what()mul(98,564)mul(312,793)~select()where()}$%<mul(796,950)!who()'<&&*[//mul(105,514)mul(600,382)^who()(mul(402,761):^when()mul(32,732)$,()^/+!,mul(115,421)@'where()select()do()&mul(98,768)^<'~#][{where()<mul(330,387)mul(30,766){who()#from()%&mul(146,838)who()}where();how()+where():mul(593,206)/(>mul(173,805)}what()/$#<why()>#}mul(369,326):why(421,566)#mul(357where(305,979)what()*)*mul(775,55){when()%select()~/what() /mul(332,608)-who(),mul(592,150)~/#&#}]mul(453,71>~'from()~%mul(540,100)$/:}#(@ mul(789,978)%$](#{>]?mul(473,873)when()where() {@]why()mul(606,201)who()how()mul(212,663)^;#:when())#don't()where()what()where()*why()why(836,629)@%mul(95,601) +~%'{;mul*,mul(88,716)^ don't()how()when()?%$}when()]?mul(565,119*> $from()-~?mul(980,419)'}from():what()mul(877,181)why()~-, from();~~(mul(347,867)[mul(259,40)?&^/mul(707,897)!!why(589,359)+*>>?@!mul(657,687)/from()who()%-?>?who()mul(795,56) ;<))},mul(3,638)-)mul(243,985)$?where()[})what()mul(487,217)+~//;:mul(449,151)$#(select()why()$%}mul(201,260)select()([[)what()];mul(773,545)~;mul(859,808)mul(235,354)mul(388,265)mul(479,697)from()when()what()select()!^[from()mul(366,310)'*when()/[![(mul(288,12),!,#how()^}}mul(173,948))~why()/!mul(461,403)& where()mul(268,357)
when(578,754)mul(12,923)# /+who();&'^mul(874,174))from()] mul(294+$$[when()where()+mul(118,975)mul(954,230)(+when()mul(579,726)why()$+]$mul(796,252):@ ^?mul(108,275) $)who()why()^mul(782,400);*@~what()'-mul(712,375)[!who(791,378)mul(163,311-]:@!#[select()%%,mul(375,461)*''from()><},@<mul(571,946)) what()!]#{why()mul(638,465) what()$+ +who()mul(446,329)why()how()-where()who()*mul(217,552)how()where()$-^@]mul(29,459)'mul(931,903)-,]?):mul(685,345)}[>{( ;mul(361,764)@mul(71,571)?%~from()what()[ +mul(384,899)@:]mul(627,870select():@!]/mul(676,247)?$what()%who()&from()from(228,681) mul(974@why()*mul(101,691)^do()[why()how()+&mul(228,575)%$where()&#why()from()mul(601,339)#:>@(from()mul(9,628)~{$],why(412,785)):mul(921,60)?when()$mul(230,472)%where(){?}@{mul(57,287)mul(620,392)who()))how()')}mul(920,110)) how()why()>*)from()'mul(66,955)who()mul(339,162)how()from()#mul(63,609)?+how()<&#where() mul(917,834)(mul(845,751)/mul(803,539)~>&:##?:why()<mul(554,940)from():;where()&who()what(807,73)from()mul(742,258)<why() why(764,736)from()when()who()mul(487,500)(%when()~+/{))[mul(689,792#why()when()>']%<select()-mul(455,937)mul(520,734)select()select()why()@@mul(330,396:,;&?where(511,149))what()<mul(528,808){::why()]['mul(482,123)how()#why()don't():)how()mul(170%+*/ !mul(172,611)select()'^from()?what()<mul(124,39),}>^what():mul(462,669)select(),##;:&(mul(211,530)select()from()when()$mul(470,942){-select()what()> mul(934,811where(),mul(424,644))~-{mul(265,791)where()who())mul(403,892)select():<~-when()who(59,475)^mulselect()^mul(265,57)mul(465,609)mul(286,417)select()mul(208,44)#}:)}(+mul(997,988), mul(701,669)where()-from()mul(90,543)-[mul(609,511)>,%-why()why()(what()from()don't()[@;~mul(18,991){;/!^'}-mul(87,567))mul(571,648)-how()#~mul(191,910)}?<&&#~mul(324,407)$(#mul(354,471)^}^#&',)&mul(441,628)'mul(553,282)select(),select()where(){mul(485,690)what()?*]#when()$:>mul(939,289)'mul(482,608)'mul(14,642) how()<@^when(805,793)/mul(132,58)how()($}?where()mul(420,564)mul(542,105)&what()} !~!'why(){mul(380,891from()~where()]what(570,293){mul(255,57)@who(288,308)what()?[how()/mul(838,593)'#+who()what()mul(796,32)&why()who()when()when()<how()when()mul(399@{* select()/+$why(),:mul(159,407)~#>^'how())?{(mul(821,630)$@mul(706,250)}+,/why(588,457)'>mul(451,548)where()]<mul(967,73):mul(219,179)!@^ when()!where()mul(766,741)(!{from()mul(50,543)<*&%don't()%how()from()mul(227,463)mul(896,276)?>where()&?select()# %mul(194,6)how()how()why()( )/~from()&mul(519,83)&from(),<select()where()when()select()<,mulselect()?!$[from()#select()'<>mul(462,510),>,-?{)how()[mul(840,412)where()!;why()#where()/<)why()mul(452,646)?}?!$+!,/mul(502,96)^~(^mul(278,460)mul(478,959)where()-,select(233,181)why()>-mul(391,238)mul(59,361)when()-why()select();<mul(670,527){;,!^% $mul(424,877)select()+when()/how()mul(359,729)why()mul(725,478)( who(794,427)&><mul(772,630)^$how()?why()$%from()who()mul(58,324) *-{/+mul(588,19),when()#^?;select(983,71)do()>why()+:@who()[from()]mul(490,50;who()((when()!why()mul(667,140)what()mul]*from()!^}why()}>:do()~ {(select():why()*mul(691,254)select()why()mul(911,232)where()]#;'don't() *^when()]{}mul(479,259)select()'!}@ *?mul(673,636)how();#how()%^ mul(428,280) }<how()(]mul(831,938)select(),how(452,321)mul(205,255)/),(#,*$mul(949,410)when())[why()^how()!(mul(430,433)#&]who()where()how()select(60,955)mul(771,119)how(837,50)how(123,251);(%/]mul(665,748/*^;mul(662,364)what()from()[] how()~~<don't()@-@mul(660,87)who()-why(513,576)%,why(),mul(49,163,??(}where(348,259)/;; ;mul(311,508)!;-how():why()<]><mul(552,41)when()@where()(#!,)where()mul(430,426)where()from()mul(570]%;mul(409,897))mul(187,740)
%*mul(895,278)#who()'!when()where()^@((mul(972,446)^how():;where()-?mul(472,202),}}&,?/';&mul(904,975)&)$#)([&&,mul(767,425)where()mul(538,130)'#*mul(31,21)do()@-mul(472,734);)~where()who()-<'from()@mul(639,321)-mul(265,870) )mul(292,879)^>}'<select()when()from()+mul(616,445)):~mul(795,885)what()~;how(277,313)#&select()$%mul(354,350)$:-+@%mul(880,234)how()where()]]where();mul(352,975){,who()mul(614,564)-(what()@#/!mul(598,715)&&>)*+mul(13,237 why()*/:/)mul(128,243)(mul(691,104)-$@mul(622,807)[:mul(489,852))what(55,333)where()#&[how()#mul(502,795)[?mul(820how()@'why()((mul(620,867)mul(507,4)$^- mul(206,143)- mul(10,462)/what()where()what()^what()what()mul(734,952)%how()@mul(730,42)what()]why()when()mul(80,180)how()~?{[<when()!how()@mul(139,626)who()~who()where()$mul(284,441)%select()how()(:mul(453,410)?mul(738,130)who()mul(539,997 ^:[what()};who()'[mul(838,734) -#how();^<,$mul(620,399){,&what()~who()how()mul(212,300)why(232,764)from()mul(990,798)%^what()]select()??)when()mul(692,463)&{how()!who(258,688)select()from()mul(189,213)why()#*(who(222,676)mul(342,33);<]}*?(from()what()mul(766,152)>:where()?+where(),mul(261,682);$>^mul(946,198)*-~{select()?where()]mul(331,301){@()how()don't()from():mul(105,560)when(378,815)+*):%^mul(613,583)where()##~mul(761,592)select()@[$why(),}{({mul(727,677)  /do()#<mul(545,311):?+&mul(273,325){mul(583,887)select();:@,<,$don't()+'/<>(<?mul(462,874)%[}from()mul(453,704)!what()how():/#/mul(370,423)))%what()*' who()who()[mul(849,165)-':[mul(749,554)@mul(552,639)why()~<~&what()][%&mul(107,792)*?who(965,657)%@- %[~mul(177,432)+mul(965,759)[from()why()/?}mul(135,657)*{do();,(:mul(932,676)~where()who()mul(149,734)!^<<{?mul(788,201)mul(427>what()%@$why()%/mul(488,910)who()who();mul(385,653)}mul+ mul(340,374))#(mul(853,934)#&??why()why()mul(764,68)what(){mul(759,904)*;%mul(398who()]}:@'$mul(126,742)how()how()+')@when(){mul(585,684)%$>)<)where()mul(463,855)how(222,976)mul?$#><&$;from()when()when()mul(100,686)!when()what(),>;mul(814,643/@>]>!]mul(835,81)&~select()how()+))+mul(155,781)'%<,;-<from())mul(459,773)'who()when()mul(795,954)&what()&why()who()/)who()where()(mul(837,486)from()how(378,922)why()*?select())&mul(32,919)mul<mul(226,220)mul(302,871) where()@%>@'who()mul(709,919) [#{select()(<,where()mul(269,144)what()'>~where()mulwhy()mul(329,330)&from()$:*from()how())mul(483,838)mul(51,282)-+@>++{mul(717,169)($)~]why()^%how(68,594)mul(51,386)where()from()why(424,17))@$mul(230,225)when()&why()*mul(472,522)^what(602,992) mul(169,570)what()/:who()['why(102,867)<when()/mul(566,632)'what()where(),-~+who()<[mul(830,978)[:what()*'mul(176,984)^$}^how()select():<mul(303,480)'mul(437,81)who()^where()when() /mul(815,435)mul(556,382)do()*?where()$$mul(18,981)$from()from()>#'@ $mul(274,92)what(){from()from()from()who() {-mul(372,694){how()select()>mul(15,627)~'who()[what()from()mul(338,200%?*&'!<)mul(477,26)why()*+what()$who()>how()mul(945,996)why()[do()>;who()>why(289,937){!%who()mul(355,685)!mul(644,668)?/ }^}mul(300 [!how()*?when()why() mul(141,672)+&];$)mul(921,47)), {{]>where()from()select()mul(622,890);@$-mul(195,976)$[:when()where()/mul(887,64))mul(799,568),select() { @*>mul(535,258)?why(567,793)^{'*;]mul(202,214)(%when()^<]$-$mul(951,841)![ ];+how()mul(925,412)$;)%']mul(303,460)$where()(mul(369,207)why()>how()%@when()-~-do():+select()#!/usr/bin/perl>#}:mul(110,876'~%from()!{select()mul(924,516)from(){::[#)!&[mul(316how()!from()>mul(570,999)
'}who()&%;}#mul(541,894),'<mul(330,649)} {~mul(883,287)who()mul(996,667)when()when() (;({]what()mul(424,528) {,*%!%&don't() mul(598,37)^<;+mul(714,376){^~from())[->[mul(35,499);select()where()^$@}?mul(239,128)~mul(750,487);how()what(),what()why()]mul(842,927)'&{;what()mul(231,35)+why())who()from()how()why()select()mul(117,134)[*%;~+@/(?mul(184,337)mul(751,262;;mul(71,403),!]/what():*mul(49,974);%mul(556,780)#&;#>when(809,477))}how()mul(934,699)@{!mul(687,51)(where(319,422)why()how():'!mul(340,635) &where() when()-select())>mul(816,984)*;who(267,941){why()#]mul(999,933+ ]$ mul(706,15)!mul(797,760) <when()select():mul(852,26)when()?;},mul(669,849)~where()~]when()mul(720,824){where()%,];who()+~mul(207,217)where())]mul(759,287)why()mul(854,683){(?:, [%mul(574,390)/]mul(267,829){-)mul(248,749)'&select()%,why()how()^mul(900,109)+'@+> )who()mul(577,413)(*don't()^@,+mul(551,920)(%@mul%* what()mul(107,870)$?when()how()mul(291,96)>>who()-<how()]!}~mul(575,386)from()+how()<<]select();mul(224,575)#when()%when(540,536)why()[mul(725,13)where()+?<what()what()+'mul& why()where()who()mul(210,274)^#;mul(477,592)mul(460,555)mul(196,861)->{]<]'mul(607,186)why()select()mul(239,162) from()how(8,248)+*mul(603,94)*how();];!+mul(937,437)select()[+mul(720,689)@ where()[*<$mul(176,240)mul(488,6)mul(884,356)#what()(#mul(357,216)<?^')who()how()why();&mul(779,226)why()mul(378,223))[&[;+mul(710,666){who()!!mul(783,256)@>$why(981,545)mul(15,621):]when(384,172)%?from()/mul(556,42);/[/when()where()%select()mul(911,748);('mul(634,654)mul(187,910):;>mul(671,55)/>why()*mul(487,182) &mul(823,339^[*when()!~$do()mul(752,615)select()}select();) ;mul(891,463)why()who(749,89)&$who() *}-where()mul(483,646)>}-[/mul(463,928)) #when()*who(818,29)where()(&don't()when()%;mul(913,774)} mul(822,254)<where()when() when(116,628);-{!mul(460,824)],~(~mul(203,913)when()mul(794}*'why()(,;how()mul(427,713)-mul(806,281)/^mul(346,763@:,when(181,333)what()why()mul(932,963)/mul(468,402)@/>@]}-mul(949,6)where()from()when();;when()>@@(mul(631,409)where()select()who()~}mul(492,930)mul(388,777) }']select(441,164)!mul(957{{how()$]/%mul(626,541)~:do();mul(796,50)%^~mul(992,764)what(460,353))[what()how()>@;where()mul(944,226)(/?who()*@what()mul(986,707)!who()<how()$:^;mul(896,957+ }how()+#(&(mul(264,193)select()/<!where()?mul(854,845)$~when()mul(733,679)*select()how();/&%mul(886,182):?where(),#;?'^*mul(941,771)}$mul(109,15):#who()where()!%%what()mul(855,377)> #++:mul(712,362)@)what()what(377,883)who(){who()~why()mul(570,45)-*why()what()!~<#mul(873,695)}{~ :~;'$+mul(526,676)?),/mul(106,52); ;%*how()from(165,744)what()from()do():?+ >;mul(151,167)mul~-!mul(873,682)%;;!~?mul(13,903)[$* }mul(188,728)#})(<<from()mul(718,235)where():/mul(843,662)]*)who()-how()[mul(540,880)) #;!%mul(791,474)}&],#don't() #from()who():from()mul(551,523)?select()select()~what(847,762)%mul(504,271)mul  &mul(150,415)^select()])/mul(808,116)where()where()$}mul(105,45)/mul(991,874)>mul(206,992)mul(112,681)who();from()why()mul(998,665)why(52,375)!mul(146,638){mul(783,242from(907,600))[why()@what()select()^ %*don't()mul(949]what(),,what()-<(%mul(519,474)who()~-)mul(640,389)%where()/~@ <&]why()mul(113,424)when()[mul(234,782;,(;-where()why()how()mul(838,733)why()mul(15,790)mul(239,592)^,select()!^*]mul(537,113)
 )}>;why()#{ {mul(469,192)?#,'what()mul%!{>+&what()mul(392,708)'mul(156,225)?]+mul(436^who()why();@#!why()how()when()&mul(506,544)<mul(457,130)who()how()mul(63,456)]from():!mul(274,828)%*mul(593,728who()mul(796,769)*where()mul(293,169):!] mul(160,717)${who()-&{'mul+?@;mul(476,586)from(800,454) mul(86,970-mul(169,184)*;[when(793,513)/how()?mul(694,81)'$when()where()mul(678,405) mul(5,852);)where()^?>why()when()mul(211,662)what()/-)](from()(]?mul(530,199))}why()> mul(833,595)+?$ (mul(98,477)*/};{why()what(966,350)#'mul(44,78)why()!%]% ~}<mul(415,437)~}^(mul(628,229){from()why()mul(588,301)'[~when()[when()where()where()^ mul(314,376):!mul(665,255)}%<*&when()'mul(798,325)*!)^; mul(468,283)'why()!;(how()@>/mul(376,267)/why()'^&&;do()^{/+~how()mul(174,478);what() %{[+!mul}/<)where()@mul(825,710)[ ]mul(60,655)+mul(538,818)-*mul(917,316)!from()&[)mul ?why()'^mul(32,228)~@-~^^mul(803,610)$((;~when()how()mul(622,388)^from()why()mul(291,294)select()+ mul(61,978)mul(986,197)why(405,935)where() where()#{%mul(383,280)%#]*@mul(266select()<>mul(459,359)*&$who()-&+&from()mul(122,271)#mul(85,195)^>select()where()?&mul(953,467 @,^@}@@[mul(297,412)^mul(845,508)where()who()how()}[?when()~%why()mul(320,30)-mul(348,650)*~;&mul(694,123)#+why()mul(516,744)mul(873,547)-select()<from()how()mul(302,513)<!?+]'what()mul(14,253)[ mul(856,612)%what()mul(3,715))/why()}$!how()>]mul(194,499)how()+what()>mul(542,764)who()%when()~mul(150,709)how()}where()@!{)from(773,172)mul(568,873)!+;mul(730,244)from()^ where()*[where()select()mul(554,163)(when()+$~where()*}@+don't()from()~select()who(421,469)&:)mul(279,687)who()%#^when() -select()}mul(8,389):)}why()do()when(984,806);!@:(&mul(59,374)&who():}:$ :mul(919,44)#)(!']why()select()mul(820,819){why(){*^<mul(252,247)when()~mul(808,286)/where()mul(478,366);mul(243,440)};mul(254,343)@[why()select(){% ;mul(825,962)[-{~~/;~mul(393,829)?&>#mul(439,942)>select()]/;select()/'mul(175,719)select()mul(890,515)+;)</{select()^%>mul(611select()-mul(881,340)><who())what(920,644)mul(184,821)])why(){how()mul(516,56)),->~?mul(984,156)mul(330,398)/;why()^ :who()mul(677,667)what()&*what()what()mul(686,640)):-when(),(+mul(40,933)what(423,54)/&#+when()mul(914,37);how()!*~!when()/mul(456,51)when()when()!+]!what()#what()don't()what(225,210),from()~^[@>mul(370,841)what()from()@~-/mul(67,351)(&}mul(59,921)$--^*}mul(566,572)!<! who()(]% mul(478,50)**+(/why();mulhow()when()#!what()who()}do()&-?select()what()mul(505,64)mul(419,683)<what()}*@%#+why()mul(29,918),why(),>how()+*mul(365,948)/$!@&mul(782,899)}/!}~how()mul(73,229);~what()when()*#&where(751,442)who()mul(487,586)when()mul(258,912)@~{what()#]>when()when()mul(124,236)<mul(971,962){~<+how() what()mul(848,349)where()from()mul(506,498)$what()where()(how()where()!who()+mul(528,48);from()when()'why(424,255)#why()?mul(263,381)?~[)><%(-from()mul(65,560)*when(73,56)mul(675,80)}:^don't()how()mul(32,556){select()%!where(500,153)@<&mul(445,212)from()&'who()who(910,924)}@~}what()mul#)why()(!)<what()do()who()$/mul(447,337)why();,who()!mul(868,238)'from(564,793)&mul(852,446),mul(539,259)]<,mul(906,42)select()what()how() :>who(339,797)what()mul(151,749)how()%#>:select(761,257)]when()]mul(866,572)%@'what()~?where()&mul(544,274)-select())mul(159,997)mul(844,233)from(): +who()%what()select()%]do()%where()mul(226,633)/?]where()(when()?don't()<where()mul(722,443)}why():})'why() ;/mul(568,585):$mul(978,828)
@)^&mul(499,826)(<select()mul(336,729select()who()select()&when(), -}mul(444,956)mul(125,420)&why()>$!~+![@mul(645,560)/who()from(715,999)?-mul(13,127)>#%'{mul(439,817)where(331,588)?+[mul(224,797)mul(464,432)mul(35,360)>when()%who()who()select()$#mul(356,503)#[mul(813,62){-<-where()##mul(241,813):[)mul(338,295)what(384,596)%;^who()mul(462,430)mul(182,412)+*/mul(616,721):;mul(10&&where()[^when(),@;{mul(954,803){])what()mul(23,586)mul(897,119)}]^&<{*mul(986,79)#??,,mul(613,13){from()when()mul(866,149)mul(772,870)(<:&who()*what()mul(515,452)what()why()$who()where()]mul(878,410)*mul(594,420)/}[where()>[+)mul(924,312)<why()*/>(#+select()$mul(310,382)$(<when()<>mul(853,276)#$select()&]</*who(774,634)mul(58,679)mul(202,615)from():how()who();mul(630,661)#-^mul(45,510)(]mul(246,385);}}:(mul(841,971)$(;:[&$^*mul(262,559)[}&<>-:'when()mul(574,694):*mul(548@'why()[[mul(705,329)@why()#what();((where()mul(754,401)$:#*:#+%mul(958,635)@[)/select()@;why()(mul(648,81)!)from() +mul(792,904)'@@^}mul(987,166)>+?mul(282,196),[how()%!{?@+mul(486,579)%+who()<?#$/(-mul(776,694)from()mul(807,233)([>'~why()mul(976,618)when()mul(763,421)<select() ,<who()from()'mul(883,779)why()mul(923,148)&<~mul(994,321)*what(344,718)who()#mul(922,344)mul(130,885)%mul(999,904)mul(863,689)-select()&-@[/>mul(421,343)^!,/,-][@^mul(895,484)*how()(why()/from()mul(527,80)]-*:{%-%mul(568,730)-mul(923,402)(!how()%)?+what()^where()mul(744,416) why()mul(824,613)! who(339,629),mul(474,127)>^#select()select()>{from(955,780)mul(961,300)why();:how()*where(674,394)why()''mul(220,654)->)why())^-mul(352,662)?where()/from()mul(755,837)@::@why()mul(173,195){,#mul(369,738)^$?;how()do()@what()where()?@)select()<mul(551,872)mul(221,320)::,,;select()] mul(402,345)how()!'{<select(){where()when()who()mul(698,4)+who()%#[don't() +why()mul(281,880)mul(112,808),+mul(373,120)mul(855,286)where()(<what()'mul(362,818)@>$>$mul(610,930)mul(692,271)}(^@when()[@mul(825,772)),[$;what(364,840)how()&@mul(514,944)*(#^mul(600,592);why()&mul(360,432)>>$!when())from()why()mul(771,433)/who() $what()^^mul(759,696)mul(794,141)?*what();what()how()/mul(575,123)how()([!;],&mul(690,157){$~why()]why()don't()what()#@))~'[</mul(261,256)when()]&when()@<}@where()mul(778,222)><)!from()what(){[]mul(901,549)'what()}from()why()when()how()<*mul(416,637)mul(518,747)!?when()why()?>don't()]]??]<;*^mul(946,35)what()[where()what()/[**/mul(60,183)/{how()~>>why()*mul(510,950)/%who()%{](,!'mul(871,310))]!mul(824,101)who(666,710))what()why()what()$how()%who()<mul(231,819)who(856,644)$/)mul(845,2),#{<!:}mul(778,246)when()*how(468,977)*select(468,903)'from()}@select()mul(39,562)#*mul(145,454)},]$}mul(864,902)mul(471,420)#:@@:where(575,814);}mul(144,92)'< ~>mul(96,822)how()who()mul(689,448)mul(469,609)!@^mul(977,145)& mul(836,253)?+where(42,895)?who()%mul(390,417)mul(641,894)what(312,406)(?when()how()?$mul(968,385)when()^^;mul(779,772)>/->(,where()mul(921,632):(select()(where(228,976)why():']mul(381,906)""" 

    model = { value = 0
            , state = Start
            , unscanned = input
            , scanning = "" 
            , junk = ""
            , scanned = [] 
            , lastCommandText = "press play to start"
            , highlightFromPosition = Nothing
            , position = 0
            , delay = defaultDelay
            , paused = True
            , conditionals = False
            , counter = 0
            , debug = "" }
  in 
    (model, Cmd.none)

-- UPDATE

type Msg = Tick | Step | TogglePlay | Faster | Slower | ToggleConditionals | Reset 

-- type alias Model = 
  -- { value : Int 
  -- , unscanned : String
  -- , scanning : String 
  -- , state : ParseState
  -- , scanned : List Segment 
  -- , lastCommandText : String
  -- , highlightFromPosition : Maybe Int
  -- , position : Int
  -- , delay : Float
  -- , paused : Bool  
  -- , conditionals : Bool 
  -- , counter : Int 
  -- , debug : String }

executeMultiply : String -> Int 
executeMultiply mul = 
  case String.split "," mul of 
    s1::s2::_ ->
      case String.split "(" s1 of 
        _::a::_ ->  
          let 
            b = String.dropRight 1 s2 
          in 
            case (String.toInt a, String.toInt b) of 
              (Just n, Just m) -> n * m 
              _ -> 0
        _ -> 0
    _ -> 0 

whichState : ParseState -> String
whichState state = 
  case state of 
    Start -> "Start"
    AfterM -> "AfterM"
    AfterU -> "AfterU"
    AfterL -> "AfterL"
    AfterMulOpen -> "AfterMulOpen"
    ReadingFirstNumber -> "ReadingFirstNumber"
    AfterComma -> "AfterComma"
    ReadingSecondNumber -> "ReadingSecondNumber"
    AfterD -> "AfterD"
    AfterO -> "AfterO"
    AfterN -> "AfterN"
    AfterQuot -> "AfterQuot"
    AfterT -> "AfterT"
    AfterCondOpen -> "AfterCondOpen"

updateReset : String -> Model -> Model 
updateReset nxt model = 
  -- Reset, everything up to this point was junk.
  let 
    junk = String.append model.scanning nxt
  in 
    { model | state = Start, scanning = "", junk = junk }

updateModelWith : String -> Model -> Model
updateModelWith nxt model = 
  case model.state of 
    Start -> 
      case nxt of 
        "m" -> 
          -- Start scanning with new state AfterM.
          let 
            scanned = 
              if String.isEmpty model.junk then 
                model.scanned 
              else 
                List.append model.scanned [ Corrupted model.junk ]
          in 
            { model | state = AfterM, scanning = nxt, scanned = scanned, junk = "" }
        "d" ->
          -- Start scanning with new state AfterD.
          if model.conditionals then 
            let 
              scanned = 
                if String.isEmpty model.junk then 
                  model.scanned 
                else 
                  List.append model.scanned [ Corrupted model.junk ]
            in 
              { model | state = AfterD, scanning = nxt, scanned = scanned, junk = "" }
          else 
            -- Keep adding to junk.
            { model | junk = String.append model.junk nxt } 
        _ ->
          -- Keep adding to junk.
          { model | junk = String.append model.junk nxt } 
    AfterM -> 
      case nxt of 
        "u" -> 
         -- Keep scanning with new state AfterU.
          { model | state = AfterU, scanning = String.append model.scanning nxt }
        _ -> 
          updateReset nxt model 
    AfterU ->
      case nxt of 
        "l" -> 
         -- Keep scanning with new state AfterL.
           { model | state = AfterL, scanning = String.append model.scanning nxt }
        _ -> 
          updateReset nxt model 
    AfterL -> 
      case nxt of 
        "(" -> 
          -- Keep scanning with new state AfterMulOpen.
          { model | state = AfterMulOpen, scanning = String.append model.scanning nxt }
        _ -> 
          updateReset nxt model 
    AfterMulOpen -> 
      case String.toInt nxt of 
        Just d -> 
          -- Keep scanning with new state ReadingFirstNumber.
          { model | state = ReadingFirstNumber, scanning = String.append model.scanning nxt }
        Nothing -> 
          updateReset nxt model 
    ReadingFirstNumber -> 
      case nxt of 
        "," -> 
          -- Keep scanning with new state AfterComma.
          { model | state = AfterComma, scanning = String.append model.scanning nxt }
        _ -> 
          case String.toInt nxt of 
            Just d -> 
              -- Keep scanning with same state ReadingFirstNumber.
              { model | state = ReadingFirstNumber, scanning = String.append model.scanning nxt }
            Nothing -> 
              updateReset nxt model 
    AfterComma -> 
      case String.toInt nxt of 
        Just d -> 
          -- Keep scanning with new state ReadingSecondNumber.
          { model | state = ReadingSecondNumber, scanning = String.append model.scanning nxt }
        Nothing -> 
          updateReset nxt model 
    ReadingSecondNumber -> 
      case nxt of 
        ")" -> 
          let
            instr = String.append model.scanning ")"
            multiplication = executeMultiply instr
            value = model.value + executeMultiply instr
            scanned = List.append model.scanned [ Instruction instr ]
          in 
            { model | state = Start, value = value, scanned = scanned, scanning = "", junk = "", lastCommandText = String.fromInt multiplication }
        _ -> 
          case String.toInt nxt of 
            Just d -> 
              -- Keep scanning with same state ReadingSecondNumber.
              { model | state = ReadingSecondNumber, scanning = String.append model.scanning nxt }
            Nothing -> 
              updateReset nxt model 
    AfterD -> 
      case nxt of 
        "o" -> 
          -- Keep scanning with new state AfterO.
          { model | state = AfterO, scanning = String.append model.scanning nxt }
        _ -> 
          updateReset nxt model 
    AfterO -> 
      case nxt of 
        "(" -> 
          -- Keep scanning with new state AfterCondOpen.
          { model | state = AfterCondOpen, scanning = String.append model.scanning nxt }
        "n" -> 
          -- Keep scanning with new state AfterN.
          { model | state = AfterN, scanning = String.append model.scanning nxt }
        _ -> 
          updateReset nxt model 
    AfterN ->
      case nxt of  
        "'" -> 
          -- Keep scanning with new state AfterQuot.
          { model | state = AfterQuot, scanning = String.append model.scanning nxt }
        _ -> 
          updateReset nxt model 
    AfterQuot -> 
      case nxt of  
        "t" -> 
          -- Keep scanning with new state AfterT.
          { model | state = AfterT, scanning = String.append model.scanning nxt }
        _ -> 
          updateReset nxt model 
    AfterT -> 
      case nxt of  
        "(" -> 
          -- Keep scanning with new state AfterCondOpen.
          { model | state = AfterCondOpen, scanning = String.append model.scanning nxt }
        _ -> 
          updateReset nxt model 
    AfterCondOpen -> 
      case nxt of  
        ")" -> 
          let
            instr = String.append model.scanning ")"
            conditionals = instr == "do()"
            scanned = List.append model.scanned [ Instruction instr ]
          in 
            { model | state = Start, conditionals = conditionals, scanned = scanned, scanning = "", junk = "" }
        _ -> 
          updateReset nxt model 

updateModel : Model -> Model
updateModel model = 
  if String.isEmpty model.unscanned then 
    -- TODO: Reset things.
    model 
  else 
    let 
      nxt = String.left 1 model.unscanned
      remaining = String.dropLeft 1 model.unscanned
    in 
      updateModelWith nxt { model | unscanned = remaining } 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset -> 
      init()
    Tick ->
      (updateModel model, Cmd.none)
    Step ->
      (updateModel model, Cmd.none)
    TogglePlay -> 
      let 
        runningText = if model.conditionals then "running (with conditionals)" else "running (without conditionals)"
      in 
        ({model | paused = not model.paused, lastCommandText = if model.paused then runningText else "press play to resume" }, Cmd.none)
    Faster -> 
      ({model | delay = model.delay / 2 }, Cmd.none)
    Slower -> 
      ({model | delay = model.delay * 2 }, Cmd.none)
    ToggleConditionals -> 
      let 
        updatedConditionals = not model.conditionals
        runningText = if updatedConditionals then "running (with conditionals)" else "running (without conditionals)"
        cmdText = if model.paused then model.lastCommandText else runningText 
      in 
        ({model | conditionals = updatedConditionals, lastCommandText = cmdText  }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.paused then Sub.none 
  else Time.every model.delay (\_ -> Tick)

-- VIEW

toInstructionHtmlElement : String -> Html Msg 
toInstructionHtmlElement s =  
  Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Html.text s ]

toCorruptedHtmlElement : String -> Html Msg 
toCorruptedHtmlElement s =  
  Html.text s 

toScannedHtmlElement : Segment -> Html Msg 
toScannedHtmlElement segment = 
  case segment of 
    Corrupted s -> toCorruptedHtmlElement s 
    Instruction s -> toInstructionHtmlElement s

view : Model -> Html Msg
view model =
  let
    -- Need to move something according to position! Update function.
    scannedElements = model.scanned |> List.map toScannedHtmlElement
    junkElements = if String.isEmpty model.junk then [] else [ toCorruptedHtmlElement model.junk ]
    scanningElements = if String.isEmpty model.scanning then [] else [ toInstructionHtmlElement model.scanning ]
    unscannedElements = [ Html.text model.unscanned ]
    textElements = List.concat [ scannedElements, junkElements, scanningElements, unscannedElements ]
    commandsStr = ""
  in 
    Html.table 
      [ Html.Attributes.style "width" "500px"]
      [ Html.tr 
          [] 
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "40px"
              , Html.Attributes.style "padding" "20px"]
              [ Html.div [] [Html.text "Advent of Code 2024" ]
              , Html.div [] [Html.text "Day 3: Mull It Over" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "20px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px", onClick Reset ] 
                [ text "Reset" ]
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
                [ text "Step" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "20px" ]
              [ Html.input 
                [ Html.Attributes.type_ "checkbox", onClick ToggleConditionals, Html.Attributes.checked model.conditionals ] 
                []
              , Html.label [] [ Html.text "Parse conditionals" ]
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "background-color" "white" 
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "12px"
              , Html.Attributes.style "padding" "20px"
              , Html.Attributes.style "width" "200px" ] 
              [ Html.div [] [ Html.text model.lastCommandText ]
              , Html.div [] [ Html.text (String.append "state: " (whichState model.state)) ]
              , Html.div [] [ Html.text (String.append "junk: " model.junk) ]
              , Html.div [] [ Html.text (String.append "scanning: " model.scanning) ]
              , Html.div [] [ Html.text (String.append "sum: " (String.fromInt model.value)) ]
              , Html.div [] [ Html.text "..." ]
              , Html.div [ Html.Attributes.align "left" ] textElements
              , Html.div [] [ Html.text commandsStr ]
              ] ] ]
