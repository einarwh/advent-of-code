module Main exposing (..)

import Browser 
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Html exposing (text)
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

type Outcome = Win | Loss | Ongoing

type Weapon = 
  Dagger 
  | Shortsword
  | Warhammer
  | Longsword
  | Greataxe

type Armor = 
  Leather
  | Chainmail
  | Splintmail
  | Bandedmail
  | Platemail

type Ring = 
  RingDamage1
  | RingDamage2
  | RingDamage3
  | RingDefense1
  | RingDefense2
  | RingDefense3

weaponList : List (Weapon, ItemStats) 
weaponList = 
    [ (Dagger,     { cost = 8,  damage = 4, armor = 0 })
    , (Shortsword, { cost = 10, damage = 5, armor = 0 })
    , (Warhammer,  { cost = 25, damage = 6, armor = 0 })
    , (Longsword,  { cost = 40, damage = 7, armor = 0 })
    , (Greataxe,   { cost = 74, damage = 8, armor = 0 })
    ]

armorList : List (Armor, ItemStats) 
armorList = 
    [ (Leather,    { cost = 13,  damage = 0, armor = 1 })
    , (Chainmail,  { cost = 31,  damage = 0, armor = 2 })
    , (Splintmail, { cost = 53,  damage = 0, armor = 3 })
    , (Bandedmail, { cost = 75,  damage = 0, armor = 4 })
    , (Platemail,  { cost = 102, damage = 0, armor = 5 })
    ]

ringList : List (Ring, ItemStats) 
ringList = 
    [ (RingDamage1,  { cost = 25,  damage = 1, armor = 0 })
    , (RingDamage2,  { cost = 50,  damage = 2, armor = 0 })
    , (RingDamage3,  { cost = 100, damage = 3, armor = 0 })
    , (RingDefense1, { cost = 20,  damage = 0, armor = 1 })
    , (RingDefense2, { cost = 40,  damage = 0, armor = 2 })
    , (RingDefense3, { cost = 80,  damage = 0, armor = 3 })
    ]

type alias ItemStats = 
  { cost : Int
  , damage : Int 
  , armor : Int }

type alias FighterStats = 
  { hitPoints : Int 
  , damage : Int 
  , armor : Int }

type alias Player = 
  { hitPoints : Int 
  , weapon : Maybe Weapon
  , armor : Maybe Armor
  , rings : List Ring }

type alias Model = 
  { boss : FighterStats 
  , player : Player
  , rounds : List String
  , paused : Bool
  , finished : Bool
  , tickInterval : Float 
  , debug : String }

-- Hit Points: 103
-- Damage: 9
-- Armor: 2

initBoss = 
  { hitPoints = 103
  , damage = 9
  , armor = 2 }

initModel : Model
initModel =
  let 
    boss = initBoss
    player = { hitPoints = 100 
             , weapon = Nothing 
             , armor = Nothing 
             , rings = [] }
    model = { boss = boss
            , player = player
            , rounds = []
            , paused = True
            , finished = False
            , tickInterval = defaultTickInterval
            , debug = "" }
  in 
    model 

init : () -> (Model, Cmd Msg)
init _ =
  (initModel, Cmd.none)

-- UPDATE

type Msg = 
  Tick 
  | Step 
  | Clear 
  | Fight 
  | Heal 
  | FindBestDeal 
  | FindWorstDeal 
  | UseWeapon Weapon 
  | ToggleArmor Armor
  | ToggleRing Ring

fightRound : Model -> Model  
fightRound model = 
  let 
    player = model.player 
    p = getPlayerStats player
    b = model.boss 
    pDmg = max 1 (p.damage - b.armor)
    bDmg = max 1 (b.damage - p.armor)
    bHp = max 0 (b.hitPoints - pDmg)
    pHp = max 0 (p.hitPoints - bDmg)
    line1 = "The player deals " ++ String.fromInt p.damage ++ "-" ++ String.fromInt b.armor ++ " = " ++ String.fromInt pDmg ++ " damage; the boss goes down to " ++ String.fromInt bHp ++ " hit points."
    line2 = "The boss deals " ++ String.fromInt b.damage ++ "-" ++ String.fromInt p.armor ++ " = " ++ String.fromInt bDmg ++ " damage; the player goes down to " ++ String.fromInt pHp ++ " hit points."
  in 
    if bHp == 0 then 
      let 
        rounds = List.append model.rounds [ line1, "The player wins!" ]
        updatedPlayer = player 
        updatedBoss = { b | hitPoints = 0 }
      in 
        { model | player = updatedPlayer, boss = updatedBoss, rounds = rounds }
    else 
      if pHp == 0 then 
        let 
          rounds = List.append model.rounds [ line1, line2, "The boss wins!" ]
          updatedPlayer = { player | hitPoints = 0 } 
          updatedBoss = { b | hitPoints = bHp }
        in 
          { model | player = updatedPlayer, boss = updatedBoss, rounds = rounds }
      else 
        let 
          rounds = List.append model.rounds [ line1, line2 ]
          updatedPlayer = { player | hitPoints = pHp } 
          updatedBoss = { b | hitPoints = bHp }
        in 
          { model | player = updatedPlayer, boss = updatedBoss, rounds = rounds }

selectOne : List a -> List (List a)
selectOne items = 
    items |> List.map (\it -> [it])

selectTwo : List a -> List (List a)
selectTwo items = 
  case items of 
    [] -> []
    [_] -> []
    [a, b] -> [[a, b]]
    it :: rest -> 
      let 
        lst1 = selectOne rest |> List.map (\lst -> it :: lst)
        lst2 = selectTwo rest 
      in 
        List.append lst1 lst2 

weaponPermutations : List Weapon
weaponPermutations = 
  let 
    weapons = weaponList |> List.map Tuple.first
  in 
    weapons

armorPermutations : List (Maybe Armor)
armorPermutations = 
  let 
    armors = armorList |> List.map (Tuple.first >> Just)
  in 
    Nothing :: armors

ringPermutations : List (List Ring)
ringPermutations =
  let 
    rings = ringList |> List.map Tuple.first 
  in 
    [] :: List.append (selectOne rings) (selectTwo rings)

combos : List (Weapon, Maybe Armor, List Ring)
combos = 
  weaponPermutations 
  |> List.concatMap (\w -> armorPermutations |> List.map (\a -> (w, a)))
  |> List.concatMap (\(w, a) -> ringPermutations |> List.map (\rs -> (w, a, rs)))

fightToWin : Model -> Maybe Model 
fightToWin model = 
  if model.boss.hitPoints == 0 then 
    Just model 
  else if model.player.hitPoints == 0 then 
    Nothing 
  else 
    model |> fightRound |> fightToWin 

fightToLose : Model -> Maybe Model 
fightToLose model = 
  if model.boss.hitPoints == 0 then 
    Nothing
  else if model.player.hitPoints == 0 then 
    Just model  
  else 
    model |> fightRound |> fightToLose 

equipPlayer : Player -> (Weapon, Maybe Armor, List Ring) -> Player 
equipPlayer player (w, a, rs) = 
  { player | weapon = Just w, armor = a, rings = rs }

updateFindCheapestWin : Model -> Model 
updateFindCheapestWin model =
  if model.finished then 
    updateFindCheapestWin initModel 
  else 
    let 
      players = combos |> List.map (equipPlayer model.player)
      models = players |> List.map (\p -> { model | player = p })
      best = models |> List.filterMap fightToWin |> List.sortBy (\m -> calculateTotalCost m.player) |> List.head |> Maybe.withDefault model 
      weapon = best.player.weapon |> Maybe.withDefault Dagger
      armor = best.player.armor 
      rings = best.player.rings
      equipped = equipPlayer model.player (weapon, armor, rings)
    in 
      { model | player = equipped } 

updateFindMostExpensiveLoss : Model -> Model 
updateFindMostExpensiveLoss model =  
  if model.finished then 
    updateFindMostExpensiveLoss initModel 
  else 
    let 
      players = combos |> List.map (equipPlayer model.player)
      models = players |> List.map (\p -> { model | player = p })
      worst = models |> List.filterMap fightToLose |> List.sortBy (\m -> calculateTotalCost m.player) |> List.reverse |> List.head |> Maybe.withDefault model 
      weapon = worst.player.weapon |> Maybe.withDefault Dagger
      armor = worst.player.armor 
      rings = worst.player.rings
      equipped = equipPlayer model.player (weapon, armor, rings)
    in 
      { model | player = equipped } 

updateClear : Model -> Model
updateClear model = 
  initModel 

healPlayer : Player -> Player 
healPlayer player = 
  { player | hitPoints = 100 }

updateHeal : Model -> Model
updateHeal model = 
  { model | boss = initBoss, player = healPlayer model.player, finished = False, rounds = [] }

updateFight model = 
  if model.finished then 
    let 
      m = initModel 
    in 
      { m | paused = False }
  else if model.paused then { model | paused = False } 
  else model 

updateTick : Model -> Model 
updateTick model = 
  if model.boss.hitPoints == 0 || model.player.hitPoints == 0 then 
    { model | finished = True, paused = True }  
  else 
    fightRound model 

updateWeapon : Weapon -> Model -> Model
updateWeapon weapon model = 
  let 
    player = model.player 
    p = { player | weapon = Just weapon }
  in 
    { model | player = p }

updateArmor : Armor -> Model -> Model
updateArmor armor model = 
  let 
    player = model.player 
    updated = 
      case player.armor of 
        Just a -> 
          if a == armor then Nothing 
          else Just armor  
        Nothing -> 
          Just armor
    p = { player | armor = updated }
  in 
    { model | player = p }

updateRing : Ring -> Model -> Model
updateRing ring model = 
  let 
    player = model.player 
    rings = player.rings 
    updated = 
      if List.member ring rings then 
        List.filter (\r -> r /= ring) rings 
      else 
        if List.length rings < 2 then ring :: rings else rings
    p = { player | rings = updated }
  in 
    { model | player = p }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> 
      (updateClear model, Cmd.none)
    FindBestDeal -> 
      (updateFindCheapestWin model, Cmd.none)
    FindWorstDeal -> 
      (updateFindMostExpensiveLoss model, Cmd.none)
    Heal -> 
      (updateHeal model, Cmd.none)
    Fight -> 
      (updateFight model, Cmd.none)
    UseWeapon weapon -> 
      (updateWeapon weapon model, Cmd.none)
    ToggleArmor armor -> 
      (updateArmor armor model, Cmd.none)
    ToggleRing ring -> 
      (updateRing ring model, Cmd.none)
    Tick -> 
      (updateTick model, Cmd.none)
    Step -> 
      (updateTick model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused || model.finished then Sub.none else Time.every model.tickInterval (\_ -> Tick)

-- VIEW

weaponRow : Player -> String -> Weapon -> Html Msg 
weaponRow player weaponName weapon = 
  let 
    stats = getWeaponStats weapon
  in 
    Html.tr 
      []
      [ Html.td []
          [ Html.input 
            [ Html.Attributes.type_ "radio", onClick (UseWeapon weapon), Html.Attributes.checked (player.weapon == Just weapon) ] 
            []
          ]
      , Html.td []
          [ Html.label [] [ Html.text weaponName ]
          ]
      , Html.td [ Html.Attributes.style "padding-left" "10px" ] [ Html.text (String.fromInt stats.cost) ] 
      , Html.td [ Html.Attributes.style "padding-left" "10px" ] [ Html.text (String.fromInt stats.damage) ] 
      , Html.td [ Html.Attributes.style "padding-left" "10px" ] [ Html.text (String.fromInt stats.armor) ] 
      ]

weaponsTable : Player -> Html Msg 
weaponsTable player = 
  Html.table 
    []
    [ Html.tr 
        [] 
        [ Html.th [] [] 
        , Html.th [] [ Html.text "Weapon" ] 
        , Html.th [ Html.Attributes.style "padding-left" "10px" ] [ Html.text "Cost" ] 
        , Html.th [ Html.Attributes.style "padding-left" "10px" ] [ Html.text "Damage" ] 
        , Html.th [ Html.Attributes.style "padding-left" "10px" ] [ Html.text "Armor" ] 
        ]
    , weaponRow player "Dagger" Dagger
    , weaponRow player "Shortsword" Shortsword
    , weaponRow player "Warhammer" Warhammer
    , weaponRow player "Longsword" Longsword
    , weaponRow player "Greataxe" Greataxe
    ]

armorRow : Player -> String -> Armor -> Html Msg 
armorRow player armorName armor = 
  let 
    stats = getArmorStats armor
  in 
    Html.tr 
      []
      [ Html.td []
          [ Html.input 
            [ Html.Attributes.type_ "checkbox", onClick (ToggleArmor armor), Html.Attributes.checked (player.armor == Just armor) ] 
            []
          ]
      , Html.td []
          [ Html.label [] [ Html.text armorName ]
          ]
      , Html.td [ Html.Attributes.style "padding-left" "10px" ] [ Html.text (String.fromInt stats.cost) ] 
      , Html.td [ Html.Attributes.style "padding-left" "10px" ] [ Html.text (String.fromInt stats.damage) ] 
      , Html.td [ Html.Attributes.style "padding-left" "10px" ] [ Html.text (String.fromInt stats.armor) ] 
      ]

armorTable : Player -> Html Msg 
armorTable player = 
  Html.table 
    []
    [ Html.tr 
        [] 
        [ Html.th [] [] 
        , Html.th [] [ Html.text "Armor" ] 
        , Html.th [ Html.Attributes.style "padding-left" "10px" ] [ Html.text "Cost" ] 
        , Html.th [ Html.Attributes.style "padding-left" "10px" ] [ Html.text "Damage" ] 
        , Html.th [ Html.Attributes.style "padding-left" "10px" ] [ Html.text "Armor" ] 
        ]
    , armorRow player "Leather" Leather
    , armorRow player "Chainmail" Chainmail
    , armorRow player "Splintmail" Splintmail
    , armorRow player "Bandedmail" Bandedmail
    , armorRow player "Platemail" Platemail
    ]

ringRow : Player -> String -> Ring -> Html Msg 
ringRow player ringName ring = 
  let 
    isChecked = player.rings |> List.member ring 
    stats = getRingStats ring 
  in 
    Html.tr 
      []
      [ Html.td []
          [ Html.input 
            [ Html.Attributes.type_ "checkbox"
            , onClick (ToggleRing ring)
            , Html.Attributes.checked isChecked
            ] 
            []
          ]
      , Html.td []
          [ Html.label [] [ Html.text ringName ]
          ]
      , Html.td [ Html.Attributes.style "padding-left" "10px" ] [ Html.text (String.fromInt stats.cost) ] 
      , Html.td [ Html.Attributes.style "padding-left" "10px" ] [ Html.text (String.fromInt stats.damage) ] 
      , Html.td [ Html.Attributes.style "padding-left" "10px" ] [ Html.text (String.fromInt stats.armor) ] 
      ]

ringTable : Player -> Html Msg 
ringTable player = 
  Html.table 
    []
    [ Html.tr 
        [] 
        [ Html.th [] [] 
        , Html.th [] [ Html.text "Ring" ] 
        , Html.th [ Html.Attributes.style "padding-left" "10px" ] [ Html.text "Cost" ] 
        , Html.th [ Html.Attributes.style "padding-left" "10px" ] [ Html.text "Damage" ] 
        , Html.th [ Html.Attributes.style "padding-left" "10px" ] [ Html.text "Armor" ] 
        ]
    , ringRow player "Damage +1" RingDamage1
    , ringRow player "Damage +2" RingDamage2
    , ringRow player "Damage +3" RingDamage3
    , ringRow player "Defense +1" RingDefense1
    , ringRow player "Defense +2" RingDefense2
    , ringRow player "Defense +3" RingDefense3
    ]

fighterRow : String -> FighterStats -> Html Msg 
fighterRow name stats = 
    Html.tr 
      []
      [ 
        Html.td []
          [ Html.label [] [ Html.text name ] ]
      , Html.td [ Html.Attributes.style "padding-left" "10px" ] [ Html.text (String.fromInt stats.hitPoints) ] 
      , Html.td [ Html.Attributes.style "padding-left" "10px" ] [ Html.text (String.fromInt stats.damage) ] 
      , Html.td [ Html.Attributes.style "padding-left" "10px" ] [ Html.text (String.fromInt stats.armor) ] 
      ]

getAllStats : Player -> List ItemStats
getAllStats player = 
  let 
    weaponStats = player.weapon |> Maybe.map getWeaponStats |> Maybe.withDefault defaultStats
    armorStats = player.armor |> Maybe.map getArmorStats |> Maybe.withDefault defaultStats
    ringStats = player.rings |> List.map getRingStats 
  in 
    weaponStats :: armorStats :: ringStats 

calculateTotalDamage : Player -> Int 
calculateTotalDamage player = 
  player |> getAllStats |> List.map (\s -> s.damage) |> List.sum 

calculateTotalArmor : Player -> Int 
calculateTotalArmor player = 
  player |> getAllStats |> List.map (\s -> s.armor) |> List.sum 

getPlayerStats : Player -> FighterStats
getPlayerStats player = 
  let 
    damage = calculateTotalDamage player 
    armor = calculateTotalArmor player 
  in 
    { hitPoints = player.hitPoints 
    , damage = damage 
    , armor = armor }

fighterTable : Player -> FighterStats -> Html Msg 
fighterTable player bossStats = 
  let 
    playerStats = getPlayerStats player 
  in 
    Html.table 
      []
      [ Html.tr 
          [] 
          [ Html.th [] [ Html.text "Fighter" ] 
          , Html.th [ Html.Attributes.style "padding-left" "10px" ] [ Html.text "Health" ] 
          , Html.th [ Html.Attributes.style "padding-left" "10px" ] [ Html.text "Damage" ] 
          , Html.th [ Html.Attributes.style "padding-left" "10px" ] [ Html.text "Armor" ] 
          ]
      , fighterRow "Player" playerStats
      , fighterRow "Boss" bossStats
      ]

defaultStats = { cost = 0, damage = 0, armor = 0 }

tryGetWeaponStats : Weapon -> Maybe ItemStats 
tryGetWeaponStats weapon = 
  weaponList |> List.filterMap (\(w, s) -> if w == weapon then Just s else Nothing) |> List.head 

tryGetArmorStats : Armor -> Maybe ItemStats 
tryGetArmorStats armor = 
  armorList |> List.filterMap (\(a, s) -> if a == armor then Just s else Nothing) |> List.head 

tryGetRingStats : Ring -> Maybe ItemStats 
tryGetRingStats ring = 
  ringList |> List.filterMap (\(r, s) -> if r == ring then Just s else Nothing) |> List.head 

getWeaponStats : Weapon -> ItemStats 
getWeaponStats weapon = 
  weapon |> tryGetWeaponStats |> Maybe.withDefault defaultStats

getArmorStats : Armor -> ItemStats 
getArmorStats armor = 
  armor |> tryGetArmorStats |> Maybe.withDefault defaultStats

getRingStats : Ring -> ItemStats 
getRingStats ring = 
  ring |> tryGetRingStats |> Maybe.withDefault defaultStats

calculateTotalCost : Player -> Int 
calculateTotalCost player = 
  player |> getAllStats |> List.map (\s -> s.cost) |> List.sum 

toRoundElement : String -> List (Html Msg)
toRoundElement str =
  let 
    textElement = Html.text str 
  in 
    [ textElement, Html.br [] [] ]

view : Model -> Html Msg
view model =
  let
    commandsStr = ""
    totalCost = calculateTotalCost model.player 
    elements = model.rounds |> List.concatMap toRoundElement 
    resultStr = 
      if model.boss.hitPoints == 0 then "Result: WIN"
      else if model.player.hitPoints == 0 then "Result: LOSS"
      else "?"
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
              , Html.div [] [Html.text "Day 21: RPG Simulator 20XX" ] ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding-bottom" "10px" ]
              [ Html.a 
                [ Html.Attributes.href "https://adventofcode.com/2015/day/21" ] 
                [ Html.text "https://adventofcode.com/2015/day/21" ]
            ] ]
      , Html.tr 
          [] 
          [ Html.td [ Html.Attributes.align "center" ] [ weaponsTable model.player ]]
      , Html.tr 
          [] 
          [ Html.td [ Html.Attributes.align "center" ] [ armorTable model.player ]]
      , Html.tr 
          [] 
          [ Html.td [ Html.Attributes.align "center" ] [ ringTable model.player ]]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ] 
              [ 
                Html.div [] [ Html.text "--------------------------------" ]
              ] ]
      , Html.tr 
          [] 
          [ Html.td [ Html.Attributes.align "center" ] [ fighterTable model.player model.boss ]]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center" ] 
              [ 
                Html.div [] [ Html.text "--------------------------------" ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "padding" "10px" ]
              [ Html.button 
                [ Html.Attributes.style "width" "80px"
                , onClick Clear
                , Html.Attributes.title "Reset everything" ] 
                [ Html.text "Reset" ]
              , Html.button 
                [ Html.Attributes.style "width" "80px"
                , onClick Heal
                , Html.Attributes.title "Restore player and boss to original health" ] 
                [ Html.text "Heal" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px"
                , onClick FindBestDeal ] 
                [ Html.text "Best" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick FindWorstDeal ] 
                [ Html.text "Worst" ] 
              , Html.button 
                [ Html.Attributes.style "width" "80px", onClick Fight, Html.Attributes.disabled (not model.paused || model.finished) ] 
                [ Html.text "Fight!" ]
              -- , Html.button 
              --   [ Html.Attributes.style "width" "80px", onClick Step ] 
              --   [ Html.text "Step" ] 
            ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "24px"
              , Html.Attributes.style "padding-top" "10px" ] 
              [ 
                Html.div [] [ Html.text ("Equipment cost: " ++ (String.fromInt totalCost)) ]
              , Html.div [] [ Html.text resultStr ]
              ] ]
      , Html.tr 
          []
          [ Html.td 
              [ Html.Attributes.align "center"
              , Html.Attributes.style "font-family" "Courier New"
              , Html.Attributes.style "font-size" "16px"
              , Html.Attributes.style "padding" "10px" ] 
              [ 
                Html.div [] elements
              ] ] ]
