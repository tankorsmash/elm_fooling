module Battle exposing (Model, Msg(..), OutMsg(..), init, subscriptions, suite, update, view)

import Array
import Browser.Dom
import Browser.Events
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Color
import Color.Convert as Convert
import DOM exposing (offsetWidth, target)
import Dict
import Element
    exposing
        ( Color
        , Element
        , alignBottom
        , alignLeft
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , height
        , modular
        , padding
        , paddingXY
        , paragraph
        , rgb
        , rgb255
        , row
        , scrollbars
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy as Lazy
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, tuple)
import Html
import Html.Attributes exposing (attribute, classList, href, property, src, style, value)
import Html.Events
import Interface as UI
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Random
import Random.List
import Task
import Test exposing (..)
import Time
import Tuple3
import UUID exposing (UUID)


type Msg
    = Noop
    | Fight
    | FindNewEnemy
    | ToggleShowExpandedLogs
    | HealGolem
    | ReviveGolem
    | SendOutMsg OutMsg


type OutMsg
    = NoOutMsg
    | ReturnToShop
    | DeliverItemToShopOnMonsterDefeat


type alias IntStat =
    { curVal : Int
    , initialVal : Int
    , maxVal : Int
    }


initStat : Int -> IntStat
initStat maxVal =
    { curVal = maxVal
    , initialVal = maxVal
    , maxVal = maxVal
    }


setStatCurVal : Int -> IntStat -> IntStat
setStatCurVal newCurVal stat =
    { stat | curVal = newCurVal }


addToStatMaxVal : Int -> IntStat -> IntStat
addToStatMaxVal addedMaxVal stat =
    { stat | maxVal = stat.maxVal + addedMaxVal }


addToStatCurVal : Int -> IntStat -> IntStat
addToStatCurVal addedCurVal stat =
    { stat | curVal = stat.curVal + addedCurVal }


setStatMaxVal : Int -> IntStat -> IntStat
setStatMaxVal newMaxVal stat =
    { stat | maxVal = newMaxVal }


setStatInitialVal : Int -> IntStat -> IntStat
setStatInitialVal newInitialVal stat =
    { stat | initialVal = newInitialVal }


type alias Monster =
    { name : String
    , statHP : IntStat
    , statStamina : IntStat
    , statPower : IntStat
    , statProtection : IntStat
    , xp : Int
    }


monsterStatMap :
    (Monster -> IntStat)
    -> (Monster -> IntStat -> Monster)
    -> (IntStat -> IntStat)
    -> Monster
    -> Monster
monsterStatMap statGetter statSetter statFunc monster =
    let
        intStat =
            statGetter monster

        newStat =
            statFunc intStat
    in
    statSetter monster newStat


monsterStatMapHP : (IntStat -> IntStat) -> Monster -> Monster
monsterStatMapHP statFunc monster =
    monsterStatMap .statHP setStatHP statFunc monster


monsterStatMapStamina : (IntStat -> IntStat) -> Monster -> Monster
monsterStatMapStamina statFunc monster =
    monsterStatMap .statStamina setStatStamina statFunc monster


monsterStatMapPower : (IntStat -> IntStat) -> Monster -> Monster
monsterStatMapPower statFunc monster =
    monsterStatMap .statPower setStatPower statFunc monster


monsterStatMapProtection : (IntStat -> IntStat) -> Monster -> Monster
monsterStatMapProtection statFunc monster =
    monsterStatMap .statProtection setStatProtection statFunc monster


setStatHP : Monster -> IntStat -> Monster
setStatHP monster newStatHP =
    { monster | statHP = newStatHP }


setStatStamina : Monster -> IntStat -> Monster
setStatStamina monster newStatStamina =
    { monster | statStamina = newStatStamina }


setStatPower : Monster -> IntStat -> Monster
setStatPower monster newStatPower =
    { monster | statPower = newStatPower }


setStatProtection : Monster -> IntStat -> Monster
setStatProtection monster newStatProtection =
    { monster | statProtection = newStatProtection }


setHpTo value monster =
    let
        hpStat =
            monster.hpStat

        newHpStat =
            { hpStat | curVal = value }
    in
    { monster | hpStat = newHpStat }


setStatToMax intStat =
    { intStat | curVal = intStat.maxVal }


type DamagedMonster
    = LivingMonster Monster
    | DeadMonster Monster


type alias MonsterAttackedData =
    { attacker : Monster
    , defender : Monster
    , attackerPower : Int
    , defenderProtection : Int
    , damageTaken : Int
    }


type FightLog
    = MonsterAttackedMonster MonsterAttackedData
    | FoundNewMonster Monster
    | GolemKilledMonster Monster Monster Int
    | MonsterKilledGolem Monster Monster


{-| we're going to have to import Character at some point, for now though this is good enough
-}
type alias BattleCharacter =
    { held_gold : Int
    , held_blood : Int
    }


type alias Model =
    { golem : DamagedMonster
    , enemyMonster : DamagedMonster
    , battleSeed : Random.Seed
    , fightLogs : List FightLog
    , showExpandedLogs : Bool
    , player : BattleCharacter --NOTE: this is hackily read from in ItemShop's updateBattleOutMsg and used to update ItemShop's player. FIXME hack on that for sure
    }


init : { a | held_blood : Int, held_gold : Int } -> Model
init { held_blood, held_gold } =
    { golem = LivingMonster <| createMonster "Golem" 10 10 0
    , enemyMonster =
        createMonster "Slime" 10 2 5
            |> monsterStatMapHP (setStatCurVal 4)
            |> LivingMonster
    , battleSeed = Random.initialSeed 123456
    , fightLogs = []
    , showExpandedLogs = False
    , player = { held_blood = held_blood, held_gold = held_gold }
    }


golemKillsEnemy : Monster -> Monster -> List FightLog -> ( DamagedMonster, DamagedMonster, List FightLog )
golemKillsEnemy golem deadEnemy fightLogs =
    let
        ( victorGolem, gainedXp ) =
            increaseMonsterXpByMonster golem deadEnemy
    in
    ( LivingMonster victorGolem
    , DeadMonster deadEnemy
    , fightLogs ++ [ GolemKilledMonster golem deadEnemy gainedXp ]
    )


enemyKillsGolem : Monster -> Monster -> List FightLog -> ( DamagedMonster, DamagedMonster, List FightLog )
enemyKillsGolem golem enemy existingFightLogs =
    ( DeadMonster golem
    , LivingMonster enemy
    , existingFightLogs ++ [ MonsterKilledGolem golem enemy ]
    )


monsterCounterAttacks : Monster -> Monster -> List FightLog -> ( DamagedMonster, DamagedMonster, List FightLog )
monsterCounterAttacks golem enemy existingFightLogs =
    case monsterFightsMonster enemy golem of
        --enemy killed golem in the counterattack
        ( LivingMonster killingEnemy, DeadMonster deadGolem, secondFightLogs_ ) ->
            enemyKillsGolem deadGolem killingEnemy (existingFightLogs ++ secondFightLogs_)

        ( e, g, secondFightLogs_ ) ->
            ( g, e, existingFightLogs ++ secondFightLogs_ )


updateFight : Model -> ( Model, Cmd Msg, OutMsg )
updateFight model =
    case ( model.golem, model.enemyMonster ) of
        ( LivingMonster golem, LivingMonster enemyMonster ) ->
            if golem.statStamina.curVal > 0 then
                let
                    ( newGolem, damagedEnemyMonster, fightLogs ) =
                        case monsterFightsMonster golem enemyMonster of
                            --golem kills enemy
                            ( LivingMonster newGolem_, DeadMonster deadEnemy, firstFightLogs_ ) ->
                                golemKillsEnemy newGolem_ deadEnemy firstFightLogs_

                            --enemy survived, so the counter attack happens
                            ( LivingMonster newGolem_, LivingMonster survivingEnemy, firstFightLogs_ ) ->
                                monsterCounterAttacks newGolem_ survivingEnemy firstFightLogs_

                            ( DeadMonster newGolem_, LivingMonster killingEnemy, firstFightLogs_ ) ->
                                enemyKillsGolem newGolem_ killingEnemy firstFightLogs_

                            --if no dead enemy, proceed as normal
                            ( g, e, firstFightLogs_ ) ->
                                ( g, e, firstFightLogs_ )

                    _ =
                        Debug.log "fight logs" fightLogs
                in
                ( { model
                    | golem = newGolem
                    , enemyMonster = damagedEnemyMonster
                    , fightLogs = List.append model.fightLogs fightLogs
                  }
                , Cmd.none
                , case damagedEnemyMonster of
                    DeadMonster _ ->
                        DeliverItemToShopOnMonsterDefeat

                    _ ->
                        NoOutMsg
                )

            else
                ( model, Cmd.none, NoOutMsg )

        _ ->
            Debug.log "dead something" ( model, Cmd.none, NoOutMsg )


{-| called from ItemShop.updateBattleOutMsg, which does some post processing
like reading what Battle.Model.player's held\_gold and held\_blood are
-}
update : Model -> Msg -> ( Model, Cmd Msg, OutMsg )
update model battleMsg =
    case battleMsg of
        Noop ->
            ( model, Cmd.none, NoOutMsg )

        Fight ->
            updateFight model

        FindNewEnemy ->
            let
                ( newMonster, newSeed ) =
                    pickMonsterToSpawn model.battleSeed
            in
            ( { model
                | enemyMonster = LivingMonster <| newMonster
                , battleSeed = newSeed
                , fightLogs = model.fightLogs ++ [ FoundNewMonster newMonster ]
              }
            , Cmd.none
            , NoOutMsg
            )

        ToggleShowExpandedLogs ->
            ( { model | showExpandedLogs = not model.showExpandedLogs }, Cmd.none, NoOutMsg )

        HealGolem ->
            if model.player.held_blood >= 5 then
                case model.golem of
                    LivingMonster golem ->
                        if not (golem.statHP.curVal == golem.statHP.maxVal) then
                            ( { model
                                | golem =
                                    golem
                                        |> monsterStatMapHP setStatToMax
                                        |> LivingMonster
                                , player =
                                    model.player
                                        |> (\p ->
                                                { p | held_blood = p.held_blood - 5 }
                                           )
                              }
                            , Cmd.none
                            , NoOutMsg
                            )

                        else
                            ( model, Cmd.none, NoOutMsg )

                    DeadMonster golem ->
                        ( model, Cmd.none, NoOutMsg )

            else
                ( model, Cmd.none, NoOutMsg )

        ReviveGolem ->
            case model.golem of
                DeadMonster golem ->
                    let
                        newGolem =
                            monsterMap
                                (monsterStatMapHP (setStatCurVal 1)
                                    >> LivingMonster
                                )
                                model.golem
                    in
                    ( { model | golem = newGolem }, Cmd.none, NoOutMsg )

                LivingMonster _ ->
                    ( model, Cmd.none, NoOutMsg )

        --handled by parent component (would be nice to handle this nicer)
        SendOutMsg out_msg ->
            ( model, Cmd.none, out_msg )



-- end of update


monsterMap : (Monster -> a) -> DamagedMonster -> a
monsterMap callback damagedMonster =
    case damagedMonster of
        LivingMonster monster ->
            callback monster

        DeadMonster monster ->
            callback monster


padLeft : String -> Int -> String
padLeft str num =
    String.padLeft num '\u{2003}' str


padRight : String -> Int -> String
padRight str num =
    String.padRight num '\u{2003}' str


padStatBar : IntStat -> String
padStatBar stat =
    padStatStrBar (stat.curVal |> String.fromInt) (stat.maxVal |> String.fromInt)


padStatStrBar : String -> String -> String
padStatStrBar leftNum rightNum =
    padLeft leftNum 3 ++ "/" ++ padRight rightNum 3


createMonster : String -> Int -> Int -> Int -> Monster
createMonster name hpMax pwrMax protMax =
    { name = name
    , statHP = initStat hpMax
    , statStamina = initStat 10
    , statPower = initStat pwrMax
    , statProtection = initStat protMax
    , xp = 0
    }


viewMonsterInBattle : DamagedMonster -> Bool -> Element Msg
viewMonsterInBattle damagedMonster showXp =
    let
        viewMonster_ monster isDead =
            column [] <|
                [ el [ Font.size 20 ] <| text monster.name
                , if isDead then
                    UI.monospace [ alignRight, centerX ] <| text <| "DEAD!"

                  else
                    UI.monospace [] <| text <| "HP: " ++ padStatBar monster.statHP
                , UI.monospace [] <| text <| "SP: " ++ padStatBar monster.statStamina
                , UI.monospace [] <| text <| "Pwr: " ++ padLeft (String.fromInt monster.statPower.curVal) 5
                , UI.monospace [] <| text <| "Prt: " ++ padLeft (String.fromInt monster.statProtection.curVal) 5
                ]
                    ++ (if showXp then
                            [ UI.monospace [ width fill ] <| text <| "XP: " ++ padLeft (String.fromInt monster.xp) 6 ]

                        else
                            []
                       )
    in
    case damagedMonster of
        LivingMonster monster ->
            viewMonster_ monster False

        DeadMonster monster ->
            viewMonster_ monster True


viewSingleFightLog : Bool -> FightLog -> Element Msg
viewSingleFightLog expandedLog fightLog =
    if expandedLog then
        case fightLog of
            MonsterAttackedMonster { attacker, defender, attackerPower, defenderProtection, damageTaken } ->
                paragraph []
                    [ text <|
                        attacker.name
                            ++ " attacked "
                            ++ defender.name
                            ++ " for "
                            ++ String.fromInt damageTaken
                            ++ " total damage. "
                            ++ "(Power of "
                            ++ String.fromInt attackerPower
                            ++ " - Protection of "
                            ++ String.fromInt defenderProtection
                            ++ ")"
                    ]

            FoundNewMonster newMonster ->
                paragraph [] [ text <| "Found new monster: " ++ newMonster.name ]

            GolemKilledMonster attacker deadMonster xp_gained ->
                paragraph [] [ text <| attacker.name ++ " killed " ++ deadMonster.name ++ ", gaining " ++ String.fromInt xp_gained ++ " XP, and an item was put up for sale." ]

            MonsterKilledGolem golem monster ->
                paragraph [] [ text <| monster.name ++ " killed " ++ golem.name ++ ". You must now Revive your Golem." ]

    else
        case fightLog of
            MonsterAttackedMonster { attacker, defender, attackerPower, defenderProtection, damageTaken } ->
                paragraph [] [ text <| attacker.name ++ " attacked " ++ defender.name ++ " for " ++ String.fromInt damageTaken ++ " total damage." ]

            FoundNewMonster newMonster ->
                paragraph [] [ text <| "Found new monster: " ++ newMonster.name ]

            GolemKilledMonster attacker deadMonster xp_gained ->
                paragraph [] [ text <| attacker.name ++ " killed " ++ deadMonster.name ++ ", gaining " ++ String.fromInt xp_gained ++ " XP." ]

            MonsterKilledGolem golem monster ->
                paragraph [] [ text <| monster.name ++ " killed " ++ golem.name ++ ". You must now Revive your Golem." ]


viewFightLog : Bool -> List FightLog -> Element Msg
viewFightLog expandedLog fightLogs =
    column [ width fill, spacing 5 ] <|
        (fightLogs
            |> List.reverse
            |> List.take 10
            |> List.map (viewSingleFightLog expandedLog)
        )


explain =
    Element.explain Debug.todo


view : ( Int, Int ) -> Model -> Element Msg
view ( player_held_gold, player_held_blood ) model =
    column [ width fill, Font.size 16 ]
        [ el [ Font.size 24, Element.paddingEach { bottom = 20, top = 0, left = 0, right = 0 } ] <| text "Battle!"
        , row [ width fill ]
            [ column [ alignLeft, width (Element.px 200) ]
                [ Element.el [ alignLeft ] <| viewMonsterInBattle model.golem True ]
            , column [ centerX ]
                [ let
                    ( buttonType, msg, txt ) =
                        case ( model.golem, model.enemyMonster ) of
                            ( LivingMonster _, LivingMonster _ ) ->
                                ( UI.primary_button, Fight, "Fight" )

                            ( LivingMonster _, DeadMonster _ ) ->
                                ( UI.secondary_button, FindNewEnemy, "New Enemy" )

                            ( DeadMonster _, _ ) ->
                                ( UI.danger_button, Noop, "You're dead" )
                  in
                  buttonType [ width (fill |> Element.minimum 125) ] msg txt
                ]
            , column
                [ alignRight, width (Element.px 200) ]
                [ Element.el [ alignRight ] <| viewMonsterInBattle model.enemyMonster False ]
            ]
        , column [ width fill, paddingXY 0 20 ]
            [ row [ width fill, centerX ]
                [ row [ width <| fillPortion 1 ] []
                , row [ width <| fillPortion 3, Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 } ] []
                , row [ width <| fillPortion 1 ] []
                ]
            ]
        , row [ width fill ]
            [ el [ width <| fillPortion 4, alignTop ] <| viewFightLog model.showExpandedLogs model.fightLogs
            , column [ width <| fillPortion 2, alignTop ]
                [ el [ centerX, width (fill |> Element.maximum 150) ] <|
                    UI.outline_button
                        [ centerX, width fill ]
                        ToggleShowExpandedLogs
                        "Details"
                , column [ width fill, spacing 1, padding 10 ]
                    [ UI.outline_button
                        [ centerX, width (fill |> Element.maximum 150) ]
                        HealGolem
                        "Heal"
                    , UI.outline_button
                        [ centerX, width (fill |> Element.maximum 150) ]
                        ReviveGolem
                        "Revive"
                    , UI.outline_button
                        [ centerX, width (fill |> Element.maximum 150) ]
                        Noop
                        "Strengthen (No-op)"
                    , UI.outline_button
                        [ centerX, width (fill |> Element.maximum 150) ]
                        Noop
                        "Harden (No-op)"
                    ]
                , UI.outline_button
                    [ centerX, width (fill |> Element.maximum 150) ]
                    (SendOutMsg ReturnToShop)
                    "Back"
                ]
            ]
        ]


monsterTakeDamage : Int -> Monster -> DamagedMonster
monsterTakeDamage damageToTake monster =
    let
        newMonster : Monster
        newMonster =
            monsterStatMapHP (addToStatCurVal -damageToTake) monster
    in
    if newMonster.statHP.curVal > 0 then
        LivingMonster newMonster

    else
        DeadMonster newMonster


monsterFightsMonster : Monster -> Monster -> ( DamagedMonster, DamagedMonster, List FightLog )
monsterFightsMonster attacker defender =
    let
        attackerPower =
            attacker.statPower.curVal

        defenderProtection =
            defender.statProtection.curVal

        damageToTake =
            attackerPower
                - defenderProtection
                |> max 1

        staminaCostToAttack =
            -1

        newAttacker =
            LivingMonster
                (monsterStatMapStamina
                    (addToStatCurVal staminaCostToAttack)
                    attacker
                )

        newDefender =
            monsterTakeDamage damageToTake defender

        fightLog =
            MonsterAttackedMonster { attacker = attacker, defender = defender, attackerPower = attackerPower, defenderProtection = defenderProtection, damageTaken = damageToTake }
    in
    ( newAttacker, newDefender, [ fightLog ] )


pickMonsterToSpawn : Random.Seed -> ( Monster, Random.Seed )
pickMonsterToSpawn seed =
    let
        skeleton =
            createMonster "Skeleton" 15 5 5

        monsters =
            [ skeleton
            , createMonster "Ogre" 15 8 2
            , createMonster "Goblin" 10 2 0
            , createMonster "Mimic" 20 3 0
            , createMonster "Titan" 50 5 3
            , createMonster "Fae" 10 2 0
            , createMonster "Rat" 12 3 0
            , createMonster "Water Elemental" 12 0 2
            , createMonster "Fire Elemental" 11 4 2
            , createMonster "Earth Elemental" 9 6 2
            ]

        ( ( maybeChosen, unChosen ), newSeed ) =
            Random.step (Random.List.choose monsters) seed
    in
    ( Maybe.withDefault skeleton maybeChosen, newSeed )


calculateXpValue : Monster -> Int
calculateXpValue monster =
    (monster.statPower.maxVal * monster.statHP.maxVal)
        |> toFloat
        |> (\val -> val / 10)
        |> round
        |> max 1


increaseMonsterXpByMonster : Monster -> Monster -> ( Monster, Int )
increaseMonsterXpByMonster monster otherMonster =
    let
        gainedXP =
            calculateXpValue otherMonster
    in
    ( { monster | xp = monster.xp + gainedXP }, gainedXP )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


suite =
    ()
