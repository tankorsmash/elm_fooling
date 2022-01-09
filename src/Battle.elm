module Battle exposing (Model, Msg, init, subscriptions, suite, update, view)

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
    = Fight
    | FindNewEnemy


type alias IntStat =
    { curVal : Int
    , initialVal : Int
    , maxVal : Int
    }


newStat : Int -> IntStat
newStat maxVal =
    { curVal = maxVal
    , initialVal = maxVal
    , maxVal = maxVal
    }


setStatCurVal : Int -> IntStat -> IntStat
setStatCurVal newCurVal stat =
    { stat | curVal = newCurVal }


addStatCurVal : Int -> IntStat -> IntStat
addStatCurVal addedCurVal stat =
    { stat | curVal = stat.curVal + addedCurVal }


setStatMaxVal : Int -> IntStat -> IntStat
setStatMaxVal newMaxVal stat =
    { stat | maxVal = newMaxVal }


setStatInitialVal : Int -> IntStat -> IntStat
setStatInitialVal newInitialVal stat =
    { stat | initialVal = newInitialVal }


type alias Monster =
    { name : String
    , hpStat : IntStat
    , spStat : IntStat
    , powerStat : IntStat
    , protectionStat : IntStat
    , xp : Int
    }


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
    | MonsterKilledMonster Monster Monster


type alias Model =
    { golem : DamagedMonster
    , enemyMonster : DamagedMonster
    , battleSeed : Random.Seed
    , fightLogs : List FightLog
    }


init : Model
init =
    { golem = LivingMonster <| createMonster "Golem" 10 10 0
    , enemyMonster =
        createMonster "Slime" 10 2 5
            |> (\s -> { s | hpStat = s.hpStat |> setStatCurVal 4 })
            |> LivingMonster
    , battleSeed = Random.initialSeed 123456
    , fightLogs = []
    }


update : Model -> Msg -> ( Model, Cmd Msg )
update model battleMsg =
    case battleMsg of
        Fight ->
            case ( model.golem, model.enemyMonster ) of
                ( LivingMonster golem, LivingMonster enemyMonster ) ->
                    let
                        ( newGolem, damagedEnemyMonster, fightLogs ) =
                            case monsterFightsMonster golem enemyMonster of
                                ( LivingMonster newGolem_, DeadMonster deadEnemy, fightLogs_ ) ->
                                    ( LivingMonster (increaseMonsterXpByMonster newGolem_ deadEnemy)
                                    , DeadMonster deadEnemy
                                    , fightLogs_ ++ [ MonsterKilledMonster newGolem_ deadEnemy ]
                                    )

                                --if no dead enemy, proceed as normal
                                -- TODO: handle dead golem
                                ( g, e, fightLogs_ ) ->
                                    ( g, e, fightLogs_ )
                    in
                    ( { model
                        | golem = newGolem
                        , enemyMonster = damagedEnemyMonster
                        , fightLogs = List.append model.fightLogs fightLogs
                      }
                    , Cmd.none
                    )

                _ ->
                    Debug.log "dead something" ( model, Cmd.none )

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
            )


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
    , hpStat = newStat hpMax
    , spStat = newStat 10
    , powerStat = newStat pwrMax
    , protectionStat = newStat protMax
    , xp = 0
    }


viewMonsterInBattle : DamagedMonster -> Bool -> Element Msg
viewMonsterInBattle damagedMonster showXp =
    case damagedMonster of
        LivingMonster monster ->
            column [] <|
                [ el [ Font.size 20 ] <| text monster.name
                , UI.monospace [] <| text <| "HP: " ++ padStatBar monster.hpStat
                , UI.monospace [] <| text <| "SP: " ++ padStatBar monster.spStat
                , UI.monospace [] <| text <| "Pwr: " ++ padLeft (String.fromInt monster.powerStat.curVal) 5
                , UI.monospace [] <| text <| "Prt: " ++ padLeft (String.fromInt monster.protectionStat.curVal) 5
                ]
                    ++ (if showXp then
                            [ UI.monospace [ width fill ] <| text <| "XP: " ++ padLeft (String.fromInt monster.xp) 6 ]

                        else
                            []
                       )

        DeadMonster monster ->
            column []
                [ el [ Font.size 20 ] <| text monster.name
                , UI.monospace [] <| text <| "HP: " ++ "DEAD!"
                , UI.monospace [] <| text <| "SP: " ++ padStatBar monster.spStat
                ]


viewSingleFightLog : Bool -> FightLog -> Element Msg
viewSingleFightLog expandedLog fightLog =
    if expandedLog then
        case fightLog of
            MonsterAttackedMonster { attacker, defender, attackerPower, defenderProtection, damageTaken } ->
                paragraph [] [ text "attack log" ]

            FoundNewMonster newMonster ->
                paragraph [] [ text "found new monster" ]

            MonsterKilledMonster attacker deadMonster ->
                paragraph [] [ text "attacker killed monster" ]

    else
        case fightLog of
            MonsterAttackedMonster { attacker, defender, attackerPower, defenderProtection, damageTaken } ->
                paragraph [] [ text <| attacker.name ++ " attacked " ++ defender.name ++ " for " ++ String.fromInt damageTaken ++ " total damage." ]

            FoundNewMonster newMonster ->
                paragraph [] [ text <| "Found new monster: " ++ newMonster.name ]

            MonsterKilledMonster attacker deadMonster ->
                paragraph [] [ text <| attacker.name ++ " killed " ++ deadMonster.name ]


viewFightLog : Bool -> List FightLog -> Element Msg
viewFightLog expandedLog fightLogs =
    column [ width fill, spacing 5 ] <|
        (fightLogs
            |> List.reverse
            |> List.take 10
            |> List.map (viewSingleFightLog expandedLog)
        )


view : Model -> Element Msg
view model =
    column [ width fill, Font.size 16 ]
        [ el [ Font.size 24, Element.paddingEach { bottom = 20, top = 0, left = 0, right = 0 } ] <| text "Battle!"
        , row [ width fill ]
            [ column [ alignLeft ]
                [ viewMonsterInBattle model.golem True ]
            , column [ centerX ]
                [ case model.enemyMonster of
                    LivingMonster _ ->
                        UI.primary_button [] Fight "Fight"

                    DeadMonster _ ->
                        UI.primary_button [] FindNewEnemy "New Enemy"
                ]
            , column
                [ alignRight ]
                [ viewMonsterInBattle model.enemyMonster False ]
            ]
        , column [ width fill, paddingXY 0 20 ]
            [ row [ width fill, centerX ]
                [ row [ width <| fillPortion 1 ] []
                , row [ width <| fillPortion 3, Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 } ] []
                , row [ width <| fillPortion 1 ] []
                ]
            ]
        , el [ width fill ] <| viewFightLog False model.fightLogs
        ]


monsterTakeDamage : Int -> Monster -> DamagedMonster
monsterTakeDamage damageToTake monster =
    let
        newMonster : Monster
        newMonster =
            { monster | hpStat = addStatCurVal -damageToTake monster.hpStat }
    in
    if newMonster.hpStat.curVal > 0 then
        LivingMonster newMonster

    else
        DeadMonster newMonster


monsterFightsMonster : Monster -> Monster -> ( DamagedMonster, DamagedMonster, List FightLog )
monsterFightsMonster attacker defender =
    let
        attackerPower =
            attacker.powerStat.curVal

        defenderProtection =
            defender.protectionStat.curVal

        damageToTake =
            attackerPower
                - defenderProtection
                |> max 1

        newAttacker =
            LivingMonster attacker

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
    (monster.powerStat.maxVal * monster.hpStat.maxVal)
        |> toFloat
        |> (\val -> val / 10)
        |> round
        |> max 1


increaseMonsterXpByMonster : Monster -> Monster -> Monster
increaseMonsterXpByMonster monster otherMonster =
    { monster | xp = monster.xp + calculateXpValue otherMonster }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


suite =
    ()
