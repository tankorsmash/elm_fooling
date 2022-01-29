module Battle exposing (DefeatAction(..), Model, Msg(..), OutMsg(..), doesGolemNeedStamina, increaseGolemStamina, init, monsterMap, secondsRequiredForSpRefill, subscriptions, suite, update, view)

import Array
import Browser.Dom
import Browser.Events
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
import Test exposing (Test, describe, fuzz, test)
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
    | TickSecond Time.Posix
    | ToggleShowLocationTypeMenu
    | ChangeLocation LocationId
    | LevelUpGolem
    | RefillGolemSp
    | GotUiOptionsMsg UiOptionMsg


type DefeatAction
    = NoDefeatAction
    | DeliverItemToShop


type OutMsg
    = NoOutMsg
    | ReturnToShop
    | OnMonsterDefeat DefeatAction


type
    UiOptionMsg
    -- = MouseEnterShopItem ListContext ( CharacterId, Item )
    -- | MouseLeaveShopItem ListContext ( CharacterId, Item )
    = GotTooltipMsg UI.TooltipMsg
    | GotTooltipSize (Result Browser.Dom.Error Browser.Dom.Element)


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
    { stat
        | curVal =
            (stat.curVal + addedCurVal)
                |> min stat.maxVal
                |> max 0
    }


setStatMaxVal : Int -> IntStat -> IntStat
setStatMaxVal newMaxVal stat =
    { stat | maxVal = newMaxVal }


setStatInitialVal : Int -> IntStat -> IntStat
setStatInitialVal newInitialVal stat =
    { stat | initialVal = newInitialVal }


isStatMaxVal : IntStat -> Bool
isStatMaxVal stat =
    stat.curVal == stat.maxVal


isStatNotMax : IntStat -> Bool
isStatNotMax stat =
    stat.curVal /= stat.maxVal


isStatEmpty : IntStat -> Bool
isStatEmpty stat =
    stat.curVal == 0


isStatNotEmpty : IntStat -> Bool
isStatNotEmpty stat =
    stat.curVal /= 0


type alias Monster =
    { name : String
    , statHP : IntStat
    , statStamina : IntStat
    , statPower : IntStat
    , statProtection : IntStat
    , level : Int
    , xp : Int
    , onDefeat : DefeatAction
    }


setGolem : DamagedMonster -> Model -> Model
setGolem newGolem model =
    { model | golem = newGolem }


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


conditionalAlpha : Bool -> Bool -> Float
conditionalAlpha invisCondition semiCondition =
    if invisCondition then
        0.0

    else if semiCondition then
        0.5

    else
        1.0


conditionalMsg : Bool -> Msg -> Msg
conditionalMsg condition msg =
    if condition then
        msg

    else
        Noop


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
    | PlayerHealedGolem Int
    | PlayerRevivedGolem


{-| we're going to have to import Character at some point, for now though this is good enough
-}
type alias BattleCharacter =
    { held_gold : Int
    , held_blood : Int
    }


type LocationType
    = Forest
    | Mountains
    | Plains


type alias LocationId =
    Int


type alias Location =
    { locationType : LocationType
    , name : String
    , monstersLeft : Int
    , locationId : LocationId
    }


maxMonstersPerLocation : Int
maxMonstersPerLocation =
    2


createLocation : LocationType -> LocationId -> String -> Location
createLocation locationType locationId name =
    { locationType = locationType
    , name = name
    , monstersLeft = maxMonstersPerLocation
    , locationId = locationId
    }


type alias Locations =
    { forest : Location
    , mountains : Location
    , plains : Location
    }


locationToPretty : LocationType -> String
locationToPretty location =
    case location of
        Forest ->
            "The Forest"

        Mountains ->
            "The Mountains"

        Plains ->
            "The Plains"


getLocationsList : Locations -> List Location
getLocationsList locations =
    [ locations.forest
    , locations.mountains
    , locations.plains
    ]


getCurrentLocation { currentLocationId, locations } =
    let
        potentialLocations =
            getLocationsList locations
    in
    potentialLocations
        |> List.foldl
            (\pl acc ->
                if pl.locationId == currentLocationId then
                    Just pl

                else
                    acc
            )
            Nothing
        |> Maybe.withDefault locations.forest


mapCurrentLocation : Model -> (Location -> Location) -> Locations
mapCurrentLocation model locationUpdater =
    let
        currentLocation =
            getCurrentLocation model

        { currentLocationId, locations } =
            model
    in
    if currentLocationId == locations.forest.locationId then
        { locations | forest = locationUpdater locations.forest }

    else if currentLocationId == locations.mountains.locationId then
        { locations | mountains = locationUpdater locations.mountains }

    else if currentLocationId == locations.plains.locationId then
        { locations | plains = locationUpdater locations.plains }

    else
        locations


type alias SecondsWaitedSince =
    { lastLocationMonsterRefill : Int
    }


type alias UiOptions =
    { hoveredTooltip : UI.HoveredTooltip
    , cachedTooltipOffsets : Dict.Dict String UI.TooltipData
    }


type alias Model =
    { golem : DamagedMonster
    , enemyMonster : Maybe DamagedMonster
    , battleSeed : Random.Seed
    , fightLogs : List FightLog
    , showExpandedLogs : Bool
    , player : BattleCharacter --NOTE: this is hackily read from in ItemShop's updateBattleOutMsg and used to update ItemShop's player. FIXME hack on that for sure
    , spRefillAmount : Int --NOTE: this is from ItemShop's update too
    , secondsWaitedSince : SecondsWaitedSince
    , shouldShowLocationTypeMenu : Bool
    , currentLocationId : LocationId
    , locations : Locations
    , uiOptions : UiOptions
    }


createMonster : String -> Int -> Int -> Int -> Monster
createMonster name hpMax pwrMax protMax =
    { name = name
    , statHP = initStat hpMax
    , statStamina = initStat 10
    , statPower = initStat pwrMax
    , statProtection = initStat protMax
    , xp = 0
    , level = 1
    , onDefeat = NoDefeatAction
    }


setOnDefeat : DefeatAction -> Monster -> Monster
setOnDefeat defeatAction monster =
    { monster | onDefeat = defeatAction }


debugMode =
    True


init : { a | held_blood : Int, held_gold : Int } -> Int -> Model
init { held_blood, held_gold } spRefillAmount =
    let
        locations : Locations
        locations =
            { forest = createLocation Forest 0 "The Forest"
            , mountains = createLocation Mountains 1 "The Mountains"
            , plains = createLocation Plains 2 "The Plains"
            }
    in
    { golem = LivingMonster <| createMonster "Golem" 25 10 0
    , enemyMonster =
        createMonster "Slime" 10 2 5
            |> monsterStatMapHP (setStatCurVal 4)
            |> setOnDefeat DeliverItemToShop
            |> LivingMonster
            |> Just
    , battleSeed = Random.initialSeed 123456
    , fightLogs = []
    , showExpandedLogs = False
    , player = { held_blood = held_blood, held_gold = held_gold }
    , spRefillAmount = spRefillAmount
    , secondsWaitedSince = { lastLocationMonsterRefill = 0 }
    , shouldShowLocationTypeMenu = False
    , currentLocationId = locations.forest.locationId
    , locations = locations
    , uiOptions = { hoveredTooltip = UI.NoHoveredTooltip, cachedTooltipOffsets = Dict.empty }
    }


golemKillsEnemy : Model -> Monster -> Monster -> List FightLog -> ( Model, OutMsg )
golemKillsEnemy model golem deadEnemy fightLogs =
    let
        ( victorGolem, gainedXp ) =
            addMonsterXpByMonster golem deadEnemy

        decrementMonstersLeft =
            \curLocation ->
                { curLocation | monstersLeft = curLocation.monstersLeft - 1 }
    in
    ( { model
        | golem = LivingMonster victorGolem
        , enemyMonster = Just <| DeadMonster deadEnemy
        , fightLogs = model.fightLogs ++ fightLogs ++ [ GolemKilledMonster golem deadEnemy gainedXp ]
        , locations = mapCurrentLocation model decrementMonstersLeft
      }
    , OnMonsterDefeat deadEnemy.onDefeat
    )


enemyKillsGolem : Model -> Monster -> Monster -> List FightLog -> ( Model, OutMsg )
enemyKillsGolem model golem enemy existingFightLogs =
    ( { model
        | golem = DeadMonster golem
        , enemyMonster = Just <| LivingMonster enemy
        , fightLogs = model.fightLogs ++ existingFightLogs ++ [ MonsterKilledGolem golem enemy ]
      }
    , NoOutMsg
    )


monsterCounterAttacks : Model -> Monster -> Monster -> List FightLog -> ( Model, OutMsg )
monsterCounterAttacks model golem enemy existingFightLogs =
    case monsterFightsMonster enemy golem of
        --enemy killed golem in the counterattack
        ( LivingMonster killingEnemy, DeadMonster deadGolem, secondFightLogs_ ) ->
            enemyKillsGolem model deadGolem killingEnemy (existingFightLogs ++ secondFightLogs_)

        ( e, g, secondFightLogs_ ) ->
            ( { model
                | golem = g
                , enemyMonster = Just e
                , fightLogs = model.fightLogs ++ existingFightLogs ++ secondFightLogs_
              }
            , NoOutMsg
            )


updateFightWithLivingGolemAndEnemy : Model -> ( Monster, Monster ) -> ( Model, Cmd Msg, OutMsg )
updateFightWithLivingGolemAndEnemy model ( golem, livingMonster ) =
    if golem.statStamina.curVal > 0 then
        let
            currentLocation =
                getCurrentLocation model

            ( newModel, newOutMsg ) =
                case monsterFightsMonster golem livingMonster of
                    --golem kills enemy
                    ( LivingMonster newGolem_, DeadMonster deadEnemy, firstFightLogs_ ) ->
                        golemKillsEnemy model newGolem_ deadEnemy firstFightLogs_

                    --enemy survived, so the counter attack happens
                    ( LivingMonster newGolem_, LivingMonster survivingEnemy, firstFightLogs_ ) ->
                        monsterCounterAttacks model newGolem_ survivingEnemy firstFightLogs_

                    ( DeadMonster newGolem_, LivingMonster killingEnemy, firstFightLogs_ ) ->
                        enemyKillsGolem model newGolem_ killingEnemy firstFightLogs_

                    --if no dead enemy, proceed as normal
                    ( g, e, firstFightLogs_ ) ->
                        ( { model
                            | golem = g
                            , enemyMonster = Just e
                            , fightLogs = model.fightLogs ++ firstFightLogs_
                          }
                        , NoOutMsg
                        )
        in
        ( newModel
        , Cmd.none
        , newOutMsg
        )

    else
        ( model, Cmd.none, NoOutMsg )


updateFight : Model -> ( Model, Cmd Msg, OutMsg )
updateFight model =
    model.enemyMonster
        |> Maybe.map
            (\enemyMonster ->
                case ( model.golem, enemyMonster ) of
                    ( LivingMonster golem, LivingMonster livingMonster ) ->
                        updateFightWithLivingGolemAndEnemy model ( golem, livingMonster )

                    _ ->
                        Debug.log "dead something" ( model, Cmd.none, NoOutMsg )
            )
        |> Maybe.withDefault ( model, Cmd.none, NoOutMsg )


healGolemBloodCost : Int
healGolemBloodCost =
    5


reviveGolemBloodCost : Int
reviveGolemBloodCost =
    25


levelUpXpCost : Int
levelUpXpCost =
    100


monsterHasXpToLevelUp : Monster -> Bool
monsterHasXpToLevelUp monster =
    monster.xp >= levelUpXpCost


{-| NOTE: assumes the monster has enough XP
-}
addMonsterLevel : Int -> Monster -> Monster
addMonsterLevel toAdd ({ xp, level } as monster) =
    { monster | level = level + toAdd }
        |> addMonsterXp -(levelUpXpCost * toAdd)


secondsRequiredForSpRefill : Int
secondsRequiredForSpRefill =
    5


secondsRequiredForLocationMonsterRefill : Int
secondsRequiredForLocationMonsterRefill =
    60


updateTick : Model -> Time.Posix -> ( Model, Cmd Msg, OutMsg )
updateTick model time =
    let
        origSecondsWaitedSince =
            model.secondsWaitedSince

        incrSecondsWaitedSince =
            { origSecondsWaitedSince
                | lastLocationMonsterRefill = origSecondsWaitedSince.lastLocationMonsterRefill + 1
            }

        newModel =
            model
                |> --apply incremented ticks to all times
                   (\m ->
                        { m | secondsWaitedSince = incrSecondsWaitedSince }
                   )
                |> -- location monster refil
                   (\({ secondsWaitedSince, locations } as m) ->
                        if m.secondsWaitedSince.lastLocationMonsterRefill >= secondsRequiredForLocationMonsterRefill then
                            let
                                locationsList =
                                    getLocationsList m.locations

                                locationRefiller location =
                                    if location.monstersLeft < maxMonstersPerLocation then
                                        { location | monstersLeft = location.monstersLeft + 1 }

                                    else
                                        location

                                newLocations =
                                    { locations
                                        | forest = locationRefiller locations.forest
                                        , mountains = locationRefiller locations.mountains
                                        , plains = locationRefiller locations.plains
                                    }

                                newSecondsWaitedSince =
                                    { secondsWaitedSince | lastLocationMonsterRefill = 0 }
                            in
                            { m
                                | locations = newLocations
                                , secondsWaitedSince = newSecondsWaitedSince
                            }

                        else
                            m
                   )
    in
    ( newModel, Cmd.none, NoOutMsg )


updateUiOption : (UiOptions -> UiOptions) -> Model -> Model
updateUiOption updater m =
    { m | uiOptions = updater m.uiOptions }


{-| called from ItemShop.updateBattleOutMsg, which does some post processing
like reading what Battle.Model.player's held\_gold and held\_blood are
-}
update : Model -> Msg -> ( Model, Cmd Msg, OutMsg )
update model battleMsg =
    let
        noop =
            ( model, Cmd.none, NoOutMsg )
    in
    case battleMsg of
        Noop ->
            ( model, Cmd.none, NoOutMsg )

        Fight ->
            updateFight model

        FindNewEnemy ->
            let
                currentLocation =
                    getCurrentLocation model
            in
            if currentLocation.monstersLeft > 0 then
                let
                    ( newMonster, newSeed ) =
                        pickMonsterToSpawn model.battleSeed currentLocation
                in
                ( { model
                    | enemyMonster = Just <| LivingMonster newMonster
                    , battleSeed = newSeed
                    , fightLogs = model.fightLogs ++ [ FoundNewMonster newMonster ]
                  }
                , Cmd.none
                , NoOutMsg
                )

            else
                ( model, Cmd.none, NoOutMsg )

        ToggleShowExpandedLogs ->
            ( { model | showExpandedLogs = not model.showExpandedLogs }, Cmd.none, NoOutMsg )

        HealGolem ->
            if model.player.held_blood >= healGolemBloodCost then
                case model.golem of
                    LivingMonster golem ->
                        if not (golem.statHP.curVal == golem.statHP.maxVal) then
                            let
                                currentHP =
                                    golem.statHP.curVal

                                maxHP =
                                    golem.statHP.maxVal
                            in
                            ( { model
                                | golem =
                                    golem
                                        |> monsterStatMapHP setStatToMax
                                        |> LivingMonster
                                , player =
                                    model.player
                                        |> (\p ->
                                                { p | held_blood = p.held_blood - healGolemBloodCost }
                                           )
                                , fightLogs =
                                    model.fightLogs ++ [ PlayerHealedGolem (maxHP - currentHP) ]
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
            if model.player.held_blood >= reviveGolemBloodCost then
                case model.golem of
                    DeadMonster golem ->
                        let
                            newGolem =
                                golem
                                    |> monsterStatMapHP (setStatCurVal 1)
                                    |> LivingMonster

                            newPlayer =
                                model.player
                                    |> (\p ->
                                            { p
                                                | held_blood = p.held_blood - reviveGolemBloodCost
                                            }
                                       )
                        in
                        ( { model
                            | golem = newGolem
                            , player = newPlayer
                            , fightLogs = model.fightLogs ++ [ PlayerRevivedGolem ]
                          }
                        , Cmd.none
                        , NoOutMsg
                        )

                    LivingMonster _ ->
                        ( model, Cmd.none, NoOutMsg )

            else
                ( model, Cmd.none, NoOutMsg )

        --handled by parent component (would be nice to handle this nicer)
        SendOutMsg out_msg ->
            ( model, Cmd.none, out_msg )

        TickSecond time ->
            updateTick model time

        ToggleShowLocationTypeMenu ->
            ( { model | shouldShowLocationTypeMenu = not model.shouldShowLocationTypeMenu }, Cmd.none, NoOutMsg )

        ChangeLocation newLocationId ->
            ( { model | currentLocationId = newLocationId, shouldShowLocationTypeMenu = False, enemyMonster = Nothing }, Cmd.none, NoOutMsg )

        LevelUpGolem ->
            let
                newGolem =
                    monsterIdentityMap
                        (addMonsterLevel 1
                            >> monsterStatMapHP (addToStatMaxVal 3)
                            >> monsterStatMapHP (addToStatCurVal 3)
                        )
                        model.golem
            in
            ( { model | golem = newGolem }, Cmd.none, NoOutMsg )

        RefillGolemSp ->
            if canAffordRefillSp model.player then
                let
                    newGolem =
                        monsterIdentityMap
                            (monsterStatMapStamina (addToStatCurVal 1))
                            model.golem

                    newPlayer =
                        model.player
                            |> (\p ->
                                    { p
                                        | held_blood = p.held_blood - refillGolemSpCost
                                    }
                               )
                in
                ( { model | golem = newGolem, player = newPlayer }, Cmd.none, NoOutMsg )

            else
                ( model, Cmd.none, NoOutMsg )

        GotUiOptionsMsg uiMsg ->
            case uiMsg of
                GotTooltipMsg tooltipMsg ->
                    case tooltipMsg of
                        UI.StartTooltipHover tooltip_id ->
                            ( updateUiOption
                                (\uio ->
                                    { uio
                                        | hoveredTooltip =
                                            Dict.get tooltip_id uio.cachedTooltipOffsets
                                                |> Maybe.withDefault { offsetX = 0, offsetY = 0, hoveredTooltipId = tooltip_id }
                                                |> UI.HoveredTooltipWithoutOffset
                                    }
                                )
                                model
                            , Task.attempt (GotUiOptionsMsg << GotTooltipSize) (Browser.Dom.getElement ("tooltip__" ++ tooltip_id))
                            , NoOutMsg
                            )

                        UI.EndTooltipHover tooltip_id ->
                            ( updateUiOption (\uio -> { uio | hoveredTooltip = UI.NoHoveredTooltip }) model, Cmd.none, NoOutMsg )

                GotTooltipSize tooltip_size_result ->
                    case tooltip_size_result of
                        Ok sizes ->
                            let
                                viewport_width =
                                    sizes.viewport.width

                                viewport_height =
                                    sizes.viewport.height

                                { x, y, width, height } =
                                    sizes.element

                                offsetX : Float
                                offsetX =
                                    toFloat <|
                                        if x < 0 then
                                            floor <| abs x + 10

                                        else if x + width > viewport_width then
                                            floor <| (viewport_width - (x + width)) - 10

                                        else
                                            floor <| 0

                                offsetY : Float
                                offsetY =
                                    toFloat <|
                                        if y < 0 then
                                            floor <| abs y + 10

                                        else if y + height > viewport_height then
                                            floor <| (viewport_height - (y + height)) - 10

                                        else
                                            floor <| 0
                            in
                            case model.uiOptions.hoveredTooltip of
                                UI.NoHoveredTooltip ->
                                    noop

                                UI.HoveredTooltipWithoutOffset oldTooltipData ->
                                    let
                                        newTooltipData =
                                            -- have to add the old offsets back in, because the new tooltip_size_result includes the cached size, so it needs to be accounted for
                                            { offsetX = offsetX + oldTooltipData.offsetX
                                            , offsetY = offsetY + oldTooltipData.offsetY
                                            , hoveredTooltipId = oldTooltipData.hoveredTooltipId
                                            }
                                    in
                                    ( updateUiOption
                                        (\uio ->
                                            { uio
                                                | cachedTooltipOffsets = Dict.insert oldTooltipData.hoveredTooltipId newTooltipData uio.cachedTooltipOffsets
                                                , hoveredTooltip = UI.HoveredTooltipWithOffset newTooltipData
                                            }
                                        )
                                        model
                                    , Cmd.none
                                    , NoOutMsg
                                    )

                                UI.HoveredTooltipWithOffset oldTooltipData ->
                                    let
                                        newTooltipData =
                                            { oldTooltipData
                                                | offsetX = offsetX
                                                , offsetY = offsetY
                                            }
                                    in
                                    ( updateUiOption (\uio -> { uio | hoveredTooltip = UI.HoveredTooltipWithOffset newTooltipData }) model
                                    , Cmd.none
                                    , NoOutMsg
                                    )

                        Err _ ->
                            noop



-- end of update


{-| does something on the actual monster and returns anything
-}
monsterMap : (Monster -> a) -> DamagedMonster -> a
monsterMap callback damagedMonster =
    case damagedMonster of
        LivingMonster monster ->
            callback monster

        DeadMonster monster ->
            callback monster


{-| does something on the actual monster and returns the same type of DamagedMonster
-}
monsterIdentityMap : (Monster -> Monster) -> DamagedMonster -> DamagedMonster
monsterIdentityMap callback damagedMonster =
    case damagedMonster of
        LivingMonster monster ->
            LivingMonster <| callback monster

        DeadMonster monster ->
            DeadMonster <| callback monster


{-| does something on the actual monster, so long as its living
-}
monsterLivingMap : (Monster -> Monster) -> DamagedMonster -> DamagedMonster
monsterLivingMap callback damagedMonster =
    case damagedMonster of
        LivingMonster monster ->
            LivingMonster <| callback monster

        DeadMonster monster ->
            damagedMonster


{-| does something on the actual monster, so long as its dead
-}
monsterDeadMap : (Monster -> Monster) -> DamagedMonster -> DamagedMonster
monsterDeadMap callback damagedMonster =
    case damagedMonster of
        LivingMonster monster ->
            damagedMonster

        DeadMonster monster ->
            DeadMonster <| callback monster


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


viewMonsterInBattle : DamagedMonster -> Bool -> Element Msg
viewMonsterInBattle damagedMonster showExtra =
    let
        viewMonster_ monster isDead =
            column [] <|
                [ paragraph [ Font.size 20 ] <|
                    [ el [] <| text <| monster.name
                    , if showExtra then
                        el [ Font.size 12 ] <| text <| " Lv. " ++ String.fromInt monster.level

                      else
                        Element.none
                    ]
                , if isDead then
                    UI.monospace [ alignRight, centerX ] <| text <| "DEAD!"

                  else
                    UI.monospace [] <| text <| "HP: " ++ padStatBar monster.statHP
                , UI.monospace [] <| text <| "SP: " ++ padStatBar monster.statStamina
                , UI.monospace [] <| text <| "Pwr: " ++ padLeft (String.fromInt monster.statPower.curVal) 5
                , UI.monospace [] <| text <| "Prt: " ++ padLeft (String.fromInt monster.statProtection.curVal) 5
                ]
                    ++ (if showExtra then
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
    case fightLog of
        MonsterAttackedMonster { attacker, defender, attackerPower, defenderProtection, damageTaken } ->
            if expandedLog then
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

            else
                paragraph [] [ text <| attacker.name ++ " attacked " ++ defender.name ++ " for " ++ String.fromInt damageTaken ++ " total damage." ]

        FoundNewMonster newMonster ->
            paragraph [] [ text <| "Found new monster: " ++ newMonster.name ]

        GolemKilledMonster attacker deadMonster xp_gained ->
            paragraph []
                [ text <|
                    attacker.name
                        ++ " killed "
                        ++ deadMonster.name
                        ++ ", gaining "
                        ++ String.fromInt xp_gained
                        ++ " XP"
                        ++ (if deadMonster.onDefeat == DeliverItemToShop then
                                ", and an item was put up for sale."

                            else
                                ""
                           )
                ]

        MonsterKilledGolem golem monster ->
            paragraph [] [ text <| monster.name ++ " killed " ++ golem.name ++ ". You must now Revive your Golem." ]

        PlayerHealedGolem amount ->
            paragraph [] [ text <| "You healed your creature by " ++ String.fromInt amount ++ " HP." ]

        PlayerRevivedGolem ->
            paragraph [] [ text <| "You revived your creature." ]


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


fillMax : Int -> Element.Length
fillMax pxWidth =
    fill |> Element.maximum pxWidth


fillMin : Int -> Element.Length
fillMin pxWidth =
    fill |> Element.minimum pxWidth


refillGolemSpCost : Int
refillGolemSpCost =
    5



-- sample tooltip
-- UI.buttonWithTooltip
--         (UI.TextParams
--             { buttonType = UI.Primary
--             , colorTheme = replaceMeColorTheme
--             , customAttrs = []
--             , onPressMsg = Noop
--             , textLabel = "test"
--             }
--         )
--         { onTooltipMsg = GotUiOptionsMsg << GotTooltipMsg
--         , tooltip_body = UI.TooltipText "body test"
--         , tooltip_id = "ssssdsdsd"
--         }
--         uiOptions.hoveredTooltip


canAffordRefillSp : BattleCharacter -> Bool
canAffordRefillSp player =
    player.held_blood >= refillGolemSpCost


viewBattleControls : Model -> List (Element Msg)
viewBattleControls { golem, player, enemyMonster, uiOptions } =
    let
        canAffordHealGolem =
            player.held_blood >= healGolemBloodCost

        golemHealable =
            case golem of
                LivingMonster livingGolem ->
                    livingGolem.statHP
                        |> (\statHP ->
                                (statHP.curVal > 0)
                                    && (statHP.curVal < statHP.maxVal)
                           )

                DeadMonster _ ->
                    False

        canAffordReviveGolem =
            player.held_blood >= reviveGolemBloodCost

        golemRevivable =
            case golem of
                LivingMonster _ ->
                    False

                DeadMonster _ ->
                    True

        canAffordGolemLevelUp : Bool
        canAffordGolemLevelUp =
            monsterMap monsterHasXpToLevelUp golem

        golemLevelupable =
            case golem of
                LivingMonster _ ->
                    True

                DeadMonster _ ->
                    False

        golemSpRefillable =
            monsterMap (\g -> g.statStamina.curVal < g.statStamina.maxVal)

        controlButton : List (Element.Attribute Msg) -> Msg -> String -> String -> Element Msg
        controlButton attrs msg txt tooltipText =
            UI.buttonWithTooltip
                (UI.TextParams
                    { colorTheme = UI.BrightTheme
                    , onPressMsg = msg
                    , textLabel = txt
                    , buttonType = UI.Outline
                    , customAttrs = [ centerX, width (fillMax 150) ] ++ attrs
                    }
                )
                { tooltip_id = "control_button__" ++ txt
                , tooltip_body = UI.TooltipText tooltipText
                , onTooltipMsg = GotUiOptionsMsg << GotTooltipMsg
                }
                uiOptions.hoveredTooltip

        canChangeLocationNow =
            canChangeLocation golem enemyMonster
    in
    [ el [ centerX, width (fillMax 150) ] <|
        -- Toggle Details
        controlButton []
            ToggleShowExpandedLogs
            "Details"
            "Toggles combat details"
    , el [ centerX, width (fillMax 150), Element.paddingEach { top = 10, bottom = 0, left = 0, right = 0 } ] <|
        -- Change Location
        controlButton [ Element.transparent <| not canChangeLocationNow ]
            (conditionalMsg canChangeLocationNow ToggleShowLocationTypeMenu)
            "Change Location"
            "Changes the location of the battle."
    , column [ width fill, spacing 1, padding 10 ]
        [ -- Heal
          controlButton
            [ Element.alpha <|
                conditionalAlpha (not golemHealable) (not canAffordHealGolem)
            ]
            (conditionalMsg (golemHealable && canAffordHealGolem) HealGolem)
            "Heal"
            "Heals your golem to full HP"
        , -- Revive
          controlButton
            [ Element.alpha <|
                conditionalAlpha (not golemRevivable) (not canAffordReviveGolem)
            ]
            (conditionalMsg (golemRevivable && canAffordReviveGolem) ReviveGolem)
            "Revive"
            "Revives your golem from the dead with 1HP"
        , -- Level Up
          controlButton
            [ Element.alpha <|
                conditionalAlpha (not golemLevelupable) (not canAffordGolemLevelUp)
            ]
            (conditionalMsg (golemLevelupable && canAffordGolemLevelUp) LevelUpGolem)
            "Strengthen"
            "Levels up your golem"
        , -- Refill SP
          controlButton
            [ Element.alpha <|
                conditionalAlpha (not <| golemSpRefillable golem) (not <| canAffordRefillSp player)
            ]
            (conditionalMsg
                (golemSpRefillable golem && canAffordRefillSp player)
                RefillGolemSp
            )
            "Envigorate"
            "Refills your golem's SP"
        ]
    , UI.outline_button
        [ centerX, width (fillMax 150) ]
        (SendOutMsg ReturnToShop)
        "Back"
    ]


canChangeLocation : DamagedMonster -> Maybe DamagedMonster -> Bool
canChangeLocation golem enemyMonster =
    case ( golem, enemyMonster ) of
        ( LivingMonster livingGolem, Just (DeadMonster deadEnemy) ) ->
            True

        ( LivingMonster livingGolem, Nothing ) ->
            True

        _ ->
            False


dividingLine : Element msg
dividingLine =
    column [ width fill, paddingXY 0 20 ]
        [ row [ width fill, centerX ]
            [ row [ width <| fillPortion 1 ] []
            , row [ width <| fillPortion 3, Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 } ] []
            , row [ width <| fillPortion 1 ] []
            ]
        ]


replaceMeColorTheme =
    UI.BrightTheme


viewLocationTypeMenu : Model -> Element Msg
viewLocationTypeMenu model =
    column [ width fill, Font.size 16 ]
        [ el
            [ Font.size 24
            , Element.paddingEach { bottom = 20, top = 0, left = 0, right = 0 }
            ]
          <|
            text "Choose LocationType"
        , paragraph [ padding 10 ] [ text "You may choose a new LocationType to visit." ]
        , paragraph [ padding 10 ] [ text "A new LocationType contains new monsters to fight, which may bring new rewards!" ]
        , column [ width fill, spacing 10, padding 10 ]
            (model.locations
                |> getLocationsList
                |> List.map
                    (\location ->
                        UI.button <|
                            UI.CustomParams
                                { buttonType = UI.Secondary
                                , colorTheme = replaceMeColorTheme
                                , customAttrs = [ width fill ]
                                , onPressMsg = ChangeLocation location.locationId
                                , customLabel =
                                    column [ centerX, spacing 5 ]
                                        [ el [ centerX ] <| text location.name
                                        , el [ centerX, Font.size 12 ] <|
                                            column [ width fill ]
                                                [ el [ Font.underline, centerX, width fill ] <| text "Monsters Remain"
                                                , el [ centerX ] <| text <| String.fromInt location.monstersLeft
                                                ]
                                        ]
                                }
                    )
                |> List.append
                    [ UI.button <|
                        UI.CustomParams
                            { buttonType = UI.Outline
                            , colorTheme = replaceMeColorTheme
                            , customAttrs = [ width fill ]
                            , onPressMsg = ToggleShowLocationTypeMenu
                            , customLabel =
                                column [ centerX, spacing 5 ]
                                    [ el [ centerX ] <| text "Back"
                                    ]
                            }
                    ]
            )
        ]


viewBattleMode : Model -> Element Msg
viewBattleMode model =
    column [ width fill, Font.size 16 ]
        [ el [ Font.size 24, Element.paddingEach { bottom = 20, top = 0, left = 0, right = 0 } ] <| text "Battle!"
        , row [ width fill ]
            [ column [ alignLeft, width (Element.px 200) ]
                [ Element.el [ alignLeft ] <| viewMonsterInBattle model.golem True ]
            , column [ centerX, spacing 10 ]
                [ let
                    ( buttonType, onPressMsg, textLabel ) =
                        case ( model.golem, model.enemyMonster ) of
                            ( LivingMonster _, Just (LivingMonster _) ) ->
                                ( UI.Primary, Fight, "Fight" )

                            ( LivingMonster _, Just (DeadMonster _) ) ->
                                if (getCurrentLocation model).monstersLeft > 0 then
                                    ( UI.Secondary, FindNewEnemy, "Find New Enemy" )

                                else
                                    ( UI.Secondary, Noop, "No enemies remain" )

                            ( LivingMonster _, Nothing ) ->
                                if (getCurrentLocation model).monstersLeft > 0 then
                                    ( UI.Secondary, FindNewEnemy, "Find New Enemy" )

                                else
                                    ( UI.Secondary, Noop, "No enemies remain" )

                            ( DeadMonster _, _ ) ->
                                ( UI.Danger, Noop, "You're dead" )
                  in
                  UI.button
                    (UI.TextParams <|
                        { buttonType = buttonType
                        , customAttrs =
                            [ width (fillMin 125)
                            , if onPressMsg == Noop then
                                Element.mouseOver []

                              else
                                attrNone
                            ]
                        , onPressMsg = onPressMsg
                        , textLabel = textLabel
                        , colorTheme = replaceMeColorTheme
                        }
                    )
                , let
                    currentLocation =
                        getCurrentLocation model
                  in
                  column [ centerX, spacing 2 ]
                    [ el [ Font.underline, Font.size 10, centerX ] <|
                        text "Current Location"
                    , el [ centerX ] <| text <| currentLocation.name
                    , el [ Font.underline, Font.size 10, centerX ] <|
                        text "Enemies Remaining"
                    , el [ centerX ] <|
                        text <|
                            String.fromInt currentLocation.monstersLeft
                    ]
                ]
            , column
                [ alignRight, width (Element.px 200), height fill ]
                [ Element.el [ alignRight, height fill ] <|
                    case model.enemyMonster of
                        Just enemyMonster ->
                            viewMonsterInBattle enemyMonster False

                        Nothing ->
                            paragraph [ centerY ]
                                [ text "Find an enemy to fight"
                                ]
                ]
            ]
        , dividingLine
        , row [ width fill ]
            [ el [ width <| fillPortion 4, alignTop ] <| viewFightLog model.showExpandedLogs model.fightLogs
            , column [ width <| fillPortion 2, alignTop ]
                (viewBattleControls model)
            ]
        , if debugMode then
            column [ width fill ]
                [ dividingLine
                , column [ width fill, paddingXY 0 20 ]
                    [ el [ Font.underline ] <|
                        text "Debug"
                    , text <|
                        "Time Until Location Monster Refill: "
                            ++ String.fromInt (secondsRequiredForLocationMonsterRefill - model.secondsWaitedSince.lastLocationMonsterRefill)
                    ]
                , dividingLine
                ]

          else
            Element.none
        ]


view : Model -> Element Msg
view model =
    if not model.shouldShowLocationTypeMenu then
        viewBattleMode model

    else
        viewLocationTypeMenu model


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


pickMonsterToSpawn : Random.Seed -> Location -> ( Monster, Random.Seed )
pickMonsterToSpawn seed { locationType } =
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
                ++ (case locationType of
                        Forest ->
                            [ createMonster "Forest Elf" 10 3 1
                            , createMonster "Dire Wolf" 15 5 0
                            , createMonster "Ursa" 20 4 3
                            ]

                        Mountains ->
                            [ createMonster "Hillock" 30 1 3
                            , createMonster "Valley Goat" 15 2 0
                            , createMonster "Elephant" 40 4 4
                            ]

                        Plains ->
                            [ createMonster "Mole" 10 2 1
                            , createMonster "Ferret" 20 3 1
                            , createMonster "Plainshawk" 30 2 3
                            ]
                   )

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


addMonsterXp : Int -> Monster -> Monster
addMonsterXp toAdd monster =
    { monster | xp = monster.xp + toAdd }


addMonsterXpByMonster : Monster -> Monster -> ( Monster, Int )
addMonsterXpByMonster monster otherMonster =
    let
        gainedXp =
            calculateXpValue otherMonster
    in
    ( addMonsterXp gainedXp monster, gainedXp )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 TickSecond


attrNone : Element.Attribute Msg
attrNone =
    Element.htmlAttribute <| Html.Attributes.class ""


{-| this is exposed to ItemShop, so im keeping it as broad as possible, so that
ItemShop doesn't need to know much about it. we'll see if its a good idea
-}
increaseGolemStamina : Model -> Int -> Model
increaseGolemStamina ({ golem } as model) staminaToGain =
    golem
        |> monsterLivingMap
            (monsterStatMapStamina (addToStatCurVal staminaToGain))
        |> (\g -> setGolem g model)


{-| this is exposed to ItemShop, so im keeping it as broad as possible, so that
ItemShop doesn't need to know much about it. we'll see if its a good idea
-}
doesGolemNeedStamina : Model -> Bool
doesGolemNeedStamina ({ golem } as model) =
    case golem of
        LivingMonster livingGolem ->
            livingGolem
                |> .statStamina
                |> isStatNotMax

        DeadMonster _ ->
            False


natural =
    Fuzz.intRange 0 Random.maxInt


positive =
    Fuzz.intRange 0 Random.maxInt


suite : Test
suite =
    describe "root test suite"
        [ describe "golem stuff"
            [ describe "golem's blood link sp gain "
                [ fuzz (Fuzz.intRange 0 90) "living golem gets sp" <|
                    \staminaToGain ->
                        let
                            testGolem : Monster
                            testGolem =
                                createMonster "Test Golem" 99 99 99
                                    |> monsterStatMapStamina (setStatCurVal 0)
                                    |> --make sure there's always enough stats to gain
                                       monsterStatMapStamina (setStatMaxVal (staminaToGain * 2))

                            model : Model
                            model =
                                init { held_blood = 100, held_gold = 0 } staminaToGain
                                    |> (setGolem <| LivingMonster testGolem)
                        in
                        Expect.equal (testGolem.statStamina.curVal + staminaToGain) <|
                            (increaseGolemStamina model staminaToGain
                                |> .golem
                                |> monsterMap
                                    (.statStamina >> .curVal)
                            )
                , fuzz (Fuzz.intRange 0 90) "dead golems do not get sp" <|
                    \staminaToGain ->
                        let
                            testGolem : Monster
                            testGolem =
                                createMonster "Test Golem" 99 99 99
                                    |> monsterStatMapStamina (setStatCurVal 0)
                                    |> --make sure there's always enough stats to gain
                                       monsterStatMapStamina (setStatMaxVal (staminaToGain * 2))

                            model : Model
                            model =
                                init { held_blood = 100, held_gold = 0 } staminaToGain
                                    |> (setGolem <| DeadMonster testGolem)
                        in
                        Expect.equal (testGolem.statStamina.curVal + 0) <|
                            (increaseGolemStamina model staminaToGain
                                |> .golem
                                |> monsterMap
                                    (.statStamina >> .curVal)
                            )
                ]
            ]
        ]
