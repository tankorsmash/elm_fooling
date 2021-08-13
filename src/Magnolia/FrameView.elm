module Magnolia.FrameView exposing (Model, Msg, init, update, view)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Browser.Navigation as Nav
import Debug
import FormData
    exposing
        ( DataType(..)
        , new_form_field_float
        , new_form_field_int
        , new_form_field_string
        )
import Html
    exposing
        ( Html
        , a
        , b
        , br
        , button
        , div
        , form
        , h1
        , h2
        , h3
        , h4
        , img
        , input
        , p
        , span
        , table
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes exposing (attribute, classList, href, property, src, style, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, at, field, list, string)
import Json.Encode exposing (string)
import List
import Magnolia.ArmorFrame exposing (ArmorFrame)
import Magnolia.WeaponCategoryFrame exposing (WeaponCategoryFrame)
import Magnolia.AttributeFrame exposing (AttributeFrame)
import Magnolia.WeaponFrame exposing (WeaponFrame)
import Magnolia.ZoneFrame exposing (ZoneFrame)
import OpenDota.OpenDota as OpenDota
import PostData exposing (PostData)
import Reddit
import String
import Table exposing (ColumnDef, ColumnType(..), PageInfoMsg, TableDefinition, update_page_info, view)
import Task
import Time
import Url
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string)
import Utils exposing (add_class)
import Weather


type Msg
    = SubmitFormData
    | GotEditWeaponFormUpdate Magnolia.WeaponFrame.EditFormUpdateType
    | GotEditZoneFormUpdate Magnolia.ZoneFrame.EditFormUpdateType
    | GotEditWeaponCategoryFormUpdate Magnolia.WeaponCategoryFrame.EditFormUpdateType
    | GotEditAttributeFormUpdate Magnolia.AttributeFrame.EditFormUpdateType
    | GotEditArmorFormUpdate Magnolia.ArmorFrame.EditFormUpdateType
    | GotTabMsg Tab.State


type alias FrameEditData f msg =
    { form_definition : FormData.FormDefinition f msg
    , frame_data : f
    , saved_frame_data : Maybe f
    }


type FrameType
    = WeaponFrame
    | ArmorFrame
    | ZoneFrame


type alias FrameEditDatas =
    { weapon : FrameEditData WeaponFrame Msg
    , armor : FrameEditData ArmorFrame Msg
    , zone : FrameEditData ZoneFrame Msg
    , weapon_category : FrameEditData WeaponCategoryFrame Msg
    , attribute : FrameEditData AttributeFrame Msg
    }


type alias Model =
    { frame_edit_datas : FrameEditDatas
    , active_tab : Tab.State
    }


tab_prefix : String
tab_prefix =
    "frame_view_tab__"


init : Model
init =
    let
        weapon_frame_data : WeaponFrame
        weapon_frame_data =
            { weapon_name = "unset in init wapn_ame"
            , frame_id = 123
            , choice_id = -1
            , pretty_name = "Pretty Wepn Name"
            , description = "This is a description"
            , frame_image_path = "weapon_img.png"
            , battle_row_type = Magnolia.WeaponFrame.Rear
            , damage_type = Magnolia.WeaponFrame.Slashing
            , bonus_attack = 0
            , bonus_power = 0
            , bonus_encumbrance = 0
            , rarity_type = 0
            , carry_weight = 0
            }

        saved_weapon_frame_data : Maybe WeaponFrame
        saved_weapon_frame_data =
            Nothing

        armor_frame_data : ArmorFrame
        armor_frame_data =
            { pretty_name = "Unset ArmorFrame Name"
            , frame_id = 123
            , bonus_defense = 0
            , bonus_protection = 0
            , bonus_protection_piercing = 0
            , bonus_protection_blunt = 0
            , bonus_protection_slashing = 0
            , bonus_encumbrance = 0
            , rarity_type = 0
            , carry_weight = 0
            }

        saved_armor_frame_data : Maybe ArmorFrame
        saved_armor_frame_data =
            Nothing

        zone_frame_data : ZoneFrame
        zone_frame_data =
            { name = "unset in init zone"
            , data_name = "unset_data_name"
            , required_zone_data_name_to_unlock = ""
            , location_data_names_in_the_zone = [ "first", "second" ]
            }

        saved_zone_frame_data : Maybe ZoneFrame
        saved_zone_frame_data =
            Nothing

        weapon_category_frame_data : WeaponCategoryFrame
        weapon_category_frame_data =
            { frame_id = 0
            , pretty_name = "unset in init weapon_category"
            , description = "description"
            , frame_image_path = "unset .png"
            , rarity_type = 0
            , weapon_frame_ids = []
            , rank_1_attr_frame_ids = [ 1, 11, 111 ]
            , rank_2_attr_frame_ids = [ 2, 22, 222 ]
            , rank_3_attr_frame_ids = [ 3, 33, 333 ]
            }

        saved_weapon_category_frame_data : Maybe WeaponCategoryFrame
        saved_weapon_category_frame_data =
            Nothing

        attribute_frame_data : AttributeFrame
        attribute_frame_data =
            { frame_id = 0
            , pretty_name = "unset in init attribute"
            , description = "description"
            , frame_image_path = "unset .png"
            , state_names = []
            , state_names_pretty_funcs = []
            , pretty_name_template = ""
            , pretty_func_name = ""
            }

        saved_attribute_frame_data : Maybe AttributeFrame
        saved_attribute_frame_data =
            Nothing
    in
    { frame_edit_datas =
        { weapon =
            { form_definition = Magnolia.WeaponFrame.edit_form_definition GotEditWeaponFormUpdate
            , frame_data = weapon_frame_data
            , saved_frame_data = saved_weapon_frame_data
            }
        , armor =
            { form_definition = Magnolia.ArmorFrame.edit_form_definition GotEditArmorFormUpdate
            , frame_data = armor_frame_data
            , saved_frame_data = saved_armor_frame_data
            }
        , zone =
            { form_definition = Magnolia.ZoneFrame.edit_form_definition GotEditZoneFormUpdate
            , frame_data = zone_frame_data
            , saved_frame_data = saved_zone_frame_data
            }
        , weapon_category =
            { form_definition = Magnolia.WeaponCategoryFrame.edit_form_definition GotEditWeaponCategoryFormUpdate
            , frame_data = weapon_category_frame_data
            , saved_frame_data = saved_weapon_category_frame_data
            }
        , attribute =
            { form_definition = Magnolia.AttributeFrame.edit_form_definition GotEditAttributeFormUpdate
            , frame_data = attribute_frame_data
            , saved_frame_data = saved_attribute_frame_data
            }
        }
    , active_tab = Tab.customInitialState <| tab_prefix ++ "attribute_frame"
    }


update_frame_edit_data : FrameEditData fd Msg -> (fd -> subMsg -> fd) -> subMsg -> FrameEditData fd Msg
update_frame_edit_data frame_edit_data updater form_update_type =
    let
        frame_data =
            frame_edit_data.frame_data
    in
    { frame_edit_data | frame_data = updater frame_data form_update_type }


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        SubmitFormData ->
            let
                weapon_fed =
                    model.frame_edit_datas.weapon

                frame_edit_datas =
                    model.frame_edit_datas

                frame_data =
                    weapon_fed.frame_data
            in
            ( { model
                | frame_edit_datas =
                    { frame_edit_datas
                        | weapon =
                            { weapon_fed | saved_frame_data = Just frame_data }
                    }
              }
            , Cmd.none
            )

        GotEditWeaponFormUpdate form_update_type ->
            let
                feds =
                    model.frame_edit_datas

                weapon_fed =
                    feds.weapon

                updater =
                    Magnolia.WeaponFrame.update_edit_form_data

                new_frame_edit_datas =
                    { feds
                        | weapon =
                            update_frame_edit_data weapon_fed updater form_update_type
                    }
            in
            ( { model | frame_edit_datas = new_frame_edit_datas }
            , Cmd.none
            )

        GotEditArmorFormUpdate form_update_type ->
            let
                feds =
                    model.frame_edit_datas

                armor_fed =
                    feds.armor

                frame_data =
                    armor_fed.frame_data

                updater =
                    Magnolia.ArmorFrame.update_edit_form_data

                new_feds =
                    { feds | armor = update_frame_edit_data armor_fed updater form_update_type }
            in
            ( { model
                | frame_edit_datas =
                    new_feds
              }
            , Cmd.none
            )

        GotEditZoneFormUpdate form_update_type ->
            let
                frame_edit_datas =
                    model.frame_edit_datas

                zone_fed =
                    frame_edit_datas.zone

                frame_data =
                    zone_fed.frame_data
            in
            ( { model
                | frame_edit_datas =
                    { frame_edit_datas
                        | zone =
                            { zone_fed | frame_data = Magnolia.ZoneFrame.update_edit_form_data frame_data form_update_type }
                    }
              }
            , Cmd.none
            )

        GotEditWeaponCategoryFormUpdate form_update_type ->
            let
                frame_edit_datas =
                    model.frame_edit_datas

                weapon_category_fed =
                    frame_edit_datas.weapon_category

                frame_data =
                    weapon_category_fed.frame_data
            in
            ( { model
                | frame_edit_datas =
                    { frame_edit_datas
                        | weapon_category =
                            { weapon_category_fed | frame_data = Magnolia.WeaponCategoryFrame.update_edit_form_data frame_data form_update_type }
                    }
              }
            , Cmd.none
            )

        GotEditAttributeFormUpdate form_update_type ->
            let
                frame_edit_datas =
                    model.frame_edit_datas

                attribute_fed =
                    frame_edit_datas.attribute

                frame_data =
                    attribute_fed.frame_data
            in
            ( { model
                | frame_edit_datas =
                    { frame_edit_datas
                        | attribute =
                            { attribute_fed | frame_data = Magnolia.AttributeFrame.update_edit_form_data frame_data form_update_type }
                    }
              }
            , Cmd.none
            )

        GotTabMsg new_state ->
            ( { model | active_tab = new_state }, Cmd.none )


bootstrap_button type_ on_click text_ =
    Button.button
        [ type_
        , Button.attrs [ onClick on_click ]
        ]
        [ text text_ ]


button_primary : msg -> String -> Html msg
button_primary on_click text_ =
    bootstrap_button Button.primary on_click text_


type alias TabItemConfig =
    { id : String, link_text : String, header : String, view : Html Msg }


render_tab_item : TabItemConfig -> Tab.Item Msg
render_tab_item config =
    Tab.item
        { id = config.id
        , link = Tab.link [] [ text config.link_text ]
        , pane =
            Tab.pane [ Spacing.mt3 ]
                [ h4 [] [ text config.header ]
                , config.view
                ]
        }


tabs_view : Model -> Html Msg
tabs_view model =
    let
        tab_configs : List TabItemConfig
        tab_configs =
            [ { id = tab_prefix ++ "weapon_frame", link_text = "WeaponFrame", header = "Edit WeaponFrame", view = form_data_view model.frame_edit_datas.weapon }
            , { id = tab_prefix ++ "armor_frame", link_text = "ArmorFrame", header = "Edit ArmorFrame", view = form_data_view model.frame_edit_datas.armor }
            , { id = tab_prefix ++ "zone_frame", link_text = "ZoneFrame", header = "Edit ZoneFrame", view = form_data_view model.frame_edit_datas.zone }
            , { id = tab_prefix ++ "weapon_category_frame", link_text = "WeaponCategoryFrame", header = "Edit WeaponCategoryFrame", view = form_data_view model.frame_edit_datas.weapon_category }
            , { id = tab_prefix ++ "attribute_frame", link_text = "AttributeFrame", header = "Edit AttributeFrame", view = form_data_view model.frame_edit_datas.attribute }
            ]

        tab_items =
            List.map render_tab_item tab_configs
    in
    Tab.config GotTabMsg
        |> Tab.items tab_items
        |> Tab.view model.active_tab


view : Model -> Html Msg
view model =
    div []
        [ tabs_view model
        ]


form_data_view : FrameEditData obj Msg -> Html Msg
form_data_view frame_edit_data =
    let
        { frame_data, form_definition, saved_frame_data } =
            frame_edit_data

        rendered_saved_form_data : Html Msg
        rendered_saved_form_data =
            case saved_frame_data of
                Nothing ->
                    div [] []

                Just saved_weapon_frame_data ->
                    div []
                        [ div [] [ text "Saved Form Data" ]
                        , FormData.render_fields form_definition.fields saved_weapon_frame_data
                        , br [] []
                        ]
    in
    Grid.row [ Row.centerMd ]
        [ Grid.col [ Col.sm11, Col.md8 ]
            [ rendered_saved_form_data
            , Form.form []
                [ FormData.render_fields form_definition.fields frame_data
                , button_primary SubmitFormData "Submit"
                ]
            ]
        ]
