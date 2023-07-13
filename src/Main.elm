module Main exposing (main, view)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Color exposing (Color)
import Html exposing (Attribute, Html, a, button, div, figure, footer, h1, h3, header, img, input, label, nav, option, p, section, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, id, min, placeholder, selected, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (index)
import List exposing (append, drop, indexedMap, length, map, map2, range, take)
import Path
import Round
import Shape exposing (defaultPieConfig)
import Svg.Attributes exposing (offset, style)
import Time
import TypedSvg exposing (circle, g, path, polygon, rect, svg, text_)
import TypedSvg.Attributes exposing (cx, cy, d, dy, fill, fillOpacity, height, opacity, points, r, stroke, strokeWidth, textAnchor, transform, viewBox, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..), em, px)
import Url


main :
    Program
        ()
        { key : Nav.Key
        , url : Url.Url
        , counter : Int
        , page : Page
        , modal : Maybe ModalState
        , popUp : Maybe PopupState
        , foods : List Food
        , currNutrition : Nutrition
        , searchTerm : String
        , response : List Ingredient
        , selectedFood : Food
        , errorMsg : String
        , settings : Settings
        }
        Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , counter : Int
    , page : Page
    , modal : Maybe ModalState
    , popUp : Maybe PopupState
    , foods : List Food
    , currNutrition : Nutrition
    , searchTerm : String
    , response : List Ingredient
    , selectedFood : Food
    , errorMsg : String
    , settings : Settings
    }



---UtilTypes---


type Page
    = Home
    | Liste
    | Suche
    | Einstellungen


type ModalState
    = ShowFood Food


type PopupState
    = FoodAdded


type Input
    = SearchInput
    | FoodAmountInput
    | SearchNumberInput
    | SearchSortInput
    | SearchSortDirectionInput
    | KcalInput
    | FatSliderInput
    | CarbsSliderInput
    | ProteinSliderInput



--- Settings ---


type alias Settings =
    { searchSettings : { number : String, sort : String, sortDirection : String }
    , nutritionSettings : { kcalGoal : String, fatSplit : String, carbsSplit : String, proteinSplit : String }
    }



--- Food ----


type alias Food =
    { name : String, id : Int, img : String, amount : Float, nutrition : Nutrition }


type alias Nutrition =
    { kcal : Float, protein : Float, fat : Float, carbs : Float }


type NutrientList
    = List Nutrient


type alias Nutrient =
    { name : String, amount : Float }


type alias Ingredient =
    { name : String, id : Int, img : String }



--- init ---


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , counter = 0
      , page =
            case url.fragment of
                Just "home" ->
                    Home

                Just "list" ->
                    Liste

                Just "search" ->
                    Suche

                Just "settings" ->
                    Einstellungen

                _ ->
                    Home
      , modal = Nothing
      , popUp = Nothing
      , foods = []
      , currNutrition = { kcal = 0.001, protein = 0.001, fat = 0.001, carbs = 0.001 }
      , searchTerm = ""
      , response = []
      , selectedFood = { name = "", id = 0, img = "", amount = 1.0, nutrition = { kcal = 0.0, protein = 0.0, fat = 0.0, carbs = 0.0 } }
      , errorMsg = ""
      , settings =
            { searchSettings = { number = "10", sort = "protein", sortDirection = "desc" }
            , nutritionSettings =
                { kcalGoal = "2000"
                , fatSplit = "30"
                , carbsSplit = "30"
                , proteinSplit = "40"
                }
            }
      }
    , Cmd.none
    )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Tick Time.Posix
    | OpenModal ModalMsg
    | CloseModal
    | TogglePopUp
    | ChangeFoods FoodMsg
    | GotFoods (Result Http.Error HTTPSearchResults)
    | GotFoodData (Result Http.Error Food)
    | Input Input String
    | KeyDown Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model
                | url = url
                , page =
                    case url.fragment of
                        Just "home" ->
                            Home

                        Just "list" ->
                            Liste

                        Just "search" ->
                            Suche

                        Just "settings" ->
                            Einstellungen

                        _ ->
                            Home
              }
            , Cmd.none
            )

        Tick newTime ->
            ( { model
                | counter = model.counter - 1
                , popUp =
                    if model.counter >= 0 then
                        Just FoodAdded

                    else
                        Nothing
              }
            , Cmd.none
            )

        OpenModal modalMsg ->
            updateModal modalMsg model

        CloseModal ->
            ( { model | modal = Nothing }, Cmd.none )

        TogglePopUp ->
            ( { model
                | popUp = Just FoodAdded
                , counter = 5
              }
            , Cmd.none
            )

        ChangeFoods foodMsg ->
            updateFood foodMsg model

        GotFoods httpResponse ->
            case httpResponse of
                Ok result ->
                    ( { model | response = result.results }, Cmd.none )

                Err _ ->
                    ( { model | errorMsg = "Liste wird nicht abgeholt" }, Cmd.none )

        GotFoodData httpResponse ->
            case httpResponse of
                Ok result ->
                    ( { model
                        | selectedFood = result
                        , modal = Just (ShowFood result)
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | errorMsg = "Informationen werden nicht abgeholt" }, Cmd.none )

        Input inputType input ->
            case inputType of
                SearchInput ->
                    ( { model | searchTerm = input }, Cmd.none )

                FoodAmountInput ->
                    ( { model | selectedFood = setAmount model.selectedFood input }, Cmd.none )

                SearchNumberInput ->
                    ( { model | settings = setSearchSettings model.settings "number" input }, Cmd.none )

                SearchSortInput ->
                    ( { model | settings = setSearchSettings model.settings "sort" input }, Cmd.none )

                SearchSortDirectionInput ->
                    ( { model | settings = setSearchSettings model.settings "direction" input }, Cmd.none )

                KcalInput ->
                    ( { model | settings = setNutritionSettings model.settings "kcal" input }, Cmd.none )

                FatSliderInput ->
                    ( { model | settings = setNutritionSettings model.settings "fat" input }, Cmd.none )

                CarbsSliderInput ->
                    ( { model | settings = setNutritionSettings model.settings "carbs" input }, Cmd.none )

                ProteinSliderInput ->
                    ( { model | settings = setNutritionSettings model.settings "protein" input }, Cmd.none )

        KeyDown key ->
            if key == 13 then
                updateFood (GetFoods model.searchTerm) model

            else
                ( model, Cmd.none )


type FoodMsg
    = GetFoods String
    | GetFoodData Food String
    | AddFood
    | DeleteFood Int


updateFood : FoodMsg -> Model -> ( Model, Cmd Msg )
updateFood foodMsg model =
    case foodMsg of
        DeleteFood i ->
            let
                newFoods =
                    append (take i model.foods) (drop (i + 1) model.foods)
            in
            if length model.foods == 1 then
                ( { model
                    | foods = []
                    , currNutrition = { kcal = 0.001, protein = 0.001, fat = 0.001, carbs = 0.001 }
                  }
                , Cmd.none
                )

            else
                ( { model
                    | foods = newFoods
                    , currNutrition = updateCurrNutrition newFoods
                  }
                , Cmd.none
                )

        GetFoods food ->
            ( model
            , Http.request
                { method = "GET"
                , headers = []
                , url =
                    let
                        settings =
                            model.settings.searchSettings
                    in
                    "https://api.spoonacular.com/food/ingredients/search?query="
                        ++ food
                        ++ "&number="
                        ++ settings.number
                        ++ "&sort="
                        ++ settings.sort
                        ++ "&sortDirection="
                        ++ settings.sortDirection
                        ++ "&apiKey=a25e9078614b4a79948065747e2cc8cf"
                , body = Http.emptyBody
                , expect = Http.expectJson GotFoods resultDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        AddFood ->
            ( let
                food =
                    setNutrition model.selectedFood

                newFoods =
                    List.append model.foods
                        [ food
                        ]
              in
              { model
                | foods = newFoods
                , currNutrition = updateCurrNutrition newFoods
                , modal = Nothing
                , popUp = Just FoodAdded
                , counter = 3
              }
            , Cmd.none
            )

        GetFoodData food id ->
            ( { model | modal = Just (ShowFood food) }
            , Http.request
                { method = "GET"
                , headers = []
                , url = "https://api.spoonacular.com/food/ingredients/" ++ id ++ "/information?amount=1&apiKey=a25e9078614b4a79948065747e2cc8cf"
                , body = Http.emptyBody
                , expect = Http.expectJson GotFoodData foodDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )


updateCurrNutrition : List Food -> Nutrition
updateCurrNutrition foods =
    { kcal = List.sum (List.map (\food -> food.nutrition.kcal) foods)
    , protein = List.sum (List.map (\food -> food.nutrition.protein) foods)
    , fat = List.sum (List.map (\food -> food.nutrition.fat) foods)
    , carbs = List.sum (List.map (\food -> food.nutrition.carbs) foods)
    }


type ModalMsg
    = OpenFood Food Int


updateModal : ModalMsg -> Model -> ( Model, Cmd Msg )
updateModal modalMsg model =
    case modalMsg of
        OpenFood food id ->
            ( { model | modal = Just (ShowFood food) }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Nutri Tracker 23"
    , body =
        [ div []
            [ navbar model
            , pageContent model
            , popup model
            ]
        ]
    }


navbar : Model -> Html Msg
navbar model =
    nav [ class "navbar is-primary" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item", href "#home" ]
                [ h1 [ class "tile" ] [ img [ src "./src/NutriTrack23.svg" ] [] ]
                ]
            ]
        , div [ class "navbar-menu" ]
            [ div [ class "navbar-start" ]
                [ a [ class "navbar-item", href "#home" ] [ text "Home" ]
                , a [ class "navbar-item", href "#search" ] [ text "Suche" ]
                , a [ class "navbar-item", href "#list" ] [ text "Liste" ]
                ]
            , div [ class "navbar-end" ]
                [ a [ class "navbar-item", href "#settings" ] [ span [ class "icon" ] [ Html.i [ class "fas fa-cog" ] [] ] ] ]
            ]
        ]


pageContent : Model -> Html Msg
pageContent model =
    case model.page of
        Home ->
            let
                nS =
                    model.settings.nutritionSettings

                fat =
                    (Maybe.withDefault 2000 (String.toFloat nS.kcalGoal) / 9.0) * (Maybe.withDefault 30 (String.toFloat nS.fatSplit) / 100)

                carbs =
                    (Maybe.withDefault 2000 (String.toFloat nS.kcalGoal) / 4.0) * (Maybe.withDefault 30 (String.toFloat nS.carbsSplit) / 100)

                protein =
                    (Maybe.withDefault 2000 (String.toFloat nS.kcalGoal) / 4.0) * (Maybe.withDefault 30 (String.toFloat nS.proteinSplit) / 100)
            in
            section [ class "section" ]
                [ nav [ class "level" ]
                    [ div [ class "level-item has-text-centered" ]
                        [ div []
                            [ p [ class "heading" ] [ text "Kalorien" ]
                            , p [ class "title" ] [ text (Round.round 2 model.currNutrition.kcal ++ " kcal") ]
                            ]
                        ]
                    , div [ class "level-item has-text-centered" ]
                        [ div []
                            [ p [ class "heading" ] [ text "Fett" ]
                            , p [ class "title" ] [ text (Round.round 2 model.currNutrition.fat ++ " g") ]
                            ]
                        ]
                    , div [ class "level-item has-text-centered" ]
                        [ div []
                            [ p [ class "heading" ] [ text "Kohlenhydrate" ]
                            , p [ class "title" ] [ text (Round.round 2 model.currNutrition.carbs ++ " g") ]
                            ]
                        ]
                    , div [ class "level-item has-text-centered" ]
                        [ div []
                            [ p [ class "heading" ] [ text "Eiweiß" ]
                            , p [ class "title" ] [ text (Round.round 2 model.currNutrition.protein ++ " g") ]
                            ]
                        ]
                    ]
                , div [ style "display: flex; align-items: center;" ]
                    [ span [ style "display: inline-block; width: 33%; padding: 0em 2em 0em;" ]
                        [ div [ style "max-width: 500px; margin:auto;" ]
                            [ if model.currNutrition.kcal <= Maybe.withDefault 2000 (String.toFloat nS.kcalGoal) then
                                foodChart
                                    [ ( "noch " ++ (Round.round 2 (Maybe.withDefault 2000 (String.toFloat nS.kcalGoal) - model.currNutrition.kcal) ++ " kcal"), model.currNutrition.kcal )
                                    , ( "", Maybe.withDefault 2000 (String.toFloat nS.kcalGoal) - model.currNutrition.kcal )
                                    ]
                                    { innerRadius = 80, corner = 10, position = 0 }
                                    (Array.fromList [ Color.rgb255 0 158 134, Color.rgb255 206 219 234 ])

                              else
                                foodChart
                                    [ ( Round.round 2 (model.currNutrition.kcal - Maybe.withDefault 2000 (String.toFloat nS.kcalGoal)) ++ " kcal zu viel", model.currNutrition.kcal - Maybe.withDefault 2000 (String.toFloat nS.kcalGoal) )
                                    , ( "", (Maybe.withDefault 4000 (String.toFloat nS.kcalGoal) * 2.0) - model.currNutrition.kcal )
                                    ]
                                    { innerRadius = 80, corner = 10, position = 0 }
                                    (Array.fromList [ Color.rgb255 211 86 70, Color.rgb255 0 158 134 ])
                            ]
                        ]
                    , span [ style "display: inline-block; width: 33%; padding: 0em 2em 0em;" ]
                        [ div [ style "max-width: 500px; margin:auto;" ]
                            [ foodChart
                                [ ( "", model.currNutrition.protein )
                                , ( "", model.currNutrition.carbs )
                                , ( "", model.currNutrition.fat )
                                ]
                                { innerRadius = 0, corner = 0, position = 50 }
                                (Array.fromList [ Color.rgb255 0 209 178, Color.rgb255 0 158 134, Color.rgb255 0 107 90 ])
                            ]
                        ]
                    , span [ style "display: inline-block; width: 33%; padding: 0em 2em 0em;" ]
                        [ div [ style "display: flex; align-items: center;" ]
                            [ span [ style "display: inline-block; width: auto; margin-right: 0.5em;" ]
                                [ div [ class "tags has-addons" ]
                                    [ span [ class "tag", style "background-color: #006b5a; color: #ebf3fc; font-size: 1.2vw;" ] [ text "Fett" ]
                                    , if model.currNutrition.fat <= fat then
                                        span [ class "tag", style "background-color: #cedbea; font-size: 1.2vw;" ] [ text ("noch " ++ Round.round 2 (fat - model.currNutrition.fat) ++ " g") ]

                                      else
                                        span [ class "tag", style "background-color: #d35646; font-size: 1.2vw;" ] [ text (Round.round 2 (model.currNutrition.fat - fat) ++ " g zu viel") ]
                                    ]
                                ]
                            , span [ style "display: inline-block; width: 15%;" ]
                                [ if model.currNutrition.fat <= fat then
                                    foodChart
                                        [ ( "", model.currNutrition.fat )
                                        , ( "", fat - model.currNutrition.fat )
                                        ]
                                        { innerRadius = 60, corner = 10, position = 0 }
                                        (Array.fromList [ Color.rgb255 0 107 90, Color.rgb255 206 219 234 ])

                                  else
                                    foodChart
                                        [ ( "", model.currNutrition.fat - fat )
                                        , ( "", fat * 2.0 - model.currNutrition.fat )
                                        ]
                                        { innerRadius = 60, corner = 10, position = 0 }
                                        (Array.fromList [ Color.rgb255 211 86 70, Color.rgb255 0 107 90 ])
                                ]
                            ]
                        , div [ style "display: flex; align-items: center;" ]
                            [ span [ style "display: inline-block; width: auto; margin-right: 0.5em;" ]
                                [ div [ class "tags has-addons" ]
                                    [ span [ class "tag", style "background-color: #009e86; color: #ebf3fc; font-size: 1.2vw;" ] [ text "Kohlenhydrate" ]
                                    , if model.currNutrition.carbs <= carbs then
                                        span [ class "tag", style "background-color: #cedbea; font-size: 1.2vw;" ] [ text ("noch " ++ Round.round 2 (carbs - model.currNutrition.carbs) ++ " g") ]

                                      else
                                        span [ class "tag", style "background-color: #d35646; font-size: 1.2vw;" ] [ text (Round.round 2 (model.currNutrition.carbs - carbs) ++ " g zu viel") ]
                                    ]
                                ]
                            , span [ style "display: inline-block; width: 15%;" ]
                                [ if model.currNutrition.carbs <= carbs then
                                    foodChart
                                        [ ( "", model.currNutrition.carbs )
                                        , ( "", carbs - model.currNutrition.carbs )
                                        ]
                                        { innerRadius = 60, corner = 10, position = 0 }
                                        (Array.fromList [ Color.rgb255 0 158 134, Color.rgb255 206 219 234 ])

                                  else
                                    foodChart
                                        [ ( "", model.currNutrition.carbs - carbs )
                                        , ( "", carbs * 2.0 - model.currNutrition.carbs )
                                        ]
                                        { innerRadius = 60, corner = 10, position = 0 }
                                        (Array.fromList [ Color.rgb255 211 86 70, Color.rgb255 0 158 134 ])
                                ]
                            ]
                        , div [ style "display: flex; align-items: center;" ]
                            [ span [ style "display: inline-block; width: auto; margin-right: 0.5em;" ]
                                [ div [ class "tags has-addons" ]
                                    [ span [ class "tag", style "background-color: #00d1b2; color: #ebf3fc; font-size: 1.2vw;" ] [ text "Eiweiss" ]
                                    , if model.currNutrition.protein <= protein then
                                        span [ class "tag", style "background-color: #cedbea; font-size: 1.2vw;" ] [ text ("noch " ++ Round.round 2 (protein - model.currNutrition.protein) ++ " g") ]

                                      else
                                        span [ class "tag", style "background-color: #d35646; font-size: 1.2vw;" ] [ text (Round.round 2 (model.currNutrition.protein - protein) ++ " g zu viel") ]
                                    ]
                                ]
                            , span [ style "display: inline-block; width: 15%;" ]
                                [ if model.currNutrition.protein <= protein then
                                    foodChart
                                        [ ( "", model.currNutrition.protein )
                                        , ( "", protein - model.currNutrition.protein )
                                        ]
                                        { innerRadius = 60, corner = 10, position = 0 }
                                        (Array.fromList [ Color.rgb255 0 209 178, Color.rgb255 206 219 234 ])

                                  else
                                    foodChart
                                        [ ( "", model.currNutrition.protein - protein )
                                        , ( "", protein * 2.0 - model.currNutrition.protein )
                                        ]
                                        { innerRadius = 60, corner = 10, position = 0 }
                                        (Array.fromList [ Color.rgb255 211 86 70, Color.rgb255 0 209 178 ])
                                ]
                            ]
                        ]
                    ]
                ]

        Liste ->
            section [ class "section" ]
                [ div [ class "container" ]
                    [ if length model.foods == 0 then
                        div []
                            [ div []
                                [ h1 [ class "subtitle", style "text-align: center;" ]
                                    [ text "Noch kein Eintrag hinzugefügt."
                                    ]
                                , div [ Html.Attributes.style "display" "flex", Html.Attributes.style "justify-content" "center" ] [ img [ class "image", src "./src/Ente.svg", Html.Attributes.style "height" "750px", Html.Attributes.style "width" "500px" ] [] ]
                                ]
                            ]

                      else
                        foodTable model
                    ]
                ]

        Suche ->
            section [ class "section" ]
                [ div [ class "container" ]
                    [ div [ class "field has-addons" ]
                        [ div [ class "control" ] [ input [ class "input", type_ "text", placeholder "Suchen...", onInput (Input SearchInput), onKeyDown KeyDown, Html.Attributes.style "width" "75em" ] [] ]
                        , div [ class "control" ] [ button [ class "button is-primary", onClick (ChangeFoods (GetFoods model.searchTerm)) ] [ text "Suchen" ] ]
                        ]
                    , searchResultsTable model
                    , modalView model
                    ]
                ]

        Einstellungen ->
            section [ class "section" ] [ settingsView model ]


popup : Model -> Html Msg
popup model =
    div
        [ Html.Attributes.class
            (if model.popUp == Just FoodAdded && model.counter > 0 then
                "animate__animated animate__bounce showSnackbar box"

             else
                "animate__animated animate__bounce snackbar box "
            )
        ]
        [ div
            []
            [ text "Eintrag hinzugefügt" ]
        ]


modalView : Model -> Html Msg
modalView model =
    case model.modal of
        Nothing ->
            span [] []

        Just modalState ->
            div [ class "modal is-active" ]
                [ div [ class "modal-background" ] []
                , case modalState of
                    ShowFood food ->
                        showFoodModal food
                ]


modalHeader : String -> Html Msg
modalHeader title =
    header [ class "modal-card-head" ]
        [ p [ class "modal-card-title" ] [ text title ]
        , button [ class "delete", ariaLabel "close", onClick CloseModal ] []
        ]


modalFooter : List (Html Msg) -> Html Msg
modalFooter modalButtons =
    footer [ class "modal-card-foot" ]
        (modalButtons
            ++ [ button [ class "button is-primary", onClick (ChangeFoods AddFood) ] [ text "Hinzufügen" ]
               , button [ class "button", onClick CloseModal ] [ text "Schließen" ]
               ]
        )


showFoodModal : Food -> Html Msg
showFoodModal food =
    let
        s =
            Html.Attributes.style
    in
    div [ class "modal-card", s "width" "50em" ]
        [ modalHeader food.name
        , div [ class "modal-card-body" ]
            [ div [ class "tile is-ancestor", s "width" "100%" ]
                [ div [ class "tile is-6" ]
                    [ div [ class "tile" ]
                        [ div [ class "tile is-parent is-vertical" ]
                            [ figure [ class "tile is-child image is-128x128" ]
                                [ img [ src ("https://spoonacular.com/cdn/ingredients_100x100/" ++ food.img) ] []
                                ]
                            , input [ class "tile is-child input", s "height" "15px", type_ "number", Html.Attributes.min "0", onInput (Input FoodAmountInput) ] []
                            ]
                        , div [ class "tile is-parent is-vertical" ]
                            [ h1 [ class "tile is-child title is-4", s "width" "100%" ] [ text "Nährwerte" ]
                            , div [ class "content" ]
                                [ table []
                                    [ tbody []
                                        [ tr [] [ td [] [ text "Kcal:" ], td [] [ text (String.fromFloat food.nutrition.kcal) ] ]
                                        , tr [] [ td [] [ text "Kohlenhydrate:" ], td [] [ text (String.fromFloat food.nutrition.carbs) ] ]
                                        , tr [] [ td [] [ text "Fett:" ], td [] [ text (String.fromFloat food.nutrition.fat) ] ]
                                        , tr [] [ td [] [ text "Eiweiß:" ], td [] [ text (String.fromFloat food.nutrition.protein) ] ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , modalFooter []
        ]



--Chart


type alias ChartSettings =
    { innerRadius : Float, corner : Float, position : Float }


pieSlice : ChartSettings -> Array Color -> Int -> Shape.Arc -> Svg msg
pieSlice settings colors index datum =
    Path.element (Shape.arc { datum | innerRadius = settings.innerRadius, cornerRadius = settings.corner }) [ fill <| Paint <| Maybe.withDefault Color.black <| Array.get index colors, stroke <| Paint Color.white ]


pieLabel : ChartSettings -> Shape.Arc -> ( String, Float ) -> Svg msg
pieLabel settings slice ( label, _ ) =
    let
        ( x, y ) =
            Shape.centroid { slice | innerRadius = settings.position, outerRadius = settings.position }
    in
    text_
        [ transform [ Translate x y ]
        , dy (em 0.25)
        , textAnchor AnchorMiddle
        , fill <| Paint <| Color.black
        ]
        [ text label ]


foodChart : List ( String, Float ) -> ChartSettings -> Array Color -> Svg msg
foodChart model settings colors =
    let
        pieData =
            model |> List.map Tuple.second |> Shape.pie { defaultPieConfig | outerRadius = 100 }
    in
    svg [ viewBox 0 0 200 200 ]
        [ g [ transform [ Translate 100 100 ] ]
            [ g [] <| List.indexedMap (pieSlice settings colors) pieData
            , g [] <| List.map2 (pieLabel settings) pieData model
            ]
        ]



---settingsView---


settingsView : Model -> Html Msg
settingsView model =
    div [ class "container" ]
        [ searchSettingsSection model
        , nutritionSettingsSection model
        ]


searchSettingsSection : Model -> Html Msg
searchSettingsSection model =
    div [ class "box" ]
        [ h1 [ class "subtitle" ] [ text "Sucheinstellung" ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Anzahl Suchergebnisse" ]
            , input [ class "input is-primary", type_ "number", Html.Attributes.min "1", placeholder "Anzahl Suchergebnisse", value model.settings.searchSettings.number, onInput (Input SearchNumberInput) ] []
            , label [ class "label" ] [ text "Sortieren nach" ]
            , div [ class "control" ]
                [ div [ class "select is-primary" ]
                    [ select [ onInput (Input SearchSortInput) ]
                        [ option [ Html.Attributes.value "calories" ] [ text "Kalorien" ]
                        , option [ Html.Attributes.value "total-fat" ] [ text "Fett" ]
                        , option [ Html.Attributes.value "carbs" ] [ text "Kohlenhydrate" ]
                        , option [ Html.Attributes.value "protein" ] [ text "Eiweiß" ]
                        ]
                    ]
                ]
            ]
        , label [ class "label" ] [ text "Sortierungsreihenfolge" ]
        , div [ class "control" ]
            [ div [ class "select is-primary" ]
                [ select [ onInput (Input SearchSortDirectionInput) ]
                    [ option [ Html.Attributes.value "desc" ] [ text "Absteigend" ]
                    , option [ Html.Attributes.value "asc" ] [ text "Aufsteigend" ]
                    ]
                ]
            ]
        ]



--, nutritionSettings : { kcalGoal : String, fatSplit : String, carbsSplit : String, proteinSplit : String }


nutritionSettingsSection : Model -> Html Msg
nutritionSettingsSection model =
    let
        value =
            Html.Attributes.value

        min =
            Html.Attributes.min

        max =
            Html.Attributes.max

        n =
            model.settings.nutritionSettings

        notificationClass =
            if
                (Maybe.withDefault 0 (String.toInt model.settings.nutritionSettings.fatSplit)
                    + (Maybe.withDefault 0 (String.toInt model.settings.nutritionSettings.carbsSplit)
                        + Maybe.withDefault 0 (String.toInt model.settings.nutritionSettings.proteinSplit)
                      )
                )
                    == 100
            then
                "notification is-primary"

            else
                "notification is-danger"
    in
    div [ class "box" ]
        [ h1 [ class "subtitle" ] [ text "Nährwerteinstellung" ]
        , div [ class "field" ]
            [ div []
                [ div [ class notificationClass ]
                    [ span [] [ text "Insgesamt " ]
                    , span []
                        [ text
                            (String.fromInt
                                (Maybe.withDefault 0 (String.toInt model.settings.nutritionSettings.fatSplit)
                                    + (Maybe.withDefault 0 (String.toInt model.settings.nutritionSettings.carbsSplit)
                                        + Maybe.withDefault 0 (String.toInt model.settings.nutritionSettings.proteinSplit)
                                      )
                                )
                                ++ "%:"
                            )
                        ]
                    , span [] [ text " Die Makronährstoffe müssen gleich 100% sein" ]
                    ]
                , label [ class "label" ] [ text "Fettanteil" ]
                , input
                    [ type_ "range"
                    , class "slider"
                    , placeholder "Anzahl Suchergebnisse"
                    , min "0"
                    , max "100"
                    , value n.fatSplit
                    , onInput (Input FatSliderInput)
                    ]
                    []
                , span
                    [ class "tag is-primary is-light"
                    , Html.Attributes.style "margin-left" "7.5px"
                    , Html.Attributes.style "width" "35px"
                    ]
                    [ text (model.settings.nutritionSettings.fatSplit ++ "%") ]
                ]
            , div []
                [ label [ class "label" ] [ text "Kohlenhydrateanteil" ]
                , input
                    [ type_ "range"
                    , class "slider"
                    , placeholder "Anzahl Suchergebnisse"
                    , min "0"
                    , max "100"
                    , value n.carbsSplit
                    , onInput (Input CarbsSliderInput)
                    ]
                    []
                , span
                    [ class "tag is-light is-primary "
                    , Html.Attributes.style "margin-left" "7.5px"
                    , Html.Attributes.style "width" "35px"
                    ]
                    [ text (model.settings.nutritionSettings.carbsSplit ++ "%") ]
                ]
            , div []
                [ label [ class "label" ] [ text "Eiweißanteil" ]
                , input
                    [ type_ "range"
                    , class "slider"
                    , placeholder "Anzahl Suchergebnisse"
                    , min "0"
                    , max "100"
                    , value n.proteinSplit
                    , onInput (Input ProteinSliderInput)
                    ]
                    []
                , span
                    [ class "tag is-primary is-light"
                    , Html.Attributes.style "margin-left" "7.5px"
                    , Html.Attributes.style "width" "35px"
                    ]
                    [ text (model.settings.nutritionSettings.proteinSplit ++ "%") ]
                ]
            , label [ class "label" ] [ text "Kalorienziel" ]
            , input [ class "input is-primary", type_ "number", Html.Attributes.min "0", placeholder "Kalorienziel", value n.kcalGoal, onInput (Input KcalInput) ] []
            ]
        ]



--Table


type TableType
    = FoodsList
    | SearchResults


foodTable : Model -> Html Msg
foodTable model =
    div [ class "table-container" ]
        [ table [ class "table is-striped is-hoverable", Html.Attributes.style "width" "100%" ]
            [ thead []
                [ tr []
                    [ th [ class "thead" ] [ text "Name" ]
                    , th [ class "thead" ] [ text "Menge" ]
                    , th [ class "thead" ] [ text "Kalorien" ]
                    , th [ class "thead" ] [ text "Kohlenhydrate" ]
                    , th [ class "thead" ] [ text "Fett" ]
                    , th [ class "thead" ] [ text "Eiweiß" ]
                    , th [ class "thead" ] []
                    ]
                ]
            , tbody [] (foodToListTable model.foods FoodsList)
            ]
        ]


searchResultsTable : Model -> Html Msg
searchResultsTable model =
    div [ class "table-container" ]
        [ table [ class "table is-striped is-hoverable", Html.Attributes.style "width" "75em" ]
            [ tbody [] (foodToSearchResultTable (map ingredientToFood model.response))
            ]
        ]



--Converter


foodToListTable : List Food -> TableType -> List (Html Msg)
foodToListTable foods tableType =
    case tableType of
        FoodsList ->
            List.indexedMap
                (\i food ->
                    tr [ class "table-row-hover-background-color", id ("row" ++ String.fromInt i) ]
                        [ td [] [ text food.name ]
                        , td [] [ text (Round.round 2 food.amount) ]
                        , td [] [ text (Round.round 2 food.nutrition.kcal) ]
                        , td [] [ text (Round.round 2 food.nutrition.carbs) ]
                        , td [] [ text (Round.round 2 food.nutrition.fat) ]
                        , td [] [ text (Round.round 2 food.nutrition.protein) ]
                        , td [] [ button [ class "delete", onClick (ChangeFoods (DeleteFood i)) ] [ text "löschen" ] ]
                        ]
                )
                foods

        SearchResults ->
            List.indexedMap
                (\i food ->
                    tr [ id ("row" ++ String.fromInt i) ]
                        [ td [] [ text food.name ]
                        ]
                )
                foods


foodToSearchResultTable : List Food -> List (Html Msg)
foodToSearchResultTable foodList =
    List.indexedMap
        (\i ingredient ->
            tr [ id ("row" ++ String.fromInt i), onClick (ChangeFoods (GetFoodData ingredient (String.fromInt ingredient.id))) ]
                [ td [] [ text ingredient.name ]
                ]
        )
        foodList


foodConverter : String -> Int -> String -> Float -> List Nutrient -> Food
foodConverter name id image amount nutrientList =
    let
        nutrition : Nutrition
        nutrition =
            List.foldl
                (\a b ->
                    case a.name of
                        "Calories" ->
                            { b | kcal = a.amount }

                        "Fat" ->
                            { b | fat = a.amount }

                        "Protein" ->
                            { b | protein = a.amount }

                        "Carbohydrates" ->
                            { b | carbs = a.amount }

                        _ ->
                            b
                )
                { kcal = 0.0, fat = 0.0, protein = 0.0, carbs = 0.0 }
                nutrientList
    in
    Food name id image amount nutrition


ingredientToFood : Ingredient -> Food
ingredientToFood ingredient =
    { name = ingredient.name, id = ingredient.id, img = ingredient.img, amount = 0.0, nutrition = { kcal = 0.0, protein = 0.0, fat = 0.0, carbs = 0.0 } }



-- HTTP Decoder


type alias HTTPSearchResults =
    { results : List Ingredient
    , offset : Int
    , number : Int
    , totalResults : Int
    }


resultDecoder : Json.Decode.Decoder HTTPSearchResults
resultDecoder =
    Json.Decode.map4 HTTPSearchResults
        (Json.Decode.field "results" (Json.Decode.list ingredientDecoder))
        (Json.Decode.field "offset" Json.Decode.int)
        (Json.Decode.field "number" Json.Decode.int)
        (Json.Decode.field "totalResults" Json.Decode.int)


ingredientDecoder : Json.Decode.Decoder Ingredient
ingredientDecoder =
    Json.Decode.map3 Ingredient
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "image" Json.Decode.string)


foodDecoder : Json.Decode.Decoder Food
foodDecoder =
    Json.Decode.map5 foodConverter
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "image" Json.Decode.string)
        (Json.Decode.field "amount" Json.Decode.float)
        (Json.Decode.field "nutrition" (Json.Decode.field "nutrients" (Json.Decode.list checkNutrientDecoder)))


checkNutrientDecoder : Json.Decode.Decoder Nutrient
checkNutrientDecoder =
    Json.Decode.field "name" Json.Decode.string |> Json.Decode.andThen nutrientDecoder


nutrientDecoder : String -> Json.Decode.Decoder Nutrient
nutrientDecoder nutrientType =
    Json.Decode.map2 Nutrient
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "amount" Json.Decode.float)



--Utils


ariaLabel : String -> Attribute msg
ariaLabel value =
    Html.Attributes.attribute "aria-label" value


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    Html.Events.on "keydown" (Json.Decode.map tagger Html.Events.keyCode)


setAmount : Food -> String -> Food
setAmount food amount =
    { food | amount = Maybe.withDefault 1.0 (String.toFloat amount) }


setNutrition : Food -> Food
setNutrition food =
    { food
        | nutrition =
            { kcal = food.nutrition.kcal * food.amount
            , protein = food.nutrition.protein * food.amount
            , fat = food.nutrition.fat * food.amount
            , carbs = food.nutrition.carbs * food.amount
            }
    }


setSearchSettings settings settingsType value =
    let
        s =
            settings.searchSettings
    in
    case settingsType of
        "number" ->
            { settings | searchSettings = { number = value, sort = s.sort, sortDirection = s.sortDirection } }

        "sort" ->
            { settings | searchSettings = { number = s.number, sort = value, sortDirection = s.sortDirection } }

        "direction" ->
            { settings | searchSettings = { number = s.number, sort = s.sort, sortDirection = value } }

        _ ->
            settings


setNutritionSettings : Settings -> String -> String -> Settings
setNutritionSettings settings settingsType value =
    let
        s =
            settings.nutritionSettings
    in
    case settingsType of
        "kcal" ->
            { settings
                | nutritionSettings =
                    { kcalGoal = value
                    , fatSplit = s.fatSplit
                    , carbsSplit = s.carbsSplit
                    , proteinSplit = s.proteinSplit
                    }
            }

        "fat" ->
            { settings
                | nutritionSettings =
                    { kcalGoal = s.kcalGoal
                    , fatSplit = value
                    , carbsSplit = s.carbsSplit
                    , proteinSplit = s.proteinSplit
                    }
            }

        "carbs" ->
            { settings
                | nutritionSettings =
                    { kcalGoal = s.kcalGoal
                    , fatSplit = s.fatSplit
                    , carbsSplit = value
                    , proteinSplit = s.proteinSplit
                    }
            }

        "protein" ->
            { settings
                | nutritionSettings =
                    { kcalGoal = s.kcalGoal
                    , fatSplit = s.fatSplit
                    , carbsSplit = s.carbsSplit
                    , proteinSplit = value
                    }
            }

        _ ->
            settings
