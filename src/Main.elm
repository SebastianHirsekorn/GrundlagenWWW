module Main exposing (main, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (Attribute, Html, a, button, div, figure, footer, h1, h3, header, img, input, label, nav, option, p, section, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, id, placeholder, selected, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (index)
import List exposing (append, drop, indexedMap, length, map, map2, range, take)
import Round
import Svg.Attributes exposing (offset, style)
import Time
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



{- type Slider
   = FatSlider
   | CarbsSlider
   | ProteinSlider
-}
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
      , page = Home
      , modal = Nothing
      , popUp = Nothing
      , foods = []
      , currNutrition = { kcal = 0.0, protein = 0.0, fat = 0.0, carbs = 0.0 }
      , searchTerm = ""
      , response = [ { name = "banana", id = 123, img = "banana.png" } ]
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
            (  { model | counter = model.counter - 1, popUp =  if model.counter >= 0 then Just FoodAdded else Nothing
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
            ( { model | foods = append (take i model.foods) (drop (i + 1) model.foods) }, Cmd.none )

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
              in
              { model
                | foods =
                    List.append model.foods
                        [ setNutrition food
                        ]
                , currNutrition =
                    { kcal = model.currNutrition.kcal + food.nutrition.kcal
                    , protein = model.currNutrition.protein + food.nutrition.protein
                    , fat = model.currNutrition.fat + food.nutrition.fat
                    , carbs = model.currNutrition.carbs + food.nutrition.carbs
                    }
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

                {- , url = "../json/foodData.json" -}
                , body = Http.emptyBody
                , expect = Http.expectJson GotFoodData foodDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )


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
    { title = "MyApp"
    , body =
        [ div []
            [ navbar model
            , pageContent model
            , button [ onClick TogglePopUp ] [ text "toggle" ]
            , popup model
            ]
        ]
    }


navbar : Model -> Html Msg
navbar model =
    nav [ class "navbar is-primary" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item" ]
                [ h1 [ class "tile" ] [ text "Nutritiontracker" ]
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
            section [ class "section" ] [ h1 [] [ text "Home" ] ]

        Liste ->
            section [ class "section" ]
                [ foodTable model
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
    div [ class "modal-card", Html.Attributes.style "width" "50em" ]
        [ modalHeader food.name
        , div [ class "modal-card-body" ]
            [ div [ class "tile is-ancestor", Html.Attributes.style "width" "100%" ]
                [ div [ class "tile is-6" ]
                    [ div [ class "tile" ]
                        [ div [ class "tile is-parent is-vertical" ]
                            [ figure [ class "tile is-child image is-128x128" ]
                                [ img [ src ("https://spoonacular.com/cdn/ingredients_100x100/" ++ food.img) ] []
                                ]
                            , input [ class "tile is-child input", type_ "number", onInput (Input FoodAmountInput) ] []
                            ]
                        , div [ class "tile is-parent is-vertical" ]
                            [ h1 [ class "tile is-child title is-4", Html.Attributes.style "width" "100%" ] [ text "Nährwerte" ]
                            , div [ class "content" ]
                                [ table []
                                    [ tbody []
                                        [ tr [] [ td [] [ text "Kcal:" ], td [] [ text (String.fromFloat (food.nutrition.kcal * food.amount)) ] ]
                                        , tr [] [ td [] [ text "Kohlenhydrate:" ], td [] [ text (String.fromFloat (food.nutrition.carbs * food.amount)) ] ]
                                        , tr [] [ td [] [ text "Fett:" ], td [] [ text (String.fromFloat (food.nutrition.fat * food.amount)) ] ]
                                        , tr [] [ td [] [ text "Eiweiß:" ], td [] [ text (String.fromFloat (food.nutrition.protein * food.amount)) ] ]
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
            , input [ class "input is-primary", type_ "number", placeholder "Anzahl Suchergebnisse", value model.settings.searchSettings.number, onInput (Input SearchNumberInput) ] []
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
                    [ span [] [ text "Gesamt-%" ]
                    , span []
                        [ text
                            (String.fromInt
                                (Maybe.withDefault 0 (String.toInt model.settings.nutritionSettings.fatSplit)
                                    + (Maybe.withDefault 0 (String.toInt model.settings.nutritionSettings.carbsSplit)
                                        + Maybe.withDefault 0 (String.toInt model.settings.nutritionSettings.proteinSplit)
                                      )
                                )
                                ++ "%"
                            )
                        ]
                    , span [] [ text "Die Makronährstoffe müssen gleich 100% sein" ]
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
            , input [ class "input is-primary", type_ "number", placeholder "Kalorienziel", value n.kcalGoal, onInput (Input KcalInput) ] []
            ]
        ]



--Table


type TableType
    = FoodsList
    | SearchResults


foodTable : Model -> Html Msg
foodTable model =
    div [ class "table-container" ]
        [ table [ class "table is-striped is-hoverable" ]
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
