module Main exposing (main, view)

--import Http
--import Json.Decode exposing (Decoder, andThen, field, int, list, map, map2, map3, map4, string)
--import String
--import Svg
--import Svg.Attributes as SvgAttr
--import Svg.Events

import Browser
import Browser.Navigation as Nav
import Html exposing (Attribute, Html, a, button, div, figure, footer, h1, header, img, input, nav, p, section, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, id, placeholder, src, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (index)
import List exposing (append, drop, indexedMap, length, map, map2, range, take)
import Svg.Attributes exposing (offset, style)
import Url


main : Program () { key : Nav.Key, url : Url.Url, page : Page, modal : Maybe ModalState, foods : List Food, searchTerm : String, response : List Ingredient, errorMsg : String } Msg
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
    , page : Page
    , modal : Maybe ModalState
    , foods : List Food
    , searchTerm : String
    , response : List Ingredient
    , errorMsg : String
    }


type Page
    = Home
    | Liste
    | Suche


type ModalState
    = ShowFood Food


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


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , page = Home
      , modal = Nothing
      , foods =
            [ { name = "Ei", id = 1, img = "", amount = 200.0, nutrition = { kcal = 200.0, protein = 200.0, fat = 200.0, carbs = 200.0 } }
            , { name = "Brot", id = 2, img = "", amount = 100.0, nutrition = { kcal = 100.0, protein = 100.0, fat = 100.0, carbs = 100.0 } }
            , { name = "Arne", id = 3, img = "", amount = 100.0, nutrition = { kcal = 100.0, protein = 100.0, fat = 100.0, carbs = 100.0 } }
            ]
      , searchTerm = ""
      , response = [ { name = "banana", id = 123, img = "banana.png" } ]
      , errorMsg = ""
      }
    , Cmd.none
    )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | OpenModal ModalMsg
    | CloseModal
    | ChangeFoods FoodMsg
    | GotFoods (Result Http.Error HTTPSearchResults)
    | GotFoodData (Result Http.Error Food)
    | Input String
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

                        _ ->
                            Home
              }
            , Cmd.none
            )

        OpenModal modalMsg ->
            updateModal modalMsg model

        CloseModal ->
            ( { model | modal = Nothing }, Cmd.none )

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
                    ( {model | modal = Just (ShowFood result)}, Cmd.none  )

                Err _ ->
                    ( { model | errorMsg = "Informationen werden nicht abgeholt" }, Cmd.none )

        Input input ->
            ( { model | searchTerm = input }, Cmd.none )

        KeyDown key ->
            if key == 13 then
                updateFood (GetFoods model.searchTerm) model

            else
                ( model, Cmd.none )


type FoodMsg
    = GetFoods String
    | GetFoodData String
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
                , url = "https://api.spoonacular.com/food/ingredients/search?query=" ++ food ++ "&number=10&sort=calories&sortDirection=desc&apiKey=a25e9078614b4a79948065747e2cc8cf"
                , body = Http.emptyBody
                , expect = Http.expectJson GotFoods resultDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        GetFoodData id ->
            ( model
            , Http.request
                { method = "GET"
                , headers = []

                {- , url = "https://api.spoonacular.com/food/ingredients/" ++ id ++ "/information?amount=1" -}
                , url = "../json/foodData.json"
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
            , Http.request
                { method = "GET"
                , headers = []

                {- , url = "https://api.spoonacular.com/food/ingredients/" ++ id ++ "/information?amount=1" -}
                , url = "../json/foodData.json"
                , body = Http.emptyBody
                , expect = Http.expectJson GotFoodData foodDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "MyApp"
    , body =
        [ div []
            [ navbar model
            , pageContent model
            ]
        ]
    }


navbar : Model -> Html Msg
navbar model =
    nav [ class "navbar is-primary" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item" ]
                [ h1 [ class "tile" ] [ text "Kalorientracker" ]
                ]
            ]
        , div [ class "navbar-menu" ]
            [ div [ class "navbar-start" ]
                [ a [ class "navbar-item", href "#home" ] [ text "Home" ]
                , a [ class "navbar-item", href "#search" ] [ text "Suche" ]
                , a [ class "navbar-item", href "#list" ] [ text "Liste" ]
                ]
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
                        [ div [ class "control" ] [ input [ class "input", type_ "text", placeholder "Suchen...", onInput Input, onKeyDown KeyDown, Html.Attributes.style "width" "75em" ] [] ]
                        , div [ class "control" ] [ button [ class "button is-primary", onClick (ChangeFoods (GetFoods model.searchTerm)) ] [ text "Suchen" ] ]
                        ]
                    , searchResultsTable model
                    , viewModal model
                    ]
                ]


viewModal : Model -> Html Msg
viewModal model =
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
            ++ [ button [ class "button is-primary", onClick CloseModal ] [ text "Schließen" ]
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
                            , input [ class "tile is-child input", type_ "number", placeholder "Suchen...", onInput Input ] []
                            ]
                        , div [ class "tile is-parent is-vertical" ]
                            [ h1 [ class "tile is-child title is-4", Html.Attributes.style "width" "100%" ] [ text "Nährwerte" ]
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
                        , td [] [ text (String.fromFloat food.amount) ]
                        , td [] [ text (String.fromFloat food.nutrition.kcal) ]
                        , td [] [ text (String.fromFloat food.nutrition.carbs) ]
                        , td [] [ text (String.fromFloat food.nutrition.fat) ]
                        , td [] [ text (String.fromFloat food.nutrition.protein) ]
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
            tr [ id ("row" ++ String.fromInt i), onClick (OpenModal (OpenFood ingredient 3)) ]
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
