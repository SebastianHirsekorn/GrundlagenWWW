module Home exposing (view)

--import Http
--import Json.Decode exposing (Decoder, andThen, field, int, list, map, map2, map3, map4, string)
--import String
--import Svg
--import Svg.Attributes as SvgAttr
--import Svg.Events

--hi sebastian

import Browser
import Browser.Navigation as Nav
import Html exposing (Attribute, Html, a, button, div, h1, input, nav, section, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, id, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import List exposing (append, drop, indexedMap, length, map, map2, range, take)


type alias Model =
    { page : Page
    , foods : List Food
    , response : List Food
    }


type Page
    = Home
    | Liste
    | Suche


type alias Food =
    { name : String, amount : Float, kcal : Float, protein : Float, fat : Float, carbs : Float }


type Msg
    = ChangeFoods FoodMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeFoods foodMsg ->
            updateFood foodMsg model


type FoodMsg
    = DeleteFood Int


updateFood : FoodMsg -> Model -> ( Model, Cmd Msg )
updateFood foodMsg model =
    case foodMsg of
        DeleteFood i ->
            ( { model | foods = append (take i model.foods) (drop (i + 1) model.foods) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ section [ class "section" ] [ h1 [] [ text "Home" ] ]
        ]


--- ARNE