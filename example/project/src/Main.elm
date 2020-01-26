port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ports exposing (..)


port alertString : String -> Cmd msg


main : Program Model Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    Int


init : Model -> ( Model, Cmd Msg )
init flags =
    ( flags, Cmd.none )


type Msg
    = AlertString String
    | AlertInt Int
    | AlertBool Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AlertString str ->
            ( model, alertString str )

        AlertInt int ->
            ( model, alertInt int )

        AlertBool bool ->
            ( model, alertBool bool )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick <| AlertString "string" ] [ text "alert string" ]
        , button [ onClick <| AlertInt 114514 ] [ text "alert 114514" ]
        , button [ onClick <| AlertBool True ] [ text "alert true" ]
        ]
