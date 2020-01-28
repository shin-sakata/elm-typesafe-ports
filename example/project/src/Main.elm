port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Ports exposing (alertInt)
import SubDir.Ports2 exposing (alertBool)
import SubDir.Ports3 exposing (alertFloat)


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
    | AlertFloat Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AlertString str ->
            ( model, alertString str )

        AlertInt int ->
            ( model, alertInt int )

        AlertBool bool ->
            ( model, alertBool bool )

        AlertFloat float ->
            ( model, alertFloat float )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view _ =
    div []
        [ button [ onClick <| AlertString "string" ] [ text "alert string" ]
        , button [ onClick <| AlertInt 114514 ] [ text "alert 114514" ]
        , button [ onClick <| AlertBool True ] [ text "alert true" ]
        , button [ onClick <| AlertFloat 114.514 ] [ text "alert 114.514" ]
        ]
