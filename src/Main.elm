module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JsonDec


type alias Model =
    { typing : String
    , token : Maybe (Result String String)
    , login : Bool
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { typing = "", token = Nothing, login = False }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GetToken (Result Http.Error String)
    | Change String
    | Login


httpErrorToString : Http.Error -> String
httpErrorToString e =
    case e of
        Http.BadUrl b ->
            "bad url" ++ b

        Http.BadPayload d r ->
            "bad" ++ d ++ r.body

        Http.NetworkError ->
            "network"

        Http.BadStatus r ->
            "bad status " ++ r.body

        _ ->
            "Unexpected error occured"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetToken (Ok token) ->
            ( { model | token = Just (Ok token) }, Cmd.none )

        GetToken (Err e) ->
            ( { model
                | token = Just (Err (httpErrorToString e))
                , login = False
              }
            , Cmd.none
            )

        Change s ->
            ( { model
                | typing = s
                , login = False
                , token = Nothing
              }
            , Cmd.none
            )

        Login ->
            ( { model | login = True }, requestLogin model.typing )


requestLogin : String -> Cmd Msg
requestLogin phonenumber =
    let
        url =
            "http://127.0.0.1:5000/login/?phonenumber=" ++ phonenumber
    in
    Http.send GetToken (Http.get url decodeToken)


decodeToken : JsonDec.Decoder String
decodeToken =
    JsonDec.at [ "token" ] JsonDec.string



-- VIEW


view : Model -> Html Msg
view model =
    case model.token of
        Nothing ->
            loginView (Ok "") model

        Just r ->
            case r of
                Ok r ->
                    completeView

                e ->
                    loginView e model


completeView : Html Msg
completeView =
    h1 [] [ text "Complete" ]


isOk : Result x b -> Bool
isOk r =
    case r of
        Ok _ ->
            True

        _ ->
            False


loginView : Result String String -> Model -> Html Msg
loginView err model =
    let
        blockTyping =
            model.login && isOk err
    in
    div []
        [ text
            (case err of
                Ok _ ->
                    ""

                Err e ->
                    e
            )
        , input
            [ placeholder "Input phonenumber"
            , disabled blockTyping
            , onInput Change
            ]
            []
        , button
            [ onClick Login, disabled blockTyping ]
            [ text
                (if model.login then
                    "Try to login..."
                 else
                    "Login"
                )
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
