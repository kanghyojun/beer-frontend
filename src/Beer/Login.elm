module Beer.Login exposing (Model, Msg, empty, update, view)

{-| Login page.

@docs Model
@docs Msg
@docs empty
@docs update
@docs view

-}

import Html
import Html.Attributes as Attr
import Html.Events as Event
import Http
import Json.Decode as JsonDec


{-| Login model
-}
type alias Model =
    { typing : String
    , token : Maybe (Result String String)
    , login : Bool
    }


{-| Empty of Model
-}
empty : Model
empty =
    { typing = "", token = Nothing, login = False }


{-| Login page message
-}
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


{-| Login page update function
-}
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


{-| Login page view function
-}
view : Model -> Html.Html Msg
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


completeView : Html.Html Msg
completeView =
    Html.h1 [] [ Html.text "Complete" ]


isOk : Result x b -> Bool
isOk r =
    case r of
        Ok _ ->
            True

        _ ->
            False


loginView : Result String String -> Model -> Html.Html Msg
loginView err model =
    let
        blockTyping =
            model.login && isOk err
    in
    Html.div []
        [ Html.text
            (case err of
                Ok _ ->
                    ""

                Err e ->
                    e
            )
        , Html.input
            [ Attr.placeholder "Input phonenumber"
            , Attr.disabled blockTyping
            , Event.onInput Change
            ]
            []
        , Html.button
            [ Event.onClick Login, Attr.disabled blockTyping ]
            [ Html.text
                (if model.login then
                    "Try to login..."
                 else
                    "Login"
                )
            ]
        ]
