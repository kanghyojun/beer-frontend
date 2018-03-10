module Beer
    exposing
        ( Model
        , Msg
        , init
        , main
        , subscriptions
        , update
        , view
        )

import Beer.Login as Login
import Html
import Navigation
import Route


type alias Model =
    { login : Login.Model
    , route : Maybe Route.Route
    }


type Msg
    = RouteChanged Navigation.Location
    | LoginMsg Login.Msg


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { route = Route.createRoute location, login = Login.empty }
    , Cmd.none
    )


main : Program Never Model Msg
main =
    Navigation.program RouteChanged
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoginMsg loginMsg ->
            let
                ( newLoginModel, cmd ) =
                    Login.update loginMsg model.login
            in
            ( { model | login = newLoginModel }
            , Cmd.none
            )

        RouteChanged r ->
            ( { model | route = Route.createRoute r }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    case model.route of
        _ ->
            Html.map (\m -> LoginMsg m) <|
                Login.view model.login


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
