module Main exposing (..)

import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
--import Json.Encode as Encode


---- MODEL ----


type alias Model =
    { info: Maybe User }

type alias User =
    { description : Maybe String
    , facebookId : Maybe String
    , followeesCount : Maybe Int
    , followersCount : Maybe Int
    , gitHubLoginName : Maybe String
    , qiitaId : Maybe String
    , itemsCount : Maybe Int
    , linkedinId : Maybe Int
    , location : Maybe String
    , name : Maybe String
    , organization : Maybe String
    , permanentId : Maybe Int
    , profileImageUrl : Maybe String
    , twitterScreenName : Maybe String
    , websiteUrl : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { info = Nothing }, Cmd.none )



---- UPDATE ----
decodeUser : Decode.Decoder User
decodeUser =
    Decode.map2 ( \( f1, f2, f3, f4, f5, f6, f7, f8 ) ( f9, f10, f11, f12, f13, f14, f15 ) -> User f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 )
        ( Decode.map8 (,,,,,,,)
            ( Decode.maybe ( Decode.field "description" Decode.string ) )
            ( Decode.maybe ( Decode.field "facebook_id" Decode.string ) )
            ( Decode.maybe ( Decode.field "followees_count" Decode.int ) )
            ( Decode.maybe ( Decode.field "followers_count" Decode.int ) )
            ( Decode.maybe ( Decode.field "github_login_name" Decode.string ) )
            ( Decode.maybe ( Decode.field "qiita_id" Decode.string ) )
            ( Decode.maybe ( Decode.field "items_count" Decode.int ) )
            ( Decode.maybe ( Decode.field "linkedin_id" Decode.int ) )
        )
        ( Decode.map7 (,,,,,,)
            (Decode.maybe ( Decode.field "location" Decode.string ) )
            (Decode.maybe ( Decode.field "name" Decode.string ) )
            (Decode.maybe ( Decode.field "organization" Decode.string ) )
            (Decode.maybe ( Decode.field "permanent_id" Decode.int ) )
            (Decode.maybe ( Decode.field "profile_image_url" Decode.string ) )
            (Decode.maybe ( Decode.field "twitter_screen_name" Decode.string ) )
            (Decode.maybe ( Decode.field "website_url" Decode.string ) )
        )


type Msg
    = Request
    | GetUserResponse (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Request ->
            let
                user = decodeUser
                url = "https://qiita.com/api/v2/users/skht777"
                info = Http.send GetUserResponse <| (Http.get url user)
            in
                ( model, info )
        GetUserResponse res ->
            let
                val = case res of
                    Ok v -> v
                    Err _ -> Debug.crash ""
            in   
                ( { model | info = Just val }, Cmd.none )




---- VIEW ----
{--
toli: String -> Html Msg

```
createElement: User -> Html Msg
createElement user =
    ul []
       [
       ]
--}

view : Model -> Html Msg
view model =
    let
        user = case model.info of
            Just u -> text <| toString model.info
            Nothing -> text ""     
    in
        div []
            [ button[onClick Request][text "まあ押せ"]
            , div []
                  [ user ]
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



{--

-- HTTP Reqeust (認証なし)

-- http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Json-Decode

decodeHoge : Decode.Decoder Hoge
decodeHoge =
    Decode.map3 Hoge
        (Decode.at [ "id" ] Decode.int)
        (Decode.at [ "name" ] Decode.string)
        (Decode.at [ "job" ] Decode.string)

decodeHogeList : Decode.Decoder (List Hoge)
decodeHogeList =
    Decode.list decodeHoge


getHoge =
    let
       url = qiitaDomain ++ "/users/ababup1192"


    in
        Http.send msg <| (Http.get url decodeHogeList)


qiitaDomain =
    "https://qiita.com/api/v2/"


authHeader =
    Http.header "Authorization"
        "Bearer YOUR_TOKEN"



-- HTTP Request (認証あり)


getAuthHoge : Cmd Msg
getAuthHoge =
    let
        url =
            qiitaDomain ++ "/users/ababup1192"

        -- POSTする必要がある場合には、jsonを生成してください。
        -- jsonBody =
        --    Http.jsonBody <| json
        request =
            Http.request
                { method = "GET"
                , headers = [ authHeader ]
                , url = url

                -- , body = jsonBody
                , expect = Http.expectStringResponse (\_ -> Ok ())
                , timeout = Nothing
                , withCredentials = False
                }
    in
        -- http://package.elm-lang.org/packages/elm-lang/http/1.0.0/Http#send
        -- http://package.elm-lang.org/packages/elm-lang/core/latest/Result
        -- Cmd Msg (Result Http.Error String)
        Http.send msg request

 --}
