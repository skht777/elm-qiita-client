module Main exposing (..)

import Html exposing (Html, a, button, div, img, input, li, p, text, ul)
import Html.Attributes exposing (href, src, style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder)


--import Json.Encode as Encode
---- MODEL ----


type alias Model =
    { target : Id, searched : Bool, info : Maybe User }


type alias Id =
    String


type alias User =
    { description : Maybe String
    , facebookId : Maybe String
    , followeesCount : Maybe Int
    , followersCount : Maybe Int
    , gitHubLoginName : Maybe String
    , qiitaId : Maybe String
    , itemsCount : Maybe Int
    , linkedinId : Maybe String
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
    ( { target = "skht777", searched = False, info = Nothing }, Cmd.none )



---- UPDATE ----


safeDecode : String -> Decoder a -> Decoder (Maybe a)
safeDecode field t =
    Decode.maybe (Decode.field field t)


safeDecodeString : String -> Decoder (Maybe String)
safeDecodeString field =
    safeDecode field Decode.string


safeDecodeInt : String -> Decoder (Maybe Int)
safeDecodeInt field =
    safeDecode field Decode.int


decodeUser : Decode.Decoder User
decodeUser =
    Decode.map2 (\( f1, f2, f3, f4, f5, f6, f7, f8 ) ( f9, f10, f11, f12, f13, f14, f15 ) -> User f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15)
        (Decode.map8 (,,,,,,,)
            (safeDecodeString "description")
            (safeDecodeString "facebook_id")
            (safeDecodeInt "followees_count")
            (safeDecodeInt "followers_count")
            (safeDecodeString "github_login_name")
            (safeDecodeString "id")
            (safeDecodeInt "items_count")
            (safeDecodeString "linkedin_id")
        )
        (Decode.map7 (,,,,,,)
            (safeDecodeString "location")
            (safeDecodeString "name")
            (safeDecodeString "organization")
            (safeDecodeInt "permanent_id")
            (safeDecodeString "profile_image_url")
            (safeDecodeString "twitter_screen_name")
            (safeDecodeString "website_url")
        )


type Msg
    = InputId Id
    | Request
    | GetUserResponse (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputId s ->
            ( { model | target = s }, Cmd.none )

        Request ->
            let
                user =
                    decodeUser

                url =
                    "https://qiita.com/api/v2/users/" ++ model.target

                info =
                    Http.send GetUserResponse <| Http.get url user
            in
            ( { model | searched = True }, info )

        GetUserResponse res ->
            let
                val =
                    case res of
                        Ok v ->
                            Just v

                        Err _ ->
                            Nothing
            in
            ( { model | info = val }, Cmd.none )



---- VIEW ----


makeLi : Html Msg -> Html Msg
makeLi e =
    li [] [ e ]


unwrap : Maybe String -> String
unwrap me =
    Maybe.withDefault "" me


unwrapInt : Maybe Int -> String
unwrapInt me =
    unwrap <| Maybe.map toString me


mapText : (Maybe a -> String) -> Maybe a -> Html Msg
mapText f me =
    text <| f me


mapLink : String -> String -> Maybe String -> Html Msg
mapLink root s me =
    Maybe.map (\name -> a [ href <| root ++ name ] [ p [] [ text <| s ++ " " ++ name ] ]) me
        |> Maybe.withDefault (text "")


createElement : User -> Html Msg
createElement user =
    ul [] <|
        List.map makeLi
            [ mapText unwrap user.description
            , mapLink "https://www.facebook.com/" "Facebook" user.facebookId
            , mapText unwrapInt user.followeesCount
            , mapText unwrapInt user.followersCount
            , mapLink "https://github.com/" "GitHub" user.gitHubLoginName
            , mapLink "https://qiita.com/" "Qiita" user.qiitaId
            , mapText unwrapInt user.itemsCount
            , mapLink "https://jp.linkedin.com/" "LinkdIn" user.linkedinId
            , mapText unwrap user.location
            , mapText unwrap user.name
            , mapText unwrap user.organization
            , mapText unwrapInt user.permanentId
            , img
                [ style [ ( "verticalAlign", "middle" ) ]
                , src <| unwrap user.profileImageUrl
                ]
                []
            , mapLink "https://twitter.com/" "Twitter" user.twitterScreenName
            , mapLink "" "WebSite" user.websiteUrl
            ]


view : Model -> Html Msg
view model =
    let
        user =
            case model.info of
                Just u ->
                    createElement u

                Nothing ->
                    text <|
                        if model.searched then
                            "ユーザが存在しません"
                        else
                            ""
    in
    div []
        [ input [ value model.target, onInput InputId ] []
        , button [ onClick Request ] [ text "検索" ]
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
