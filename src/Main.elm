port module Main exposing (main)

import Browser
import Bytes.Encode
import Base64
import File exposing (File, toUrl)
import File.Select as Select
import Html exposing (Html, button, div, h2, input, label, li, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, id, style, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (get)
import Json.Decode as D
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = ChooseFile { message : Maybe String, url : Maybe String }
    | Parsing
    | Parsed Data


init : () -> ( Model, Cmd Msg )
init _ =
    ( ChooseFile { message = Nothing, url = Nothing}, Cmd.none )


type Msg
    = FatalError String
    | FontUpload
    | FontSelected File
    | FontRequest String
    | ChangeUrl { message : Maybe String, url : Maybe String }
    | FontResponse (Result Http.Error String)
    | SendFont String
    | FontLoaded (Result D.Error FontData)
    | ChangeFamily String
    | ChangeWeight String
    | ChangeMime String
    | ChangeExt String
    | NoOp
    | Reset
    | Clear


byteStringToBase64 : String -> Maybe String
byteStringToBase64 string =
    Bytes.Encode.string string
        |> Bytes.Encode.encode
        |> Base64.fromBytes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FatalError message ->
            ( ChooseFile { message = Just message, url = Nothing}
            , Cmd.none
            )

        FontUpload ->
            ( model
            , Select.file [] FontSelected
            )

        FontSelected file ->
            ( Parsing
            , Task.perform SendFont
                <| File.toUrl file
            )

        FontRequest url ->
            ( Parsing
            , Http.get
                { url = url
                , expect = Http.expectString FontResponse
                }
            )
        
        ChangeUrl message ->
            ( ChooseFile message
            , Cmd.none
            )

        FontResponse result ->
            case result of
                Ok value ->
                    let
                        stringBase64 = byteStringToBase64 value
                    in
                        case stringBase64 of
                            Just string ->
                                ( Parsing
                                , Task.perform SendFont
                                    <| Task.succeed string
                                )

                            Nothing ->
                                ( ChooseFile { message = Just "oops", url = Nothing }
                                , Cmd.none
                                )
                
                Err _ ->
                    ( ChooseFile { message = Just "http error", url = Nothing }
                    , Cmd.none
                    )

        SendFont data ->
            ( Parsing
            , fontBinary data
            )

        FontLoaded data ->
            ( case data of
                Ok value ->
                    if value.fontExtension == "woff2" then
                        Parsed (Data value value Warn (Just "Can't parse family and weight for WOFF2 files."))

                    else
                        Parsed (Data value value Good (Just "Successfully parsed the font file."))

                Err err ->
                    ChooseFile {message = Just (D.errorToString err), url = Nothing }
            , Cmd.none
            )

        ChangeFamily val ->
            ( case model of --replace with just model?
                Parsed state ->
                    let
                        currentState =
                            state.current
                    in
                    Parsed (Data state.original { currentState | fontFamily = val } Good Nothing)

                _ ->
                    ChooseFile { message = Just "oops", url = Nothing }
            , Cmd.none
            )

        ChangeWeight val ->
            ( case model of
                Parsed state ->
                    let
                        currentState =
                            state.current
                    in
                    Parsed (Data state.original { currentState | fontWeight = val } Good Nothing)

                _ ->
                    ChooseFile { message = Just "oops", url = Nothing }
            , Cmd.none
            )

        ChangeMime val ->
            ( case model of
                Parsed state ->
                    let
                        currentState =
                            state.current
                    in
                    Parsed (Data state.original { currentState | fontMime = val } Good Nothing)

                _ ->
                    ChooseFile { message = Just "oops", url = Nothing }
            , Cmd.none
            )

        ChangeExt val ->
            ( case model of
                Parsed state ->
                    let
                        currentState =
                            state.current
                    in
                    Parsed (Data state.original { currentState | fontExtension = val } Good Nothing)

                _ ->
                    ChooseFile { message = Just "oops", url = Nothing }
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )

        Reset ->
            ( case model of
                Parsed state ->
                    Parsed (Data state.original state.original Good (Just "Settings reset."))

                _ ->
                    ChooseFile { message = Just "oops", url = Nothing }
            , Cmd.none
            )

        Clear ->
            ( ChooseFile { message = Nothing, url = Nothing }
            , Cmd.none
            )


type alias FontData =
    { fontFamily : String
    , fontWeight : String
    , fontMime : String
    , fontExtension : String
    , base64 : String
    }


type alias Data =
    { original : FontData
    , current : FontData
    , status : Status
    , message : Maybe String
    }


type Status
    = Good
    | Warn
    | Error



-- VIEW


viewStatusMessage : String -> Status -> Html Msg
viewStatusMessage message status =
    div [ class (statusClass status) ]
        [ span [] [ text message ] ]

--Need to properly handle font-style (italic) and font-weight (bold)
formatFontString : FontData -> String
formatFontString encoded =
    String.join ""
        [ "font-family:" ++ encoded.fontFamily ++ ";"
        , "src:url(data:" ++ encoded.fontMime ++ ";"
        , "charset=utf-8;base64," ++ encoded.base64 ++ ")"
        , "format('" ++ encoded.fontExtension ++ "')"
        , formatFontWeight encoded.fontWeight
        ]

formatFontWeight : String -> String
formatFontWeight input =
    if input == "" then
        ""
    else
        "font-weight:" ++ input ++ ";"


statusClass : Status -> String
statusClass status =
    case status of
        Good ->
            "status good"

        Warn ->
            "status warn"

        Error ->
            "status error"


parsedMessage : Data -> Html Msg
parsedMessage data =
    case data.message of
        Just message ->
            viewStatusMessage message data.status

        Nothing ->
            text ""

submitFont : Maybe String -> Msg
submitFont msg =
    case msg of
        Just string ->
            FontRequest string

        Nothing ->
            NoOp

urlForm : { message : Maybe String, url : Maybe String } -> Html Msg
urlForm message =
    div [ ]
        [ label []
            [ text "Load font from URL"
            , input [ onInput (\v -> ChangeUrl { message | url = Just v})] []
            ]
        , button [ onClick (submitFont message.url) ] [ text "Get font from URL" ]
        ]

view : Model -> Html Msg
view model =
    case model of
        ChooseFile message ->
            div []
                [ case message.message of
                    Just _ ->
                        viewStatusMessage
                            """Something went wrong.
                            Perhaps the file was not a font file or it was corrupted."""
                            Error

                    Nothing ->
                        text ""
                , h2 [] [ text "Choose a font to embed" ]
                , p [] [ text "Supports WOFF, WOFF2, TTF, and OTF fonts" ]
                , p [] [ text "Version 0.1" ]
                , button
                    [ onClick FontUpload
                    , class "primary"
                    ]
                    [ text "Load font" ]
                , urlForm message
                ]

        Parsing ->
            div [ id "loader" ]
                [ div [ class "lds-dual-ring" ] []
                , span [] [ text "Parsing font..." ]
                ]

        Parsed data ->
            p [ style "white-space" "pre" ]
                [ parsedMessage data
                , ul []
                    [ li []
                        [ label [] [ text "Font family" ]
                        , input [ onInput ChangeFamily, value data.current.fontFamily ] []
                        ]
                    , li []
                        [ label [] [ text "Font weight" ]
                        , input [ onInput ChangeWeight, value data.current.fontWeight ] []
                        ]
                    , li []
                        [ label [] [ text "MIME type" ]
                        , input [ onInput ChangeMime, value data.current.fontMime ] []
                        ]
                    , li []
                        [ label [] [ text "Format" ]
                        , input [ onInput ChangeExt, value data.current.fontExtension ] []
                        ]
                    ]
                , div []
                    [ label [] [ text "Copy this into Axure" ]
                    , textarea
                        [ id "output"
                        , style "display" "block"
                        , value (formatFontString data.current)
                        ]
                        []
                    ]
                , button
                    [ id "copy"
                    , class "primary"
                    , attribute "data-clipboard-action" "copy"
                    , attribute "data-clipboard-target" "#output"
                    ]
                    [ text "Copy to clipboard" ]
                , button [ onClick Reset ] [ text "Reset" ]
                , button [ onClick Clear ] [ text "Start over" ]
                ]



-- PORTS


port parsedFont : (D.Value -> msg) -> Sub msg


port fontBinary : String -> Cmd msg


port parseError : (String -> msg) -> Sub msg



-- DECODE


fontDecoder : D.Decoder FontData
fontDecoder =
    D.map5 FontData
        (D.field "fontFamily" D.string)
        (D.field "fontWeight" D.string)
        (D.field "fontMime" D.string)
        (D.field "fontExtension" D.string)
        (D.field "base64" D.string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ parsedFont (FontLoaded << D.decodeValue fontDecoder)
        , parseError FatalError
        ]
