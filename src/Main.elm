port module Main exposing (main)

import Browser
import File exposing (File, toUrl)
import File.Select as Select
import Html exposing (Html, button, div, h2, input, label, li, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, id, style, value)
import Html.Events exposing (onClick, onInput)
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
    = ChooseFile (Maybe String)
    | Parsing
    | Parsed Data


init : () -> ( Model, Cmd Msg )
init _ =
    ( ChooseFile Nothing, Cmd.none )


type Msg
    = FontRequested
    | FontSelected File
    | SendFont String
    | FontLoaded (Result D.Error FontData)
    | ChangeFamily String
    | ChangeWeight String
    | ChangeMime String
    | ChangeExt String
    | Reset
    | Clear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FontRequested ->
            ( model
            , Select.file [] FontSelected
            )

        FontSelected file ->
            ( Parsing
            , Task.perform SendFont (File.toUrl file)
            )

        SendFont data ->
            ( Parsing
            , fontBinary data
            )

        FontLoaded data ->
            ( case data of
                Ok value ->
                    if value.fontExtension == "woff2" then
                        Parsed (Data value value Woff2 (Just "Can't parse family and weight for WOFF2 files."))

                    else
                        Parsed (Data value value Good (Just "Successfully parsed the font file."))

                Err err ->
                    ChooseFile (Just (D.errorToString err))
            , Cmd.none
            )

        ChangeFamily val ->
            ( case model of
                Parsed state ->
                    let
                        currentState =
                            state.current
                    in
                    Parsed (Data state.original { currentState | fontFamily = val } Good Nothing)

                _ ->
                    ChooseFile (Just "Oops")
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
                    ChooseFile (Just "Oops")
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
                    ChooseFile (Just "Oops")
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
                    ChooseFile (Just "Oops")
            , Cmd.none
            )

        Reset ->
            ( case model of
                Parsed state ->
                    Parsed (Data state.original state.original Good (Just "Settings reset."))

                _ ->
                    ChooseFile (Just "Oops")
            , Cmd.none
            )

        Clear ->
            ( ChooseFile Nothing
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
    | Woff2



-- VIEW


errorView : Maybe String -> Html Msg
errorView maybeError =
    case maybeError of
        Just err ->
            p [] [ text err ]

        Nothing ->
            text ""


formatFontString : FontData -> String
formatFontString encoded =
    String.join ""
        [ "font-family:"
        , encoded.fontFamily ++ ";"
        , "src:url(data:"
        , encoded.fontMime ++ ";"
        , "charset=utf-8;base64,"
        , encoded.base64 ++ ")"
        , "format('"
        , encoded.fontExtension ++ "')"
        , "font-weight:"
        , encoded.fontWeight ++ ";"
        ]


statusClass : Status -> String
statusClass status =
    case status of
        Good ->
            "status good"

        Woff2 ->
            "status woff2"


parsedMessage : Data -> Html Msg
parsedMessage data =
    if data.message /= Nothing then
        div [ class (statusClass data.status) ]
            [ span []
                [ text
                    (case data.message of
                        Just string ->
                            string

                        Nothing ->
                            ""
                    )
                ]
            ]

    else
        text ""


view : Model -> Html Msg
view model =
    case model of
        ChooseFile err ->
            div []
                [ h2 [] [ text "Choose a font to embed" ]
                , p [] [ text "Supports WOFF, WOFF2, TTF, and OTF fonts" ]
                , p [] [ text "Version 0.1" ]
                , button
                    [ onClick FontRequested
                    , class "primary"
                    ]
                    [ text "Load Font" ]
                , errorView err
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
    parsedFont (FontLoaded << D.decodeValue fontDecoder)