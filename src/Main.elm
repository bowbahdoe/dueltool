module Main exposing (main)

import Browser
import Browser.Events
import Coin exposing (Coin(..))
import Die exposing (Die)
import Duel exposing (Duel, getLife, updatePlayer)
import Element exposing (Element, alpha, centerX, centerY, column, el, fill, fillPortion, height, padding, paddingXY, px, rgb255, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (button)
import History exposing (History)
import InputState exposing (InputState, NumericInputButton(..))
import Json.Decode as Decode
import Json.Encode exposing (Value)
import LifePoints exposing (LifePoints)
import Maybe
import Player exposing (Player, PlayerId(..))
import Ports
import Random



-- Model


evolve : (a -> a) -> History a -> History a
evolve f history =
    History.to (f (History.current history)) history


type DisplayableResult
    = DieRoll Die
    | CoinFlip Coin


type alias Model =
    { duelHistory : History Duel
    , inputState : InputState

    -- If the user has rolled a die or flipped a coin we want to show them the result of that
    -- action.
    , result : Maybe DisplayableResult
    }

init : Value -> ( Model, Cmd msg )
init flags =
    case Decode.decodeValue (Decode.field "storedDuel" (Decode.nullable Duel.decode)) flags of
        Ok (Just duel) ->
            ( initialModel { previousDuel = Just duel }
            , Ports.info "Loaded in previous duel from flags."
            )

        Ok Nothing ->
            ( initialModel { previousDuel = Nothing }
            , Ports.info "There was no previous duel. Starting from scratch."
            )

        Err _ ->
            ( initialModel { previousDuel = Nothing }
            , Ports.error "The previous duel was unable to be decoded. Starting from scratch."
            )


initialModel : { a | previousDuel : Maybe Duel } -> Model
initialModel { previousDuel } =
    { duelHistory =
        History.new
            (case previousDuel of
                Just duel ->
                    duel

                Nothing ->
                    Duel.default
            )
    , inputState = InputState.init
    , result = Nothing
    }


{-| In YuGiOh life point changes that aren't 50 + some multiple of 100 are
almost non-existent. As a shortcut for players we can scale up a small
number to it's closest multiple of 100.
-}
scaleUpSmallLifeChanges : Int -> Int
scaleUpSmallLifeChanges n =
    if abs n < 100 && abs n /= 50 then
        n * 100

    else
        n


submitLifePointChange : Model -> Model
submitLifePointChange model =
    let
        { inputState, duelHistory } =
            model

        duel =
            History.current duelHistory

        ( newInputState, newDuel ) =
            case InputState.lifeChangeIndicated model.inputState of
                Just { change, forPlayer } ->
                    ( inputState |> InputState.removePlayerSelection |> InputState.clearChangeInput
                    , updatePlayer forPlayer (Player.changeLifeBy (scaleUpSmallLifeChanges change)) duel
                    )

                Nothing ->
                    ( inputState, duel )
    in
    { model | inputState = newInputState, duelHistory = History.to newDuel duelHistory }



-- Update


type Msg
    = NoOp
    | ChangeLife { playerId : PlayerId, by : Int }
    | ToggleSelection { playerId : PlayerId }
    | SubmitLifeChange
    | NumericButtonPressed NumericInputButton
    | Reset
    | RequestDieRoll
    | RecieveDieRoll Die
    | RequestCoinFlip
    | RecieveCoinFlip Coin
    | DismissResultPrompt
    | ResizeWindow { width : Int, height : Int }


type alias UpdateFn =
    Msg -> Model -> ( Model, Cmd Msg )


update : UpdateFn
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeLife { playerId, by } ->
            ( { model
                | duelHistory =
                    model.duelHistory
                        |> evolve (updatePlayer playerId (Player.changeLifeBy by))
              }
            , Cmd.none
            )

        ToggleSelection { playerId } ->
            ( { model
                | inputState =
                    if InputState.isSelected playerId model.inputState then
                        InputState.removePlayerSelection model.inputState

                    else
                        InputState.selectPlayer playerId model.inputState
              }
            , Cmd.none
            )

        SubmitLifeChange ->
            ( submitLifePointChange model, Cmd.none )

        NumericButtonPressed numericButton ->
            ( { model | inputState = InputState.pressNumeric numericButton model.inputState }, Cmd.none )

        Reset ->
            ( initialModel { previousDuel = Nothing }, Cmd.none )

        RequestDieRoll ->
            ( model, Random.generate RecieveDieRoll Die.roll )

        RecieveDieRoll die ->
            ( { model | result = Just (DieRoll die) }, Cmd.none )

        RequestCoinFlip ->
            ( model, Random.generate RecieveCoinFlip Coin.flip )

        RecieveCoinFlip coin ->
            ( { model | result = Just (CoinFlip coin) }, Cmd.none )

        DismissResultPrompt ->
            ( { model | result = Nothing }, Cmd.none )

        ResizeWindow { width, height } ->
            ( model, Cmd.none )

{-| Wraps an update function and stores the state of the duel if it has changed. -}
storeDuelState : UpdateFn -> UpdateFn
storeDuelState f =
    let
        newUpdateFn msg model =
            let (newModel, cmd) = f msg model
                newCmd =
                    if (History.current newModel.duelHistory) /= History.current model.duelHistory then
                        Ports.storeDuelState (Duel.encode (History.current newModel.duelHistory))
                    else
                        Cmd.none
            in
                (newModel, Cmd.batch [ cmd, newCmd])
    in
        newUpdateFn


-- View


lpChangeText : Int -> String
lpChangeText change =
    if change >= 0 then
        "+" ++ String.fromInt change

    else
        String.fromInt change


lifeDisplay : PlayerId -> Model -> Element Msg
lifeDisplay playerId model =
    let
        lp =
            getLife playerId (History.current model.duelHistory)
    in
    let
        color =
            if InputState.isSelected playerId model.inputState then
                rgb255 230 0 0

            else
                rgb255 0 0 0
    in
    el
        [ paddingXY 30 10
        , Border.width 5
        , Border.rounded 3
        , Border.color color
        , Font.color color
        ]
        (text (LifePoints.toString lp))


centeredText : String -> Element msg
centeredText string =
    el [ centerX ] (text string)


inputButton : msg -> String -> Element msg
inputButton msg label =
    button
        [ width <| fillPortion 1
        , height fill
        , Border.color (rgb255 100 200 0)
        , Border.solid
        , Border.width 5
        , Border.rounded 5
        , padding 10
        ]
        { onPress = Just msg, label = centeredText label }


numberPad : Element Msg
numberPad =
    column [ width fill, height fill, centerX, padding 30, Element.spacing 5 ]
        [ row [ Element.spacing 5, height fill, width fill, centerX ]
            [ inputButton (NumericButtonPressed MINUS) "-"
            , inputButton Reset "RESET"
            , inputButton (NumericButtonPressed PLUS) "+"
            ]
        , row [ Element.spacing 5, height fill, width fill, centerX ]
            [ inputButton (NumericButtonPressed ONE) "1"
            , inputButton (NumericButtonPressed TWO) "2"
            , inputButton (NumericButtonPressed THREE) "3"
            ]
        , row [ Element.spacing 5, height fill, width fill, centerX ]
            [ inputButton (NumericButtonPressed FOUR) "4"
            , inputButton (NumericButtonPressed FIVE) "5"
            , inputButton (NumericButtonPressed SIX) "6"
            ]
        , row [ Element.spacing 5, height fill, width fill, centerX ]
            [ inputButton (NumericButtonPressed SEVEN) "7"
            , inputButton (NumericButtonPressed EIGHT) "8"
            , inputButton (NumericButtonPressed NINE) "9"
            ]
        , row [ Element.spacing 5, height fill, width fill, centerX ]
            [ inputButton (NumericButtonPressed ZERO) "0"
            , inputButton (NumericButtonPressed DOUBLE_ZERO) "00"
            , inputButton (NumericButtonPressed TRIPLE_ZERO) "000"
            ]
        , row [ Element.spacing 5, height fill, width fill, centerX ]
            [ inputButton RequestCoinFlip "COIN"
            , inputButton SubmitLifeChange "="
            , inputButton RequestDieRoll "DICE"
            ]
        ]


resultDisplay : Model -> Element Msg
resultDisplay { result } =
    case result of
        Just res ->
            el
                [ centerX
                , centerY
                , width fill
                , height fill
                , Element.behindContent
                    (el
                        [ width fill
                        , height fill
                        , alpha 0.1
                        , Background.color (rgb255 0 25 210)
                        , onClick DismissResultPrompt
                        ]
                        Element.none
                    )
                ]
                (el
                    [ centerX
                    , centerY
                    , width (px 300)
                    , height (px 300)
                    , Background.color (rgb255 200 300 222)
                    , Font.size 128
                    ]
                    (case res of
                        DieRoll die ->
                            el [ centerX, centerY ] (Element.text (Die.toString die))

                        CoinFlip Heads ->
                            el [ centerX, centerY ] (Element.text "H")

                        CoinFlip Tails ->
                            el [ centerX, centerY ] (Element.text "T")
                    )
                )

        Nothing ->
            Element.none


view : Model -> Browser.Document Msg
view model =
    { title = "YuGiOh Duel Calculator"
    , body =
        [ Element.layout [ Element.inFront (resultDisplay model) ] <|
            column
                [ width fill
                , height fill
                ]
                [ row [ fill |> width, padding 20 ]
                    [ column [ fill |> width, fill |> height ]
                        [ el [ centerX, onClick (ToggleSelection { playerId = Player1 }) ]
                            (lifeDisplay Player1 model)
                        ]
                    , column [ fill |> width, fill |> height ]
                        [ el [ centerX, onClick (ToggleSelection { playerId = Player2 }) ]
                            (lifeDisplay Player2 model)
                        ]
                    ]
                , el [ centerX, padding 20 ]
                    (case InputState.lifeChangeIndicated model.inputState of
                        Just { change } ->
                            text (lpChangeText change)

                        Nothing ->
                            text "---"
                    )
                , numberPad
                ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize (\w h -> ResizeWindow { width = w, height = h })


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = storeDuelState update
        , subscriptions = \_ -> Sub.none
        }
