module InputState exposing
    ( InputState
    , NumericInputButton(..)
    , clearChangeInput
    , init
    , isSelected
    , lifeChangeIndicated
    , pressNumeric
    , removePlayerSelection
    , selectPlayer
    )

import Player exposing (PlayerId)


type Sign
    = Positive
    | Negative


type alias InputState =
    { selectedPlayer : Maybe PlayerId
    , changeAmountEntered : Maybe Int
    , sign : Sign
    }


type NumericInputButton
    = ONE
    | TWO
    | THREE
    | FOUR
    | FIVE
    | SIX
    | SEVEN
    | EIGHT
    | NINE
    | ZERO
    | DOUBLE_ZERO
    | TRIPLE_ZERO
    | PLUS
    | MINUS


init : InputState
init =
    { selectedPlayer = Nothing
    , changeAmountEntered = Nothing
    , sign = Positive
    }


selectPlayer : PlayerId -> InputState -> InputState
selectPlayer playerId inputState =
    (if isSelected playerId inputState then
        identity

     else
        clearChangeInput
    )
        { inputState | selectedPlayer = Just playerId }


removePlayerSelection : InputState -> InputState
removePlayerSelection inputState =
    { inputState | selectedPlayer = Nothing }


clearChangeInput : InputState -> InputState
clearChangeInput inputState =
    { inputState | changeAmountEntered = Nothing }


lifeChangeIndicated : InputState -> Maybe { change : Int, forPlayer : PlayerId }
lifeChangeIndicated { sign, selectedPlayer, changeAmountEntered } =
    let
        signFactor =
            case sign of
                Positive ->
                    1

                Negative ->
                    -1
    in
    case ( selectedPlayer, changeAmountEntered ) of
        ( Just player, Just changeAmount ) ->
            Just
                { change = changeAmount * signFactor
                , forPlayer = player
                }

        ( _, _ ) ->
            Nothing


isSelected : PlayerId -> InputState -> Bool
isSelected playerId inputState =
    case inputState.selectedPlayer of
        Just selectedPlayer ->
            playerId == selectedPlayer

        Nothing ->
            False


lifeLimit : Int
lifeLimit =
    9999


appendNumber : Int -> InputState -> InputState
appendNumber n inputState =
    { inputState
        | changeAmountEntered =
            case inputState.changeAmountEntered of
                Just amount ->
                    -- We don't want the life total to go over 9999, and we also dont want to round
                    -- up to 9999, so any single number input that would bring us there should be ignored.
                    if amount > lifeLimit then
                        Just amount

                    else
                        Just ((amount * 10) + n)

                Nothing ->
                    Just n
    }


scaleBy : Int -> InputState -> InputState
scaleBy n inputState =
    { inputState
        | changeAmountEntered =
            case inputState.changeAmountEntered of
                Just amount ->
                    if amount * n > lifeLimit then
                        Just amount

                    else
                        Just (amount * n)

                Nothing ->
                    Nothing
    }


pressNumeric : NumericInputButton -> InputState -> InputState
pressNumeric numericButton inputState =
    if inputState.selectedPlayer /= Nothing then
        case numericButton of
            ONE ->
                appendNumber 1 inputState

            TWO ->
                appendNumber 2 inputState

            THREE ->
                appendNumber 3 inputState

            FOUR ->
                appendNumber 4 inputState

            FIVE ->
                appendNumber 5 inputState

            SIX ->
                appendNumber 6 inputState

            SEVEN ->
                appendNumber 7 inputState

            EIGHT ->
                appendNumber 8 inputState

            NINE ->
                appendNumber 9 inputState

            ZERO ->
                scaleBy 10 inputState

            DOUBLE_ZERO ->
                scaleBy 100 inputState

            TRIPLE_ZERO ->
                scaleBy 1000 inputState

            PLUS ->
                { inputState | sign = Positive }

            MINUS ->
                { inputState | sign = Negative }

    else
        inputState
