module PostUX.State exposing (..)

import Common.Types
import Post
import PostUX.Types exposing (..)
import UserNotice as UN exposing (UserNotice)


init : Model
init =
    { showInput = None
    , showAddress = False
    }


update :
    Msg
    -> Model
    -> UpdateResult
update msg prevModel =
    case msg of
        --NoOp ->
        --justModelUpdate prevModel
        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]

        PhaceIconClicked ->
            justModelUpdate
                { prevModel
                    | showAddress =
                        not prevModel.showAddress
                }

        SupportBurnClicked ->
            UpdateResult
                { prevModel
                    | showInput =
                        case prevModel.showInput of
                            Burn _ ->
                                None

                            _ ->
                                Burn ""
                }
                Cmd.none
                []

        SupportTipClicked ->
            UpdateResult
                { prevModel
                    | showInput =
                        case prevModel.showInput of
                            Tip _ ->
                                None

                            _ ->
                                Tip ""
                }
                Cmd.none
                []

        AmountInputChanged newInput ->
            case prevModel.showInput of
                None ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        [ Common.Types.AddUserNotice <|
                            UN.unexpectedError "Input changed when showInput == None" newInput
                        ]

                Tip _ ->
                    justModelUpdate
                        { prevModel
                            | showInput =
                                Tip newInput
                        }

                Burn _ ->
                    justModelUpdate
                        { prevModel
                            | showInput =
                                Burn newInput
                        }

        SupportTipSubmitClicked postId amount ->
            UpdateResult
                prevModel
                Cmd.none
                [ Common.Types.SubmitTip postId amount ]

        SupportBurnSubmitClicked postId amount ->
            UpdateResult
                prevModel
                Cmd.none
                [ Common.Types.SubmitBurn postId amount ]

        ResetActionForm ->
            justModelUpdate
                { prevModel
                    | showInput = None
                }
