module View.About exposing (view)

import Element exposing (Element, centerX, column, el, fill, height, padding, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Theme exposing (black, white)
import Types exposing (Model, Msg)
import View.Attrs exposing (cappedWidth, hover, whiteGlowAttribute)
import View.Common exposing (link, when)


view : Model -> Element Msg
view model =
    let
        isMobile =
            model.dProfile == Types.Mobile
    in
    [ Element.image
        [ width fill
        , Background.color black
        , whiteGlowAttribute
        , centerX
        ]
        { src = "./img/banner.png"
        , description = "Never be silenced"
        }
        |> when (not isMobile)
    , [ [ viewHeader "What is SmokeSignal?"
        , viewPara "SmokeSignal uses the Ethereum blockchain to facilitate uncensorable, global chat."
        , viewPara "All SmokeSignal posts are permanent and impossible for anyone to delete."
        , viewPara "All SmokeSignal authors are pseudonymous by default and cannot be deplatformed."
        , [ text "All you need to post is a web3 wallet like "
          , link "https://metamask.io/" "MetaMask"
          , text " and "
          , link "https://www.google.com/search?q=how+to+buy+ether"
                "ETH to burn"
          , text "."
          ]
            |> paragraph []
        , viewPara "The more ETH you burn, the more attention the post will get."
        ]
            |> column [ spacing 10 ]
      , [ viewHeader "What tokens are supported?"
        , [ text "SmokeSignal currently supports "
          , link "https://ethereum.org/" "Ethereum"
          , text " and "
          , link "https://www.xdaichain.com/" "xDai"
          , text "."
          ]
            |> paragraph []
        ]
            |> column [ width fill, spacing 10 ]
      , [ viewHeader "What is xDai?"
        , [ text "The xDai chain is a stable payments blockchain designed for fast and inexpensive transactions."
          ]
            |> paragraph []
        ]
            |> column [ spacing 20 ]
      , Element.newTabLink [ hover, centerX ]
            { url = "https://github.com/team-toast/SmokeSignal"
            , label =
                [ Element.image [ height <| px 32 ]
                    { src = "./img/github.png", description = "" }
                , text "Source code"
                    |> el [ Font.underline ]
                ]
                    |> row [ spacing 10 ]
            }
      , [ text "Posts to previous versions of SmokeSignal can be found at "
        , Element.newTabLink
            [ Font.bold
            , hover
            , Font.underline
            ]
            { url = model.alphaUrl
            , label = text "SmokeSignal Alpha"
            }
        , text "."
        ]
            |> paragraph
                [ View.Attrs.sansSerifFont
                , Font.center
                ]
      ]
        |> column
            [ width fill
            , Background.color black
            , padding 30
            , spacing 30
            , Font.color white
            ]
    ]
        |> column
            [ height fill
            , cappedWidth 750
            , spacing 20
            , centerX
            ]


viewHeader : String -> Element msg
viewHeader =
    text
        >> List.singleton
        >> paragraph [ Font.size 30, Font.bold ]


viewPara : String -> Element msg
viewPara =
    text
        >> List.singleton
        >> paragraph []
