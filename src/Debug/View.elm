module Debug.View exposing (inspect, inspect2)

import Native.Debug.View
import Html exposing (Html)
import Html.Attributes exposing (style, id)


inspect : String -> (a -> Html msg) -> a -> Html msg
inspect identifier view x =
    wrapper
        (view x
            :: (entryWrapper identifier <|
                    Native.Debug.inspect x identifier
               )
        )


inspect2 : String -> (a -> b -> Html msg) -> a -> b -> Html msg
inspect2 identifier view x y =
    wrapper
        (view x y
            :: (entryWrapper identifier <|
                    Native.Debug.inspect2 x y identifier
               )
        )


wrapper : List (Html msg) -> Html msg
wrapper content =
    Html.span
        [ style
            [ ( "position", "relative" )
            ]
        ]
        content


styles { radius, border, inverse } =
    [ ( "position", "absolute" )
    , ( "margin-top", "-10px" )
    , ( "left", "-4px" )
    , ( "background-color"
      , if inverse then
            "#c7f465"
        else
            "rgb(85, 99, 102)"
      )
    , ( "color"
      , if inverse then
            "rgb(85, 99, 102)"
        else
            "#fefefe"
      )
    , ( "overflow", "scroll" )
    , ( "max-height", "400px" )
    , ( "border-color", "#c7f465" )
    , ( "border-width", "2px" )
    , ( "border-style", border )
    , ( "box-shadow", "0 1px 3px rgba(85, 99, 102, 0.12), 0 1px 2px rgba(85, 99, 102, 0.24)" )
    , ( "border-radius", radius )
    ]


entryWrapper : String -> List String -> List (Html msg)
entryWrapper identifier history =
    [ Html.div
        [ style <|
            [ ( "display", "none" )
            , ( "padding", "10px" )
            , ( "z-index", "30000" )
            ]
                ++ styles { radius = "5px", border = "solid", inverse = False }
        , id ("elm-render-visualizer-entry-" ++ identifier)
        ]
        (closeButton identifier
            :: (List.reverse <| List.indexedMap (entry identifier) history)
        )
    , counter identifier <| List.length history
    ]


closeButton : String -> Html msg
closeButton identifier =
    Html.button
        [ style <|
            [ ( "display", "none" )
            , ( "padding", "10px" )
            , ( "position", "absolute" )
            , ( "padding", "10px" )
            , ( "position", "absolute" )
            , ( "background-color", "rgb(199, 244, 101)" )
            , ( "color", "rgb(85, 99, 102)" )
            , ( "max-height", "400px" )
            , ( "border-color", "rgb(199, 244, 101)" )
            , ( "border-width", "2px" )
            , ( "border-style", "none" )
            , ( "box-shadow", "rgba(85, 99, 102, 0.117647) 0px 1px 3px, rgba(85, 99, 102, 0.239216) 0px 1px 2px" )
            , ( "border-bottom-left-radius", "50%" )
            , ( "width", "20px" )
            , ( "height", "20px" )
            , ( "display", "block" )
            , ( "z-index", "99999999" )
            , ( "position", "absolute" )
            , ( "right", "-3px" )
            , ( "top", "-3px" )
            , ( "line-height", "2px" )
            , ( "padding", "0px" )
            ]
        , id ("elm-render-visualizer-close-button-" ++ identifier)
        ]
        [ Html.text "X"
        ]


counter : String -> Int -> Html msg
counter identifier index =
    Html.div
        [ style <|
            [ ( "list-style", "none" )
            , ( "padding", "2px" )
            , ( "font-size", "8px" )
            , ( "z-index", "20000" )
            ]
                ++ styles { radius = "50%", border = "none", inverse = True }
        , id ("elm-render-visualizer-counter-" ++ identifier)
        ]
        [ Html.text <| toString index ]


entry : String -> Int -> String -> Html msg
entry identifier index log =
    Html.div
        [ style
            [ ( "list-style", "none" )
            , ( "width", "400px" )
            ]
        ]
        [ Html.text (toString (index + 1) ++ ": " ++ log) ]
