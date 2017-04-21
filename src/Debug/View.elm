module Debug.View exposing (inspect, inspect2)

import Native.Debug.View
import Html exposing (Html)
import Html.Attributes exposing (style, id, attribute, property, class)


inspect : String -> (a -> Html msg) -> a -> Html msg
inspect identifier view x =
    contentSpan
        (view x
            :: (wrapper identifier <|
                    Native.Debug.inspect x identifier
               )
        )


inspect2 : String -> (a -> b -> Html msg) -> a -> b -> Html msg
inspect2 identifier view x y =
    contentSpan
        (view x y
            :: (wrapper identifier <|
                    Native.Debug.inspect2 x y identifier
               )
        )


stylesheet : Html msg
stylesheet =
    Html.node "style"
        []
        [ Html.text css
        ]


css : String
css =
    """
    .elm-render-visualizer-ellipsis,
    .elm-render-visualizer-text {
        white-space: nowrap;
        text-overflow: ellipsis;
        white-space: nowrap;
        overflow: hidden;
    }

    .elm-render-visualizer-text {
        display: none;
    }

    .elm-render-visualizer-collapsed .elm-render-visualizer-text {
        display: inline;
    }

    .elm-render-visualizer-detailed {
        padding-left: 2em;
    }

    .elm-render-visualizer-collapsed .elm-render-visualizer-detailed {
        display: none;
    }
"""


contentSpan : List (Html msg) -> Html msg
contentSpan content =
    Html.span
        [ style
            [ ( "position", "relative" )
            ]
        ]
        (stylesheet :: content)


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
    , ( "max-height", "400px" )
    , ( "border-color", "#c7f465" )
    , ( "border-width", "2px" )
    , ( "border-style", border )
    , ( "box-shadow", "0 1px 3px rgba(85, 99, 102, 0.12), 0 1px 2px rgba(85, 99, 102, 0.24)" )
    , ( "border-radius", radius )
    ]


wrapper : String -> List ElmType -> List (Html msg)
wrapper identifier history =
    [ counter identifier <| List.length history
    , entries identifier history
    ]


entries : String -> List ElmType -> Html msg
entries identifier history =
    Html.div
        [ style <|
            [ ( "display", "none" )
            , ( "padding", "10px" )
            , ( "z-index", "30000" )
            , ( "font-family", "monospace" )
            ]
                ++ styles { radius = "5px", border = "solid", inverse = False }
        , id ("elm-render-visualizer-entry-" ++ identifier)
        ]
        [ closeButton identifier
        , List.indexedMap (entry identifier) history
            |> List.reverse
            |> Html.div
                [ style
                    [ ( "overflow", "scroll" )
                    , ( "width", "100%" )
                    ]
                ]
        ]


closeButton : String -> Html msg
closeButton identifier =
    Html.button
        [ style <|
            [ ( "display", "none" )
            , ( "padding", "10px" )
            , ( "position", "absolute" )
            , ( "padding", "10px" )
            , ( "background-color", "rgb(199, 244, 101)" )
            , ( "color", "rgb(85, 99, 102)" )
            , ( "max-height", "400px" )
            , ( "border-color", "rgb(199, 244, 101)" )
            , ( "border-width", "2px" )
            , ( "border-style", "none" )
            , ( "border-bottom-left-radius", "50%" )
            , ( "width", "20px" )
            , ( "height", "20px" )
            , ( "display", "block" )
            , ( "z-index", "99999999" )
            , ( "position", "absolute" )
            , ( "right", "-2px" )
            , ( "top", "-2px" )
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


type ElmType
    = ElmFunction String
    | ElmBoolean Bool
    | ElmNumber String
    | ElmList (List ElmType)
    | ElmTuple (List ElmType)
    | ElmArray (List ElmType)
    | ElmSet (List ElmType)
    | ElmDict (List ElmType)
    | ElmRecord (List ( String, ElmType ))
    | ElmChar Char
    | ElmString String
    | ElmCustom String


entry : String -> Int -> ElmType -> Html msg
entry identifier index log =
    Html.div
        [ style
            [ ( "list-style", "none" )
            , ( "width", "400px" )
            , ( "overflow", "hidden" )
            , ( "text-overflow", "ellipsis" )
            ]
        ]
        [ Html.text (toString index ++ ": ")
        , renderElmType log
        ]


renderElmType : ElmType -> Html msg
renderElmType =
    elmTypeToTree >> treeToHtml


type Tree
    = ListItem String
    | KeyValue ( String, Tree )
    | Block String String (List Tree)


elmTypeToTree : ElmType -> Tree
elmTypeToTree log =
    case log of
        ElmFunction name ->
            ListItem <| "Function " ++ name

        ElmBoolean bool ->
            ListItem <| toString bool

        ElmNumber num ->
            ListItem num

        ElmList xs ->
            Block "[" "]" <| List.map elmTypeToTree xs

        ElmTuple xs ->
            Block "(" ")" <| List.map elmTypeToTree xs

        ElmArray xs ->
            Block "Array.fromList [" "]" <| List.map elmTypeToTree xs

        ElmSet xs ->
            Block "Set.fromList [" "]" <| List.map elmTypeToTree xs

        ElmDict xs ->
            Block "Dict.fromList [" "]" <| List.map elmTypeToTree xs

        ElmRecord [] ->
            ListItem "{}"

        ElmRecord xs ->
            Block "{" "}" <| List.map (\( k, v ) -> KeyValue ( k, elmTypeToTree v )) xs

        ElmChar char ->
            ListItem <| String.fromChar char

        ElmString string ->
            ListItem string

        ElmCustom something ->
            ListItem something


treeToText : Tree -> String
treeToText t =
    case t of
        ListItem string ->
            string

        KeyValue ( k, v ) ->
            k ++ " = " ++ treeToText v

        Block open close xs ->
            [ open
            , List.intersperse (ListItem ", ") xs
                |> List.map treeToText
                |> String.concat
            , close
            ]
                |> String.concat


treeToHtml : Tree -> Html msg
treeToHtml t =
    case t of
        ListItem string ->
            Html.text string

        KeyValue ( k, v ) ->
            Html.span []
                [ Html.text <| k ++ " = "
                , treeToHtml v
                ]

        Block open close xs ->
            Html.span
                [ class "elm-render-visualizer-collapsed"
                , attribute "onclick" "_elmRenderVisualizerToggleCollapse(this);"
                ]
                [ Html.span [ class "elm-render-visualizer-text" ] [ Html.text <| treeToText t ]
                , renderItems open close xs
                    |> Html.div [ class "elm-render-visualizer-detailed" ]
                ]


renderItems : String -> String -> List Tree -> List (Html msg)
renderItems open close xs =
    case List.map treeToHtml xs of
        [] ->
            [ Html.text (open ++ close) ]

        head :: tail ->
            List.concat
                [ [ nowrap [ Html.text <| open ++ " ", head ] ]
                , List.map (\tree -> nowrap [ Html.text ", ", tree ]) tail
                , [ Html.div [] [ Html.text close ] ]
                ]


nowrap : List (Html msg) -> Html msg
nowrap children =
    Html.div [ class "elm-render-visualizer-ellipsis" ] children
