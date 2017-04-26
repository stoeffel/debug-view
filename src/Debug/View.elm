module Debug.View
    exposing
        ( inspect
        , inspect2
        , inspect3
        , inspect4
        , inspect5
        , inspect6
        , inspect7
        , inspect8
        )

{-| # Debug.View

Allows to inspect values passed to a view-function.

@docs inspect, inspect2, inspect3, inspect4, inspect5, inspect6, inspect7, inspect8
-}

import Native.Debug.View
import Html exposing (Html)
import Html.Keyed exposing (node)
import Html.Attributes exposing (style, id, attribute, property, class)


inspectBase : String -> Html msg -> (String -> List ElmType) -> Html msg
inspectBase identifier view nativeFunction =
    contentSpan
        (view :: (wrapper identifier <| nativeFunction identifier))


{-| Inspect 1 value passed to a view-function.

    Debug.View.inspect "id" (\x -> h1 [] [ text x ]) model.title
-}
inspect : String -> (a -> Html msg) -> a -> Html msg
inspect identifier view x1 =
    inspectBase identifier (view x1) <|
        Native.Debug.View.inspect x1


{-| Inspect 2 value passed to a view-function.
-}
inspect2 : String -> (a -> b -> Html msg) -> a -> b -> Html msg
inspect2 identifier view x1 x2 =
    inspectBase identifier (view x1 x2) <|
        Native.Debug.View.inspect2 x1 x2


{-| Inspect 3 value passed to a view-function.
-}
inspect3 : String -> (a -> b -> c -> Html msg) -> a -> b -> c -> Html msg
inspect3 identifier view x1 x2 x3 =
    inspectBase identifier (view x1 x2 x3) <|
        Native.Debug.View.inspect3 x1 x2 x3


{-| Inspect 4 value passed to a view-function.
-}
inspect4 : String -> (a -> b -> c -> d -> Html msg) -> a -> b -> c -> d -> Html msg
inspect4 identifier view x1 x2 x3 x4 =
    inspectBase identifier (view x1 x2 x3 x4) <|
        Native.Debug.View.inspect4 x1 x2 x3 x4


{-| Inspect 5 value passed to a view-function.
-}
inspect5 : String -> (a -> b -> c -> d -> e -> Html msg) -> a -> b -> c -> d -> e -> Html msg
inspect5 identifier view x1 x2 x3 x4 x5 =
    inspectBase identifier (view x1 x2 x3 x4 x5) <|
        Native.Debug.View.inspect5 x1 x2 x3 x4 x5


{-| Inspect 6 value passed to a view-function.
-}
inspect6 : String -> (a -> b -> c -> d -> e -> f -> Html msg) -> a -> b -> c -> d -> e -> f -> Html msg
inspect6 identifier view x1 x2 x3 x4 x5 x6 =
    inspectBase identifier (view x1 x2 x3 x4 x5 x6) <|
        Native.Debug.View.inspect6 x1 x2 x3 x4 x5 x6


{-| Inspect 7 value passed to a view-function.
-}
inspect7 : String -> (a -> b -> c -> d -> e -> f -> g -> Html msg) -> a -> b -> c -> d -> e -> f -> g -> Html msg
inspect7 identifier view x1 x2 x3 x4 x5 x6 x7 =
    inspectBase identifier (view x1 x2 x3 x4 x5 x6 x7) <|
        Native.Debug.View.inspect7 x1 x2 x3 x4 x5 x6 x7


{-| Inspect 8 value passed to a view-function.
-}
inspect8 : String -> (a -> b -> c -> d -> e -> f -> g -> h -> Html msg) -> a -> b -> c -> d -> e -> f -> g -> h -> Html msg
inspect8 identifier view x1 x2 x3 x4 x5 x6 x7 x8 =
    inspectBase identifier (view x1 x2 x3 x4 x5 x6 x7 x8) <|
        Native.Debug.View.inspect8 x1 x2 x3 x4 x5 x6 x7 x8


stylesheet : Html msg
stylesheet =
    Html.node "style"
        []
        [ Html.text css
        ]


css : String
css =
    """
    .elm-debug-view-ellipsis,
    .elm-debug-view-text {
        white-space: nowrap;
        text-overflow: ellipsis;
        white-space: nowrap;
        overflow: hidden;
    }

    .elm-debug-view-text {
        display: none;
    }

    .elm-debug-view-collapsed .elm-debug-view-text {
        display: inline;
    }

    .elm-debug-view-detailed {
        padding-left: 2em;
    }

    .elm-debug-view-collapsed .elm-debug-view-detailed {
        display: none;
    }

    .elm-debug-view-entry {
        list-style: none;
        width: 400px;
        overflow: hidden;
        text-overflow: ellipsis;
        cursor: pointer;
    }

    .elm-debug-view-entry > span > .elm-debug-view-detailed {
        padding-left: 0px;
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


styles : { radius : String, border : String, inverse : Bool } -> List ( String, String )
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
        , id ("elm-debug-view-entry-" ++ identifier)
        ]
        [ closeButton identifier
        , List.indexedMap (entry identifier) history
            |> List.reverse
            |> node "div"
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
        , id ("elm-debug-view-close-button-" ++ identifier)
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
        , id ("elm-debug-view-counter-" ++ identifier)
        ]
        [ Html.text <| toString index ]


entry : String -> Int -> ElmType -> ( String, Html msg )
entry identifier index log =
    ( identifier ++ toString index
    , Html.div
        [ class "elm-debug-view-entry"
        ]
        [ Html.span
            [ style
                [ ( "display", "flex" )
                , ( "opacity", ".5" )
                ]
            ]
            [ Html.text (toString index), Html.hr [ style [ ( "flex", "1" ) ] ] [] ]
        , renderElmType log
        ]
    )


renderElmType : ElmType -> Html msg
renderElmType =
    elmTypeToTree >> treeToHtml


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
    | ElmUnionType String (List ElmType)


type Tree
    = ListItem String
    | KeyValue ( String, Tree )
    | Block BlockData (List Tree)


type alias BlockData =
    { open : String
    , close : String
    , empty : String
    , separator : String
    }


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
            Block
                { open = "["
                , close = "]"
                , empty = "[]"
                , separator = ", "
                }
            <|
                List.map elmTypeToTree xs

        ElmTuple xs ->
            Block
                { open = "("
                , close = ")"
                , empty = "()"
                , separator = ", "
                }
            <|
                List.map elmTypeToTree xs

        ElmArray xs ->
            Block
                { open = "Array.fromList ["
                , close = "]"
                , empty = "Array.empty"
                , separator = ", "
                }
            <|
                List.map elmTypeToTree xs

        ElmSet xs ->
            Block
                { open = "Set.fromList ["
                , close = "]"
                , empty = "Set.empty"
                , separator = ", "
                }
            <|
                List.map elmTypeToTree xs

        ElmDict xs ->
            Block
                { open = "Dict.fromList ["
                , close = "]"
                , empty = "Dict.empty"
                , separator = ", "
                }
            <|
                List.map elmTypeToTree xs

        ElmRecord [] ->
            ListItem "{}"

        ElmRecord xs ->
            Block
                { open = "{"
                , close = "}"
                , empty = "{}"
                , separator = ", "
                }
            <|
                List.map (\( k, v ) -> KeyValue ( k, elmTypeToTree v )) xs

        ElmChar char ->
            ListItem <| ("'" ++ String.fromChar char ++ "'")

        ElmString string ->
            ListItem ("\"" ++ string ++ "\"")

        ElmCustom something ->
            ListItem something

        ElmUnionType ctor args ->
            Block
                { open = ctor ++ " "
                , close = ""
                , empty = ctor
                , separator = " "
                }
            <|
                List.map elmTypeToTree args


treeToOneLine : Tree -> List (Html msg)
treeToOneLine t =
    case t of
        ListItem string ->
            [ Html.text string ]

        KeyValue ( k, v ) ->
            List.concat
                [ [ highlighted k ]
                , [ Html.text " = " ]
                , treeToOneLine v
                ]

        Block { empty } [] ->
            [ Html.text empty ]

        Block { open, close, separator } xs ->
            [ [ Html.text open ]
            , List.intersperse (ListItem separator) xs
                |> List.concatMap treeToOneLine
            , [ Html.text close ]
            ]
                |> List.concat


treeToHtml : Tree -> Html msg
treeToHtml t =
    case t of
        ListItem string ->
            Html.text string

        KeyValue ( k, v ) ->
            Html.span []
                [ Html.span []
                    [ highlighted k
                    , Html.span [] [ Html.text " = " ]
                    ]
                , treeToHtml v
                ]

        Block blockData xs ->
            Html.span
                [ class "elm-debug-view-collapsed"
                , attribute "onclick" "_elmRenderVisualizerToggleCollapse(this);"
                ]
                [ Html.span [ class "elm-debug-view-text" ] <| treeToOneLine t
                , renderItems blockData xs
                    |> Html.div [ class "elm-debug-view-detailed" ]
                ]


highlighted : String -> Html msg
highlighted t =
    Html.span
        [ style [ ( "color", "#c7f465" ) ]
        ]
        [ Html.text t ]


renderItems : BlockData -> List Tree -> List (Html msg)
renderItems { open, close, empty, separator } xs =
    case List.map treeToHtml xs of
        [] ->
            [ Html.text empty ]

        head :: tail ->
            List.concat
                [ [ nowrap [ Html.text <| open ++ " ", head ] ]
                , List.map (\tree -> nowrap [ Html.text separator, tree ]) tail
                , [ Html.div [] [ Html.text close ] ]
                ]


nowrap : List (Html msg) -> Html msg
nowrap children =
    Html.div [ class "elm-debug-view-ellipsis" ] children
