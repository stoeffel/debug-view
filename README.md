Debug.View
==========

> Debug util to inspect values passed to a view function.


## Installation

You can install this using [elm-github-install](https://github.com/gdotdesign/elm-github-install).


## Usage

```elm
-- ...
import Debug.View


title : String -> Int -> Html msg
title str nr =
  h2 []
    [ text <| toString nr
    , text str
    ]


view model =
  -- ...
    Debug.View.inspect2 title model.title model.version
  -- ...
```
