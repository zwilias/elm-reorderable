# elm-reorderable [![Build Status](https://travis-ci.org/zwilias/elm-reorderable.svg?branch=master)](https://travis-ci.org/zwilias/elm-reorderable)
> Reorder entries while maintaining a key/value correspondence.

Having a list of entries that need to accept input, need to render efficiently,
*and* can be arbitrarily reordered is fairly common. `Keyed` nodes are a great
tool for implementing that, but there's a catch: every item needs to have some
sort of unique identifier.

In reality, you don't always want every single item to have an identifier. You
could of course add that to your model, _just_ for the sake of making it
possible to use `Keyed` nodes, but that's not always practical, and arguably
it's simply not the best solution.

`Reorderable` attempts to take some of that pain away by keeping track of a
sequential identifier for everything you add to it; while also making sure that
the identifier survives things like dropping items, swapping items at places or
arbitrarily moving an item through the structure.

## [Example](https://ellie-app.com/mKsXW3R6sa1/0)

```elm
module Main exposing (main)

import Html exposing (Html, text)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed as Keyed
import Reorderable exposing (Reorderable)


type alias Model =
    Reorderable String


type Msg
    = Add
    | Remove Int
    | MoveUp Int
    | MoveDown Int
    | Input Int String


initialModel : Model
initialModel =
    Reorderable.singleton ""


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            Reorderable.push "" model

        Remove idx ->
            Reorderable.drop idx model

        MoveUp idx ->
            Reorderable.moveUp idx model

        MoveDown idx ->
            Reorderable.moveDown idx model

        Input idx input ->
            Reorderable.set idx input model


view : Model -> Html Msg
view items =
    Html.div []
        [ Keyed.node "div" [] <|
            List.indexedMap viewItem (Reorderable.toKeyedList items)
        , Html.button
            [ Events.onClick Add ]
            [ text "Add another entry" ]
        ]


viewItem : Int -> ( String, String ) -> ( String, Html Msg )
viewItem idx ( key, value ) =
    Html.div [ Attr.class "side-by-side" ]
        [ Html.textarea [ Events.onInput <| Input idx ] []
        , Html.ul [ Attr.class "actions" ]
            [ Html.li [ Events.onClick <| Remove idx ] [ text "remove" ]
            , Html.li [ Events.onClick <| MoveUp idx ] [ text "move up" ]
            , Html.li [ Events.onClick <| MoveDown idx ] [ text "move down" ]
            ]
        ]
        |> (,) key


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
```

---

Made with ❤️
