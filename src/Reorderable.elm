module Reorderable exposing
    ( Reorderable, empty, isEmpty, singleton, push, get, set, update
    , swap, moveUp, moveDown, move, insertAt, insertAfter, drop, reverse
    , fromList, toList, toKeyedList
    )

{-| `Reorderable` is useful for structures where you want to allow a user to
reorder things, while still wanting to make effective use of `Html.Keyed`. The
idea is to have a datastructure that (internally) keeps the association between
some an incremental key and data (like an Array), but retains that association
while shuffling data around.


# Basics

@docs Reorderable, empty, isEmpty, singleton, push, get, set, update


# Manipulation

@docs swap, moveUp, moveDown, move, insertAt, insertAfter, drop, reverse


# List-y stuff

@docs fromList, toList, toKeyedList

-}

import Array exposing (Array)


{-| -}
type Reorderable a
    = Reorderable Int (Array ( Int, a ))


{-| Does what one would expect it to do: create an empty `Reorderable`. Note
that checking equality against `empty` is not a good way to check if there are 0
values. Pushing and subsequently dropping entries will increment the internal
key.

    toList empty
    --> []

    fromList []
    --> empty

-}
empty : Reorderable a
empty =
    Reorderable 0 Array.empty


{-| Checks if a reorderable contains zero entries.

    isEmpty empty
    --> True


    singleton "hi"
        |> isEmpty
    --> False


    fromList [ "hello", "world" ]
        |> drop 0
        |> drop 0
        |> isEmpty
    --> True

    fromList [ "hello", "world" ]
        |> drop 0
        |> drop 0
        |> (==) empty
    --> False

-}
isEmpty : Reorderable a -> Bool
isEmpty (Reorderable _ values) =
    Array.isEmpty values


{-| Create a reorderable from a single piece of data.

    singleton "hi"
        |> toList
    --> [ "hi" ]

-}
singleton : a -> Reorderable a
singleton v =
    push v empty


{-| Pushes a piece of data onto the end of a `Reorderable`.

    empty
        |> push "hello"
        |> push "world"
        |> toList
    --> [ "hello", "world" ]

-}
push : a -> Reorderable a -> Reorderable a
push val (Reorderable nextKey values) =
    Reorderable (nextKey + 1) <| Array.push ( nextKey, val ) values


{-| Inserts a piece of data into a specifc position of the `Reorderable`.

    letters : Reorderable String
    letters =
        fromList [ "a", "b", "c", "d" ]


    letters
        |> insertAt 0 "foo"
        |> toList
    --> [ "foo", "a", "b", "c", "d" ]


    letters
        |> insertAt 4 "foo"
        |> toList
    --> [ "a", "b", "c", "d", "foo" ]


    letters
        |> insertAt 2 "foo"
        |> toKeyedList
    --> [ ( "0", "a" )
    --> , ( "1", "b" )
    --> , ( "4", "foo" )
    --> , ( "2", "c" )
    --> , ( "3", "d" )
    --> ]

-}
insertAt : Int -> a -> Reorderable a -> Reorderable a
insertAt index val (Reorderable nextKey values) =
    let
        before =
            Array.slice 0 index values

        after =
            Array.slice index (Array.length values) values
    in
    Reorderable (nextKey + 1) <|
        Array.append (Array.push ( nextKey, val ) before) after


{-| Convenience function to insert something after a specified index.

    fromList [ "a", "c" ]
        |> insertAfter 0 "b"
        |> toList
    --> [ "a", "b", "c" ]


    fromList [ "a", "b" ]
        |> insertAfter 1 "c"
        |> toList
    --> [ "a", "b", "c" ]

-}
insertAfter : Int -> a -> Reorderable a -> Reorderable a
insertAfter index =
    insertAt (index + 1)


{-| Drops the entry at a certain index.

    fromList [ "a", "removeMe", "b" ]
        |> drop 1
        |> toKeyedList
    --> [ ( "0", "a" ), ( "2", "b" ) ]

-}
drop : Int -> Reorderable a -> Reorderable a
drop idx (Reorderable nextKey values) =
    Reorderable nextKey <|
        Array.append
            (Array.slice 0 idx values)
            (Array.slice (idx + 1) (Array.length values) values)


{-| Swaps two entries, unless either of the indices is out of bounds.

    fromList [ "a", "d", "c", "b" ]
        |> swap 1 3
        |> toKeyedList
    --> [ ( "0", "a" )
    --> , ( "3", "b" )
    --> , ( "2", "c" )
    --> , ( "1", "d" )
    --> ]


    fromList [ "hi", "there" ]
        |> swap 0 2
        |> toList
    --> [ "hi", "there" ]

-}
swap : Int -> Int -> Reorderable a -> Reorderable a
swap a b (Reorderable nextKey values) =
    Maybe.map2
        (\first second ->
            values
                |> Array.set a second
                |> Array.set b first
        )
        (Array.get a values)
        (Array.get b values)
        |> Maybe.withDefault values
        |> Reorderable nextKey


{-| Convenience function to move an item "up", i.e. "back".

    fromList [ "a", "c", "b" ]
        |> moveUp 2
        |> toKeyedList
    --> [ ( "0", "a" )
    --> , ( "2", "b" )
    --> , ( "1", "c" )
    --> ]

-}
moveUp : Int -> Reorderable a -> Reorderable a
moveUp idx =
    swap (idx - 1) idx


{-| Convenience function to move an item "down", i.e. "forward".

    fromList [ "a", "c", "b" ]
        |> moveDown 1
        |> toKeyedList
    --> [ ( "0", "a" )
    --> , ( "2", "b" )
    --> , ( "1", "c" )
    --> ]

-}
moveDown : Int -> Reorderable a -> Reorderable a
moveDown idx =
    swap idx (idx + 1)


{-| Move an item from one location to another location.

    fromList [ "b", "c", "a" ]
        |> move 2 0
        |> toList
    --> [ "a", "b", "c" ]

    fromList [ "a", "c", "b"]
        |> move 2 1
        |> toList
    --> [ "a", "b", "c" ]

-}
move : Int -> Int -> Reorderable a -> Reorderable a
move from to ((Reorderable nextKey values) as original) =
    case Array.get from values of
        Nothing ->
            original

        Just value ->
            let
                firstHole =
                    if from < to then
                        1

                    else
                        0

                secondHole =
                    if from > to then
                        1

                    else
                        0

                before =
                    Array.slice 0 (min from to) values

                between =
                    Array.slice
                        (min from to + firstHole)
                        (max from to + firstHole)
                        values

                after =
                    Array.slice
                        (max from to + firstHole + secondHole)
                        (Array.length values)
                        values
            in
            Reorderable nextKey <|
                case compare from to of
                    LT ->
                        Array.append before between
                            |> Array.push value
                            |> (\x -> Array.append x after)

                    EQ ->
                        values

                    GT ->
                        Array.push value before
                            |> (\x -> Array.append x between)
                            |> (\x -> Array.append x after)


{-| Reverse the order of the entries.

    fromList [ "first", "second", "third" ]
        |> reverse
        |> toKeyedList
    --> [ ( "2", "third" )
    --> , ( "1", "second" )
    --> , ( "0", "first" )
    --> ]

-}
reverse : Reorderable a -> Reorderable a
reverse (Reorderable nextKey values) =
    Reorderable nextKey (arrayReverse values)


arrayReverse : Array a -> Array a
arrayReverse =
    Array.toList >> List.reverse >> Array.fromList


{-| Run an update-function on a specified item.

    fromList [ "UPPER", "lower", "UPPER"]
        |> update 1 String.toUpper
        |> toKeyedList
    --> [ ( "0", "UPPER" )
    --> , ( "1", "LOWER" )
    --> , ( "2", "UPPER" )
    --> ]

-}
update : Int -> (a -> a) -> Reorderable a -> Reorderable a
update idx f (Reorderable nextKey values) =
    Reorderable nextKey <|
        case Array.get idx values of
            Just ( key, val ) ->
                Array.set idx ( key, f val ) values

            Nothing ->
                values


{-| Try to get the item at a specified index.

    fromList [ "a", "b", "c" ]
        |> get 1
    --> Just "b"

-}
get : Int -> Reorderable a -> Maybe a
get idx (Reorderable _ values) =
    Array.get idx values |> Maybe.map Tuple.second


{-| Set the value at a specified index (maintaining the key). Basically
shorthand for `update index (always val) reorderable`.

If the specified index does not exist, this does nothing.

-}
set : Int -> a -> Reorderable a -> Reorderable a
set index val =
    update index (always val)


{-| Convert a `Reorderable a` to a plain old `List a`.

    fromList [ "a", "b", "c" ]
        |> toList
    --> [ "a", "b", "c" ]

-}
toList : Reorderable a -> List a
toList (Reorderable _ values) =
    Array.foldr (\( _, val ) acc -> val :: acc) [] values


{-| Convert a `Reorderable a` to a `List (String, a)`, useful for eventually
rendering a `Html.Keyed` node.

    fromList [ "a", "b", "c" ]
        |> toKeyedList
    --> [ ( "0", "a" )
    --> , ( "1", "b" )
    --> , ( "2", "c" )
    --> ]

This retains the key during swap/insertAt/drop/move\*/.. operations, so that your
`Html.Keyed` node can work correctly.

-}
toKeyedList : Reorderable a -> List ( String, a )
toKeyedList (Reorderable _ values) =
    Array.foldr (\( key, val ) acc -> ( String.fromInt key, val ) :: acc) [] values


{-| Initialize a `Reorderable a` from a `List a`. Useful for initializing data
and decoding with `Json.Decode.map Reorderable.fromList`.
-}
fromList : List a -> Reorderable a
fromList values =
    List.foldl push empty values
