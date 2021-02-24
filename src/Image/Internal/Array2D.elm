module Image.Internal.Array2D exposing (Array2D, fromArray, fromList, fromList2D, last, lastLength, push)

import Array exposing (Array)


type alias Array2D a =
    Array (Array a)


fromArray : Int -> Array a -> Array2D a
fromArray w arr =
    fromArray_ w arr Array.empty


fromArray_ : Int -> Array a -> Array (Array a) -> Array2D a
fromArray_ w arr acc =
    if Array.length arr > w then
        let
            ( a1, a2 ) =
                splitAt w arr
        in
        fromArray_ w a2 (Array.push a1 acc)

    else
        Array.push arr acc


splitAt : Int -> Array a -> ( Array a, Array a )
splitAt index xs =
    let
        len =
            Array.length xs
    in
    case ( index > 0, index < len ) of
        ( True, True ) ->
            ( Array.slice 0 index xs, Array.slice index len xs )

        ( True, False ) ->
            ( xs, Array.empty )

        ( False, True ) ->
            ( Array.empty, xs )

        ( False, False ) ->
            ( Array.empty, Array.empty )


fromList : Int -> List a -> Array2D a
fromList w l =
    fromList_ w l (Array.push Array.empty Array.empty)


fromList_ : Int -> List a -> Array2D a -> Array2D a
fromList_ w l acc =
    case l of
        a :: rest ->
            let
                newAcc =
                    applyIf (lastLength acc >= w) (Array.push Array.empty) acc
            in
            fromList_ w rest (push a newAcc)

        [] ->
            acc


fromList2D : List (List a) -> Array2D a
fromList2D =
    List.foldl (Array.fromList >> Array.push) Array.empty


push : a -> Array2D a -> Array2D a
push item arr =
    Array.get (lastIndex_ arr) arr
        |> Maybe.map (\arr2 -> Array.set (lastIndex_ arr) (Array.push item arr2) arr)
        |> Maybe.withDefault arr


last : Array2D a -> Maybe a
last arr =
    Array.get (lastIndex_ arr) arr |> Maybe.andThen (\subArr -> Array.get (lastIndex_ subArr) subArr)


lastLength : Array (Array a) -> Int
lastLength arr =
    Array.get (lastIndex_ arr) arr
        |> Maybe.map Array.length
        |> Maybe.withDefault 0


lastIndex_ : Array a -> Int
lastIndex_ arr =
    Array.length arr - 1


applyIf : Bool -> (a -> a) -> a -> a
applyIf bool f a =
    if bool then
        f a

    else
        a
