module Image.Internal.Array2d exposing (Array2d, apply, fromArray, fromList, fromList2D, last, lastLength, part, push)

import Array exposing (Array)
import Image.Internal.Util as Util


type alias Array2d a =
    Array (Array a)


part : Int -> Int -> Int -> Int -> Array2d a -> Array2d a
part sx sy sw sh arr =
    let
        arrW =
            lastLength arr

        arrH =
            Array.length arr

        newSw =
            min arrW (sx + sw)

        newSh =
            min arrH (sy + sh)
    in
    if arrW < sx || arrH < sy then
        arr

    else
        part_ sx sy (Array.push Array.empty Array.empty) sx sy newSw newSh arr


part_ x y acc sx sy sw sh arr =
    let
        newAcc_ =
            Array.get y arr
                |> Maybe.andThen (\row -> Array.get x row |> Maybe.map (\a -> push a acc))
                |> Maybe.withDefault acc

        ( newX, newY, newAcc ) =
            if (x + 1) >= sw then
                ( sx, y + 1, Array.push Array.empty newAcc_ )

            else
                ( x + 1, y, newAcc_ )
    in
    if (y + 1) >= sh then
        acc

    else
        part_ newX newY newAcc sx sy sw sh arr


apply : Int -> Int -> Array2d a -> Array2d a -> Array2d a
apply sx sy from to =
    Util.indexedFoldl
        (\y fromRow acc1 ->
            case Array.get (y + sy) acc1 of
                Just toRow ->
                    Array.set (y + sy)
                        (Util.indexedFoldl ((+) sx >> Array.set) toRow fromRow)
                        acc1

                Nothing ->
                    acc1
        )
        to
        from


fromArray : Int -> Array a -> Array2d a
fromArray w arr =
    fromArray_ w arr Array.empty


fromArray_ : Int -> Array a -> Array (Array a) -> Array2d a
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


fromList : Int -> List a -> Array2d a
fromList w l =
    fromList_ w l (Array.push Array.empty Array.empty)


fromList_ : Int -> List a -> Array2d a -> Array2d a
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


fromList2D : List (List a) -> Array2d a
fromList2D =
    List.foldl (Array.fromList >> Array.push) Array.empty


push : a -> Array2d a -> Array2d a
push item arr =
    Array.get (lastIndex_ arr) arr
        |> Maybe.map (\arr2 -> Array.set (lastIndex_ arr) (Array.push item arr2) arr)
        |> Maybe.withDefault arr


last : Array2d a -> Maybe a
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
