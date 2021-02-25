module Image.Internal.Util exposing (indexedFoldl)

import Array exposing (Array)


indexedFoldl : (Int -> a -> acc -> acc) -> acc -> Array a -> acc
indexedFoldl func acc list =
    Tuple.second (Array.foldl (indexedFoldlStep func) ( 0, acc ) list)


indexedFoldlStep : (Int -> b -> a -> c) -> b -> ( Int, a ) -> ( Int, c )
indexedFoldlStep fn x ( i, thisAcc ) =
    ( i + 1, fn i x thisAcc )
