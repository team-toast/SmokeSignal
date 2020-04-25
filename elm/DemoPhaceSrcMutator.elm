module DemoPhaceSrcMutator exposing (..)

import Hex
import Random


type alias MutateInfo =
    { place : Int
    , change : Int
    }


mutateInfoGenerator : Random.Generator MutateInfo
mutateInfoGenerator =
    Random.map2
        MutateInfo
        (Random.int 0 39)
        (Random.int -2 2)


addressSrcGenerator : Random.Generator String
addressSrcGenerator =
    Random.list 40
        hexCharGenerator
        |> Random.map String.fromList


hexCharGenerator : Random.Generator Char
hexCharGenerator =
    Random.int 0 15
        |> Random.map
            (Hex.toString
                >> String.toList
                >> List.head
                >> Maybe.withDefault '0'
            )


mutateSrc : MutateInfo -> String -> String
mutateSrc mutateInfo src =
    src
        |> String.toList
        |> List.indexedMap
            (\i c ->
                if i == mutateInfo.place then
                    c
                        |> (List.singleton >> String.fromList)
                        |> Hex.fromString
                        |> Result.withDefault 0
                        |> (+) mutateInfo.change
                        |> max 0
                        |> min 15
                        |> Hex.toString
                        |> String.toList
                        |> List.head
                        |> Maybe.withDefault '0'

                else
                    c
            )
        |> String.fromList
