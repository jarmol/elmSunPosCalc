module AiSieve exposing (sieve)

sieve : Int -> List Int
sieve limit =
    let
        numbers =
            List.range 2 limit

        removeMultiples p ns =
            List.filter (\n -> remainderBy p n /= 0) ns

        siever ns =
            case ns of
                [] ->
                    []

                p :: rest ->
                    p :: siever (removeMultiples p rest)
    in
    siever numbers
