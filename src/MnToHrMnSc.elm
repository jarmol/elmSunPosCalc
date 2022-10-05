module MnToHrMnSc exposing (mnToHrMn)

-- Converts minutes to hh:mm:ss
-- also negative minutes, example -10 -> 23:50
-- also decimal part to seconds, example mnToHrMn 1435.25 -> 23:55:15


mnToHrMn : Float -> String
mnToHrMn mns =
    let
        mna : Float
        mna =
            if mns >= 0 then
                mns

            else
                1440 + mns

        lmins : Int
        lmins =
            remainderBy 60 (floor mna)

        lhrs : Int
        lhrs =
            floor (mna / 60.0)

        lsec : Int
        lsec =
            floor (60 * mna - toFloat (3600 * lhrs + 60 * lmins))
    in
    zeroFill lhrs
        ++ ":"
        ++ zeroFill lmins
        ++ ":"
        ++ zeroFill lsec


zeroFill : Int -> String
zeroFill x =
    if x < 10 then
        "0" ++ String.fromInt x

    else
        String.fromInt x
