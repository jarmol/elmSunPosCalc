module DecimalFormat exposing (cutDec3, cutDec6)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), Locale, usLocale)


sharesLocale : Locale
sharesLocale =
    { usLocale
        | decimals = Exact 6
        , negativePrefix = "-"
        , positivePrefix = " "
    }


sharesLocale3 : Locale
sharesLocale3 =
    { sharesLocale
        | decimals = Exact 3
    }


cutDec6 : Float -> String
cutDec6 x =
    format sharesLocale x


cutDec3 : Float -> String
cutDec3 x =
    format sharesLocale3 x
