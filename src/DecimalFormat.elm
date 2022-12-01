module DecimalFormat exposing (cutDec3, cutDec6)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), Locale, usLocale)

sharesLocale =
    { usLocale
       | decimals = Exact 6
        , negativePrefix = "-"
        , positivePrefix = " "
        
    }
    

sharesLocale3 =
    { sharesLocale
        | decimals = Exact 3
    }

cutDec6 x =
   format sharesLocale x

cutDec3 x =
   format sharesLocale3 x

