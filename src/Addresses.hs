module Addresses
    ( spaceOrStop
    , spaceOrStop'
    , spaceOrComma
    , spaceOrComma'
    , skipUntil
    , takeUntil
    , cardinalPoints
    , states
    , streetTypes
    , aPoint
    , aState
    , aStreetType
    , postcodeEOF
    , postcodeS
    , postcode
    , foundPoint
    , the
    , skipUntilN
    , takeUntilN

    , Suffix
    , Prefix
    , Number
    , Single
    , StreetNumber
    , singleNumber
    , singleFix
    , single
    , oneNumber
    , rangeNumber
    , streetNumber

    , Box
    , StreetName
    , StreetType
    , Pobox(..)
    , StreetAddress
    , AddressLocation(..)
    , Address
    , addressLocation
    , step
    , poBox
    , streetAddress

    , Suburb(..)
    , Postcode(..)
    , State(..)
    -- , Locality(..)
    ) where

import           Address
import           Components
import           Locality
import           StreetNumber
