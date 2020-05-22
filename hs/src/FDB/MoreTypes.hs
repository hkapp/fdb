module FDB.MoreTypes where

type Date = ()  -- should import from Data.Dates (in dates-0.2.3.0 package though)

dateFromString :: String -> Date
dateFromString = undefined

tryDateFromString :: String -> Maybe Date
tryDateFromString = undefined

data DateInterval = Days Int | Weeks Int | Months Int

minusInterval :: Date -> DateInterval -> Date
minusInterval = undefined

plusInterval :: Date -> DateInterval -> Date
plusInterval = undefined

type Decimal = Double  -- should import Data.Decimal instead

type PhoneNumber = ()
