module MoreTypes where

type Date = ()

fromString :: String -> Date
fromString = undefined

tryFromString :: String -> Maybe Date
tryFromString = undefined

type Decimal = Double  -- should import Data.Decimal instead

type PhoneNumber = ()
