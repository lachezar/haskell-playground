module Throwaway where

import Data.List (dropWhile, dropWhileEnd)
import Data.Char (isSpace)

data Customer = MkCustomer
  { customerId :: Int
  , customerFirstName :: String
  , customerLastName :: String
  , customerEmail :: String
  , customerBirthYear :: Int
  , customerCity :: String
  , customerCountry :: String
  } deriving (Eq, Show, Ord)

removeSpaces :: String -> String
removeSpaces s = dropWhileEnd (\x -> isSpace x) (dropWhile (\x -> isSpace x) s)

cleanupName :: Customer -> Customer
cleanupName MkCustomer{customerId=cid, customerFirstName=fn, customerLastName=ln, customerEmail=e, customerBirthYear=by, customerCity=cy, customerCountry=co} =
  MkCustomer
    { customerId = cid
    , customerFirstName = (removeSpaces fn)
    , customerLastName = (removeSpaces ln)
    , customerEmail = e
    , customerBirthYear = by
    , customerCity = cy
    , customerCountry = co
    }

cleanupName' :: Customer -> Customer
cleanupName' cust@MkCustomer{customerFirstName=fn, customerLastName=ln} =
    cust
    { customerFirstName = (removeSpaces fn)
    , customerLastName = (removeSpaces ln)
    }