module Val where
-- this file contains definitions for Val and aux functions

import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Data.Char   


-- The values manipulated by FORTH
data Val = Integer Int 
    | Real Float
    | Id String
    deriving (Show, Eq)

-- Converts a string to Val 
-- sequence tried is Integer, Float, String
strToVal :: String -> Val
strToVal s = case readMaybe s :: Maybe Int of
    Just i -> Integer i
    Nothing -> case readMaybe s :: Maybe Float of
        Just f -> Real f 
        Nothing -> Id s

-- Converts to Float if Real or Integer, error otherwise
-- Used to deal with arguments of operators
toFloat :: Val -> Float
toFloat (Real x) = x
toFloat (Integer i) = fromIntegral i     
toFloat (Id _) = error "Not convertible to float"

toString :: Val -> String
toString (Real x) = show x 
toString (Integer i) = show i
toString (Id str) = str