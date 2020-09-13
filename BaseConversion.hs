{-# LANGUAGE BinaryLiterals #-}

module BaseConversion where

import Data.List(lookup)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

-- Conversion to binary, octal, hexadecimal, decimal
-- For the number to be converted, write as a biary, octal,
-- hexadecimal, or decimal literal with 0b, 0O, 0x prefixes
toBinary :: Integral a => a -> String
toBinary = convertToBase 2

toOctal :: Integral a => a -> String
toOctal = convertToBase 8

toDecimal :: Integral a => a -> String
toDecimal = convertToBase 10

toHex :: Integral a => a -> String
toHex = convertToBase 16

prefix :: Integral a => a -> String
prefix base =
  case base of
    2 -> "0b"
    8 -> "0O"
    16 -> "0x"
    otherwise -> ""

convertToBase :: Integral a => a -> a -> String
convertToBase base num = (prefix base) ++ (map showDigit $ getDigits base num [])

getDigits :: Integral a => a -> a -> [a] -> [a]
getDigits b 0 acc = acc
getDigits b n acc =
  let (q,r) = divMod n b
  in (getDigits b q (r:acc))

showDigit :: Integral a => a -> Char
showDigit d =
  case lookup d mapping of
    Just c -> c
    Nothing -> ' '   -- This should never happen, but output a space on error is sensible
  where
    mapping = zip [0..15] $ (map (head . show) [0..9]) ++ ['a'..'f']

-- Use functions from Numeric and Data.Char

convertToBase' :: (Integral a, Show a) => a -> a -> String
convertToBase' b n =
  showIntAtBase b intToDigit n ""
