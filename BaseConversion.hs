{-# LANGUAGE BinaryLiterals #-}

module BaseConversion where

import Data.List(lookup)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

-- This module has functions to convert between binary, octal, hexadecimal, decimal
-- The input is a number (constrained to be Integral). For binary, octal, and
-- hexadecimal use the 0b, 0O, and 0x prefixes.
-- The output is a string representing the digits of the converted number so we
-- can use 'a' through 'f' in hexadecimal numbers.
-- Since the output string uses the prefixes for binary, octal, and haxadecial,
-- we can turn into a number with read (doesn't seem to work for binary literals)

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
    2         -> "0b"
    8         -> "0O"
    16        -> "0x"
    otherwise -> ""

-- Convert a number to its representation in the specified base, and display as
-- a String with the base prefix ("Ob", "0O", "0x").
convertToBase :: Integral a => a -> a -> String
convertToBase base num = prefix base ++ getDigits base num []

-- Return the list of digits to represent a number in the specified base.
-- Use an accumulator to avoid reversing the list.
getDigits :: Integral a => a -> a -> String -> String
getDigits b 0 acc = acc
getDigits b n acc =
  let (q,r) = divMod n b
  in getDigits b q ((showDigit r):acc)

-- Convert a numeric "digit" (the quotient from divMod) to a Char. 10 through 15 are
-- translated into 'a' through 'f'
showDigit :: Integral a => a -> Char
showDigit d =
  case lookup d mapping of
    Just c -> c
    Nothing -> ' '   -- This should never happen, but output a space on error is sensible
  where
    mapping = zip [0..15] $ (map (head . show) [0..9]) ++ ['a'..'f']


-- As an alternative, use functions from Numeric and Data.Char to convert to Base

convertToBase' :: (Integral a, Show a) => a -> a -> String
convertToBase' b n =
  showIntAtBase b intToDigit n ""
