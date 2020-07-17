module Sequences where

import Data.Char (ord, chr)

maxOf2 :: Int -> Int -> Int
-- Returns first argument if it is larger than the second,
-- the second argument otherwise
maxOf2 x y 
    | x > y    = x
    | otherwise = y

maxOf3 :: Int -> Int -> Int -> Int
-- Returns the largest of three Ints
maxOf3 x y z = 
	x `maxOf2` y  `maxOf2`  z	
-- we can also use  : if maxOf2 x y < z then z else maxOf2 x y, this compares the biggest number out of the first 2 with the third

isADigit :: Char -> Bool
-- Returns True if the character represents a digit '0'..'9';
-- False otherwise
-- isADigit c = error "TODO: implement isADigit"
-- False otherwise
isADigit d
   = d >= '0' && d<= '9'




isAlpha :: Char -> Bool
-- Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
-- isAlpha c = error "TODO: implement isAlpha"
isAlpha alp
	= alp >= 'a' && alp <= 'z' || alp >= 'A' && alp <= 'Z'




digitToInt :: Char -> Int
-- Pre: the character is one of '0'..'9'
-- Returns the integer [0..9] corresponding to the given character.
-- Note: this is a simpler version of digitToInt in module Data.Char,
-- which does not assume the precondition.
-- digitToInt c = error "TODO: implement digitToInt"
digitToInt dig
	| dig >= '0' && dig <= '9' = ord dig - ord '0'
	| otherwise = -1
-- Same thing as ord dig - 48





toUpper :: Char -> Char
-- Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.
-- toUpper c = error "TODO: implement toUpper"

toUpper digit 
	| digit >= 'a' && digit <= 'z' = chr (ord digit - ord 'a' + ord 'A')
	| digit >= 'A' && digit <= 'Z' = digit
	| otherwise 		       = '!'




--
-- Sequences and series
--

-- Arithmetic sequences
arithmeticSeq :: Double -> Double -> Int -> Double
--arithmeticSeq a d n = error "TODO: implement arithmeticSeq"
arithmeticSeq a d n	
	= a + fromIntegral n * d




-- Geometric sequence
geometricSeq :: Double -> Double -> Int -> Double
-- geometricSeq a r n = error "TODO: implement geometricSeq"
geometricSeq a r n 
	= a * r ^ n

		--  it overflows : power r n
			-- where
  			-- power :: Double -> Int -> Double
			-- power z x -- z is r and x is n, just to avoid scope
			--	| x==0 = 1
			--	 | otherwise = z * power z x-1
 		






-- Arithmetic series
arithmeticSeries :: Double -> Double -> Int -> Double
--arithmeticSeries a d n = error "TODO: implement arithmeticSeries"
arithmeticSeries a d n 
	= fromIntegral ( n + 1 ) * (a + ( fromIntegral n * d ) / 2)






-- Geometric series
geometricSeries :: Double -> Double -> Int -> Double
-- geometricSeries a r n = error "TODO: implement geometricSeries"
geometricSeries a r n
	| r == 1 = a * fromIntegral (n+1)
	| otherwise = a * (1 - r^(n+1))/(1-r)


