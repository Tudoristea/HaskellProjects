module Crypto where

import Data.Char

import Prelude hiding (gcd)

{-
The advantage of symmetric encryption schemes like AES is that they are efficient
and we can encrypt data of arbitrary size. The problem is how to share the key.
The flaw of the RSA is that it is slow and we can only encrypt data of size lower
than the RSA modulus n, usually around 1024 bits (64 bits for this exercise!).

We usually encrypt messages with a private encryption scheme like AES-256 with
a symmetric key k. The key k of fixed size 256 bits for example is then exchanged
via the aymmetric RSA.
-}
-- a ^ (-1) -- inverse is u mod m
-------------------------------------------------------------------------------
-- PART 1 : asymmetric encryption

gcd :: Int -> Int -> Int
gcd m n
    | n == 0 = m
    | otherwise = gcd n (m `mod` n)

phi :: Int -> Int
phi mRange
  = length [ x | x <- [ 1.. mRange] , gcd x mRange == 1]

     -- length [x|x<-xs, f(x)]

     -- Calculates (u, v, d) the gcd (d) and Bezout coefficients (u and v)
-- such that au + bv = d
computeCoeffs :: Int -> Int -> (Int, Int)
computeCoeffs a b
  | b == 0 = (1, 0)
  | otherwise = (v, (u - q * v))
    where 
      (q, r) = quotRem a b 
      (u, v) = computeCoeffs b r


    
-- Inverse of a modulo m
inverse :: Int -> Int -> Int
inverse a m
   | gcd a m /= 1 = undefined
   | otherwise = u `mod` m
      where 
       (u,_) = computeCoeffs a m 

        -- Calculates (a^k mod m)


modPow :: Int -> Int -> Int -> Int
modPow a k m
  | k == 0         = 1 `mod` m
  | k `mod` 2 == 0 = (modPow ((a * a) `mod` m) (k `div` 2) m) 
  | otherwise      = ((a `mod` m) * (modPow a (k-1) m)) `mod` m


-- modPow1 :: Int -> Int -> Int -> Int
-- modPow1 a k m
 -- | k == 0         = 1
 -- | k `div` 2 == 0 = ((((a `mod` m) * (a `mod` m)) `mod` m) * modPow1 a (k`div`4) m) `mod` m
 -- | otherwise      = ((a `mod` m) * (((a `mod` m) * (a `mod` m)) `mod` m) * modPow1 a (k`div`4) m) `mod` m


-- modPow5 :: Int -> Int -> Int -> Int
-- modPow5 a k m
 -- | k == 0         = 1
 -- | k `mod` 2 == 0 = (((a * a) `mod` m) * modPow5 a (k `div` 2) m ) `mod` m
 -- | otherwise      = (a * modPow5 a ((k - 1) `div` 2) m) `mod` m

-- modPow4 :: Int -> Int -> Int -> Int
-- modPow4 a k m
 --  | k == 0         = 1
  -- | k `mod` 2 == 0 = modPow4 (a * a) (k `div` 2) m `mod` m
  -- | otherwise      = a * modPow4 (a * a) ((k - 1)`div` 2) m `mod` m   


--modPow2 :: Int -> Int -> Int -> Int
--modPow2 a k m 
 -- | k == 0         = 1
 -- | k `mod` 2 == 0 = ((a * a) `mod` m * modPow2 (a * a) (k `div` 2) m) `mod` m
 -- | otherwise      = (a * modPow2 a (k - 1) m `mod` m )

            -- kept all previous fails as reference
  
--modPow  :: Int -> Int -> Int -> Int
--modPow a k m 
--  | k == 0         = 1
 -- | k `mod` 2 == 0 = modPow ((a `mod` m) * (a `mod` m)) (k `div` 2) m `mod` m
--  | otherwise      = (((a `mod` m) * modPow ((a `mod` m) * (a `mod` m)) (k `div` 2) m) `mod` m) `mod` m


-- modPow3 :: Int -> Int -> Int -> Int
-- modPow3 a k m
 -- | k `mod` 2 == 0 = (a ^ 2 `mod` m) ^ (k `div` 2) `mod` m
 -- | otherwise = a * ((a ^ 2 `mod` m) ^ ((k - 1) `div` 2) `mod` m) `mod` m
  
--we can also do the more general case = a ^ k `mod` m but it is not as reliable
  
-- Returns the smallest integer that is coprime with phi
smallestCoPrimeOf :: Int -> Int
smallestCoPrimeOf coPr
  = smallestCoPrimeOf' coPr 2
   where
     smallestCoPrimeOf' coPr b
       | gcd coPr b == 1 = b
       | otherwise       = smallestCoPrimeOf' coPr (b + 1)
  

-- Generates keys pairs (public, private) = ((e, n), (d, n))
-- given two "large" distinct primes, p and q
genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
genKeys p q 
  = ((e, n), (d, n))
    where 
      n     = p * q
      nStar = (p - 1) * (q - 1) -- nStar should be equal to phi n but doesn't work. Why?
      e     = smallestCoPrimeOf nStar
      d     = inverse e nStar


 -- Alternative I tried to make, without using the external functions so much
 -- Saw some weird crashes so decided to switch to the one above
 -- genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
-- genKeys p q 
--  = ((genKeysE n 2, n) , (inverse (genKeysE n 2) (phi n), n))
   -- where
     -- n = p * q 
     -- genKeysE n e'
       -- | gcd e' (phi n) == 1 = e'
       -- | otherwise            = genKeysE n (e' + 1)
     
        -- Tried to make genKeys with 2 separate functions
       -- genKeysD e d'
      -- | (e * d') `mod` phi n == 1 = d' 
      -- | otherwise                = genKeysD e (d' + 1)

-- RSA encryption/decryption
rsaEncrypt :: Int -> (Int, Int) -> Int
rsaEncrypt txt (e, n)
  = modPow txt e n

rsaDecrypt :: Int -> (Int, Int) -> Int
rsaDecrypt done (d, n)
  = modPow done d n

-------------------------------------------------------------------------------
-- PART 2 : symmetric encryption

-- Returns position of a letter in the alphabet
-- Pre - condition c is a letter
toInt :: Char -> Int
toInt c
  | ord 'a' <= ord c && ord c <= ord 'z' = ord c - ord 'a' 
  | ord 'A' <= ord c && ord c <= ord 'Z' = ord c - ord 'A'

-- Returns the n^th letter
toChar :: Int -> Char
toChar p
  = chr (p + (ord 'a'))

-- "adds" two letters
add :: Char -> Char -> Char
add
  = undefined

-- "substracts" two letters
substract :: Char -> Char -> Char
substract
  = undefined

-- the next functions present
-- 2 modes of operation for block ciphers : ECB and CBC
-- based on a symmetric encryption function e/d such as "add"

-- ecb (electronic codebook) with block size of a letter
--
ecbEncrypt :: Char -> String -> String
ecbEncrypt
  = undefined

ecbDecrypt :: Char -> String -> String
ecbDecrypt
  = undefined

-- cbc (cipherblock chaining) encryption with block size of a letter
-- initialisation vector iv is a letter
-- last argument is message m as a string
--
cbcEncrypt :: Char -> Char -> String -> String
cbcEncrypt
  = undefined

cbcDecrypt :: Char -> Char -> String -> String
cbcDecrypt
  = undefined
