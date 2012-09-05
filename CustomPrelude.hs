{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP, BangPatterns #-}
module CustomPrelude
  ( module BasicPrelude

  -- Applicative operators for monads
  , (<&>), (<@>)

  -- Splitting variants
  , splitOn

  -- Fold variants
  , foldlStrict
  , foldlMaybe
  , foldlStrictMaybe

  -- Misc
  , (.:)
  , bool

  -- Math
  , nextPowerOf2

  ) where

import BasicPrelude
import Data.Bits ((.|.), shiftR, bitSize)


------------------------------------------
-- LITTLE BIT OF APPLICATIVE FOR MONADS --
------------------------------------------

-- Operators equivalent to those in Applicative
-- Defined for all Monads

-- Equivalent to <*>
infixl 4 <&>
(<&>) :: Monad m => m (a -> b) -> m a -> m b
(<&>) = ap

-- Equivalent to <$>
infixl 4 <@>
(<@>) :: Monad m => (a -> b) -> m a -> m b
(<@>) f g = return f <&> g


-------------------------------------------------------
-- UTILITY FUNCTIONS THAT SHOULD HAVE BEEN IN PRELUDE--
-------------------------------------------------------

---------------
-- SPLITTING --
---------------

-- Split lists at delimiter
-- Provides the most common use case for splitting lists
--  without adding a dependency on the split package
-- NOTE: Drops empty groups (similar to `words`)
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s = case dropWhile (==c) s of
  [] -> []
  s' -> w : splitOn c s''
    where (w, s'') = break (==c) s'


-------------
-- FOLDING --
-------------

-- A Standard strict version of foldl
foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f = lgo
  where
    lgo z []     = z
    lgo z (x:xs) = let z' = f z x in z' `seq` lgo z' xs

-- Specialised foldl' with short circuit evaluation
-- A Nothing stops processing for the rest of the list
foldlMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldlMaybe f z0 = lgo (Just z0)
  where
    lgo Nothing  _      = Nothing
    lgo z        []     = z
    lgo (Just z) (x:xs) = lgo (f z x) xs

-- Strict version of specialised foldl' with short circuit evaluation
foldlStrictMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldlStrictMaybe f z0 = lgo (Just z0)
  where
    lgo Nothing  _      = Nothing
    lgo z        []     = z
    lgo (Just z) (x:xs) = let z' = f z x in z' `seq` lgo z' xs


----------
-- MISC --
----------

-- Seamless composition of a one and a two arg function
-- (for point free programming)
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- Bool deconstructor in the spirit of 'either' and 'maybe'
-- Similar to the lambda-if proposal
bool :: a -> a -> Bool -> a
bool a b p = if p then a else b


----------------
-- MATH STUFF --
----------------

-- Computes the next power of two for integers
-- Works only on a 32/64 bit machine (is there any other kind?)
nextPowerOf2 :: Int -> Int
nextPowerOf2 0 = 1
nextPowerOf2 !n =
    let !n1 = n - 1
        !n2 = n1 .|. (n1 `shiftR` 1)
        !n3 = n2 .|. (n2 `shiftR` 2)
        !n4 = n3 .|. (n3 `shiftR` 4)
        !n5 = n4 .|. (n4 `shiftR` 8)
        !n6 = n5 .|. (n5 `shiftR` 16)
        !n7 = if bitSize (undefined :: Int) == 32
                then n6
                else n6 .|. (n6 `shiftR` 32)
    in n7 + 1

