{- Compare our custom implementations with those provided by Data.Split -}
module Main where

import Criterion.Main
import qualified Data.List.Split as S
import qualified CustomPrelude   as C

list :: [Int]
list = take 1000 $ cycle [1..100]

main = defaultMain [
         bench "Data.List.splitWhen: " $ nf (S.splitWhen even) list
       , bench "Custom    splitWhen: " $ nf (C.splitWhen even) list
       , bench "Data.List.splitOn: " $ nf (S.splitOn [10]) list
       , bench "Custom    splitOn: " $ nf (C.splitOn [10]) list
       ]

