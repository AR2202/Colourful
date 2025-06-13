{-# LANGUAGE OverloadedStrings #-}

module Backtranspiler
  ( invertMap,
    backtranspile,
  )
where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Parser

swap :: (b, a) -> (a, b)
swap (a, b) = (b, a)

invertMap :: (Ord k) => M.Map a k -> M.Map k a
invertMap m = M.fromList $ map swap $ M.toList m

backtranspile ::  M.Map String SKI -> SKI -> String
backtranspile cDict (App ski1 ski2) = case M.lookup (App ski1 ski2) skiDict of 
  Just colour -> colour
  Nothing -> "Can't transpile"
  where
    skiDict = invertMap cDict
backtranspile cDict ski = M.findWithDefault "Can't transpile" ski skiDict 
    where skiDict = invertMap cDict