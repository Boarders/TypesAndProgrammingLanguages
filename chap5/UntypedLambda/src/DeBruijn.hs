{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module DeBruijn where

import Prelude hiding (cycle)

data DBLTerm where
  DVar :: Int      -> DBLTerm
  DAbs :: DBLTerm -> DBLTerm
  DApp :: DBLTerm -> DBLTerm -> DBLTerm

printD :: DBLTerm -> String
printD = \case
  DVar n
    -> show n
  DAbs bd
    -> "\\ " ++ printD bd
  DApp t1 t2
    ->
      " " ++ printD t1 ++ " " ++
      " " ++ printD t2 ++ " "

instance Show DBLTerm where
  show = printD



--                - to be read as substituting nameless var n for lambda term sub
--                |
substitute :: (Int, DBLTerm) -> DBLTerm -> DBLTerm
substitute (n, sub) = \case
  DVar m | n == m
    -> sub
  DVar m
    -> DVar m
  DAbs t
    -> DAbs $ substitute (n + 1, cycleSucc sub) t
  DApp t1 t2
    ->
      DApp
        (substitute (n, sub) t1)
        (substitute (n, sub) t2)

-- This is cycles every term above a threshold by a given amount
-- taking into consideration how far our term is within a binding
-- tree.

--        - cycle amount
--        |
--        |       - cycle threshold
--        |       |
cycle :: Int ->  Int -> DBLTerm -> DBLTerm
cycle n k = \case
  DVar v
    ->  DVar $ shift n k v
  DApp l1 l2
    ->  DApp (cycle n k l1) (cycle n k l2)
  DAbs l1
    ->  DAbs $ cycle n (k + 1) l1

  where
    shift :: Int -> Int -> Int -> Int
    shift  n k arg
      | arg < k    = arg
      | otherwise = arg + n

cycleSucc = cycle  1 0
cyclePred = cycle (-1) 0

--             -- term to be substituted
--             |
topSubst :: DBLTerm -> DBLTerm -> DBLTerm
topSubst sub body  =
  cyclePred $ substitute (0, cycleSucc sub) body

isVal :: DBLTerm -> Bool
isVal = \case
  DAbs _
    -> True
  DVar _
    -> True
  _
    -> False


smallStep :: DBLTerm -> Maybe DBLTerm
smallStep = \case
  DApp (DAbs body) sub
    -> pure $ topSubst sub body
  DApp t1 t2 | isVal t1
    -> DApp t1 <$> (smallStep t2)
  DApp t1 t2
    -> DApp <$> (smallStep t1) <*> pure t2
  _ -> Nothing

eval :: DBLTerm -> DBLTerm
eval t0 = case (smallStep t0) of
  Nothing -> t0
  Just t1 -> eval t1

zero = DAbs $ DAbs $ DVar 0

succD :: DBLTerm -> DBLTerm
succD t = let t' = eval (DApp (DApp t s) z) in
    DAbs  $ DAbs $ DApp (DVar 1) t'
  where s = DVar 1
        z = DVar 0
