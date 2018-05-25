{-# LANGUAGE GADTs #-}
module ArithBool where

data Term where
  T      :: Term
  F      :: Term
  If     :: Term -> Term -> Term -> Term
  Zero   :: Term
  Succ   :: Term -> Term
  Pred   :: Term -> Term
  IsZero :: Term -> Term
    deriving (Show, Eq)

boolVal :: Term -> Bool
boolVal t = case t of
  T -> True
  F -> True
  _ -> False

numVal :: Term -> Bool
numVal t = case t of
  Zero   -> True
  Succ n -> numVal n
  _      -> False

isVal :: Term -> Bool
isVal = (||) <$> boolVal <*> numVal

smallStepEval :: Term -> Maybe Term
smallStepEval t = case t of
  _ -> undefined

bigStepEval :: Term -> Term
bigStepEval term = case term of
  T -> T
  F -> F
  Zero -> Zero
  Succ n -> Succ (bigStepEval n)
  Pred n -> case n of
              Zero -> Zero
              Succ n' | numVal n' ->  (bigStepEval n')
              t    -> Pred (bigStepEval t)
  IsZero t -> case bigStepEval t of
                Zero   -> T
                Succ n | numVal n  -> F
  If t1 t2 t3 -> case bigStepEval t1 of
                   T  ->  bigStepEval t2
                   F  -> bigStepEval t3
