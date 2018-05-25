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
  T -> Nothing
  F -> Nothing
  If t1 t2 t3 -> case t2 of
                   T -> Just t2
                   F -> Just t3
                   _ -> If <$> (smallStepEval t1) <*> pure t2 <*> pure t3
  Zero    -> Nothing
  Succ t1 -> Succ <$> (smallStepEval t1)
  Pred t  -> case t of
               Zero -> Just Zero
               Succ n | numVal n -> Just n
               _ -> Pred <$> (smallStepEval t)
  IsZero t -> case t of
                Zero -> Just T
                Succ n | numVal n -> Just F
                _ -> IsZero <$> (smallStepEval t)

eval :: Term -> Term
eval t = case smallStepEval t of
  Nothing -> t
  Just t' -> eval t'

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
