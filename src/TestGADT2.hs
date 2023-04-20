{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestGADT2 where
import GHC.Exts (Constraint)

-- data Expr a where
--  I   :: Int -> Expr Int
--  B   :: Bool -> Expr Bool
--  Add :: Expr Int -> Expr Int->Expr Int
--  Mul :: Expr Int -> Expr Int->Expr Int
--  Eq  :: Eq a => Expr a -> Expr a -> Expr Bool

-- instance Functor Expr where
--   fmap fv (I a) = let a' = fv a
--                     in I a'
--   fmap fv (B a) = let a' = fv a
--                     in B a'
--   fmap fv (Add a b) = undefined
--   fmap fv (Mul a b) = undefined
--   fmap fv (Eq a b) = undefined

-- data ExprNF :: (* -> Constraint) -> (* -> *) -> * -> * where
--   FMap :: Eq x => (x->a) -> Expr x -> ExprNF Eq Expr a

-- data WhatICan :: * -> * where
--   W :: c -> WhatICan c
--   WW ::  (a -> b) -> c -> WhatICan c

-- instance Functor (ExprNF Eq Expr) where
--    fmap :: (a->b)->ExprNF Eq Expr a -> ExprNF Eq Expr b
--    fmap g (FMap h tx) = FMap (g . h) tx

-- liftNF :: Eq a => Expr a -> ExprNF Eq Expr a
-- liftNF ta = FMap id ta

-- lowerNF :: (forall x . Eq x => (x->a)-> Expr x -> Expr a) -> ExprNF Eq Expr a -> Expr a
-- lowerNF fmp (FMap g tx) = fmp g tx

-- -- fmapExpr :: (Eq a) => (a -> b) -> Expr a -> Expr b
-- -- fmapExpr fv a = lowerNF inner (fmap fv (liftNF a))
-- --   where
-- --     inner :: (Eq a) => (a -> b) -> Expr a -> Expr b
-- --     inner (fv' :: (Int -> Int)) (I a') = let r = fv' a'
-- --                                            in _
-- --     inner (fv' :: (Bool -> Bool)) (B a') = B $ fv' a'
-- --     inner fv' (Add a b) = undefined
-- --     inner fv' (Mul a b) = undefined
-- --     inner fv' (Eq a b) = undefined

-- test :: IO ()
-- test = do
--   let expr1 = I 5
--   let expr2 = fmap fv (liftNF expr1)
--   let (I expr3) = lowerNF lowerFvI expr2
--   print expr3
--   return ()
--   where
--     lowerFvI :: (a -> Int) -> Expr a -> Expr Int
--     lowerFvI f (I a) = I $ f a
--     lowerFv :: (a -> Bool) -> Expr a -> Expr Bool
--     lowerFv f (B a) = B $ f a
--     fv :: Int -> Int
--     fv a = a * a
