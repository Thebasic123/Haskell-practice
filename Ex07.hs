{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Ex07 where

data Nat = Z | S Nat

type family (+) (n :: Nat) (m :: Nat) :: Nat
type instance Z     + m = m
type instance (S n) + m = S (n + m)

type family (*) (n :: Nat) (m :: Nat) :: Nat
-- IMPLEMENT TYPE INSTANCES
type instance  Z * Z = Z
type instance Z     * m = Z
type instance (S n) * m = m + (n * m)

data Vec (n :: Nat) a where
  VNil  :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

deriving instance Show a => Show (Vec n a)
deriving instance Eq a   => Eq   (Vec n a)

-- IMPLEMENT headV
headV :: Vec (n::Nat) a -> a
headV VNil = error "Vector is empty"
headV (VCons head xs) = head  


appendV :: Vec n a -> Vec m a -> Vec (n + m) a
appendV VNil         ys = ys
appendV (VCons x xs) ys = VCons x (appendV xs ys)

-- IMPLEMENT concatV
concatV :: Vec n (Vec m a) -> Vec (n*m) a
concatV VNil = VNil
concatV (VCons x xs) = appendV x (concatV xs)
-- I googled concat, and looked Haskell lib code for this part