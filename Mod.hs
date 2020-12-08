{-# language RoleAnnotations #-}
{-# language GADTs #-}
{-# language DataKinds #-}
{-# language InstanceSigs #-}
{-# language ScopedTypeVariables #-}
{-# language DerivingStrategies #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language StandaloneDeriving #-}
module Mod (Mod, mod) where

import Prelude hiding (mod)
import qualified Prelude
import GHC.TypeLits (KnownNat, Nat)
import qualified GHC.TypeLits as Nat
import Data.Kind
import Data.Singletons

data Mod :: Nat -> Type -> Type where
  Mod :: forall n m . Sing n -> m -> Mod n m

instance Eq m => Eq (Mod n m) where
  Mod _ m == Mod _ m' = m == m'
  Mod _ m /= Mod _ m' = m /= m'

deriving stock instance Show m => Show (Mod n m)

mod :: forall n m . KnownNat n => Integral m => m -> Mod n m
mod m = Mod @n Sing $ Prelude.mod m (fromInteger $ Nat.natVal @n Proxy)

instance (KnownNat n, Integral m, Show m) => Num (Mod n m) where
  fromInteger = mod . fromInteger
  Mod _ a + Mod _ b = mod $ a + b
  Mod _ a - Mod _ b = mod $ a - b
  Mod _ a * Mod _ b = mod $ a * b
  abs = id
  signum = const 1
