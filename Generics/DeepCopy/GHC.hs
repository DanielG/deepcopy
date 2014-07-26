{-# LANGUAGE TypeOperators, DefaultSignatures, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, DeriveGeneric, UndecidableInstances #-}
module Generics.DeepCopy.GHC (DeepCopyGHC(..)) where

import GHC.Generics
import Control.Monad
import Control.Applicative

import Control.Concurrent.MVar
import Data.IORef

import Generics.DeepCopy


class GDeepCopy f where
  gdeepCopy :: f a -> IO (f a)


instance GDeepCopy V1 where gdeepCopy x = return x
instance GDeepCopy U1 where gdeepCopy U1 = return U1

instance DeepCopyGHC c => GDeepCopy (K1 i c) where
  gdeepCopy (K1 c) = K1 <$> deepCopy c

instance (GDeepCopy a, GDeepCopy b) => GDeepCopy (a :+: b) where
  gdeepCopy (L1 x) = L1 <$> gdeepCopy x
  gdeepCopy (R1 x) = R1 <$> gdeepCopy x

instance (GDeepCopy a, GDeepCopy b) => GDeepCopy (a :*: b) where
  gdeepCopy (x :*: y) = liftA2 (:*:) (gdeepCopy x) (gdeepCopy y)

instance GDeepCopy f => GDeepCopy (M1 i t f) where
  gdeepCopy (M1 x) = M1 <$> gdeepCopy x

class DeepCopyGHC a where
  deepCopyGHC :: a -> IO a

  default deepCopyGHC :: (Generic a, GDeepCopy (Rep a)) => a -> IO a
  deepCopyGHC = fmap to . gdeepCopy . from


instance DeepCopyGHC a => DeepCopy a where
    deepCopy = deepCopyGHC

instance DeepCopyGHC Int where deepCopyGHC = return

instance (DeepCopyGHC a, DeepCopyGHC b) => DeepCopyGHC (a,b)

instance DeepCopyGHC a => DeepCopyGHC (IORef a) where
  deepCopyGHC = newIORef <=< deepCopyGHC <=< readIORef

instance DeepCopyGHC a => DeepCopyGHC (MVar a) where
  deepCopyGHC = newMVar <=< deepCopyGHC <=< readMVar
