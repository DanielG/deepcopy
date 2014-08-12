{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Generics.DeepCopy.TH.Class where

import Control.Monad
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.C.Error

import Generics.DeepCopy

class DeepCopyTH a where
    deepCopyTH :: a -> IO a
    deepCopyTH = return

instance DeepCopyTH a => DeepCopy a where
    deepCopy = deepCopyTH

instance DeepCopyTH Bool
instance DeepCopyTH Char
instance DeepCopyTH Double
instance DeepCopyTH Float
instance DeepCopyTH Int
instance DeepCopyTH Int8
instance DeepCopyTH Int16
instance DeepCopyTH Int32
instance DeepCopyTH Int64
instance DeepCopyTH Integer
instance DeepCopyTH Ordering
instance DeepCopyTH Word
instance DeepCopyTH Word8
instance DeepCopyTH Word16
instance DeepCopyTH Word32
instance DeepCopyTH Word64
instance DeepCopyTH ()
instance DeepCopyTH CUIntMax
instance DeepCopyTH CIntMax
instance DeepCopyTH CUIntPtr
instance DeepCopyTH CIntPtr
instance DeepCopyTH CSUSeconds
instance DeepCopyTH CUSeconds
instance DeepCopyTH CTime
instance DeepCopyTH CClock
instance DeepCopyTH CSigAtomic
instance DeepCopyTH CWchar
instance DeepCopyTH CSize
instance DeepCopyTH CPtrdiff
instance DeepCopyTH CDouble
instance DeepCopyTH CFloat
instance DeepCopyTH CULLong
instance DeepCopyTH CLLong
instance DeepCopyTH CULong
instance DeepCopyTH CLong
instance DeepCopyTH CUInt
instance DeepCopyTH CInt
instance DeepCopyTH CUShort
instance DeepCopyTH CShort
instance DeepCopyTH CUChar
instance DeepCopyTH CSChar
instance DeepCopyTH CChar
instance DeepCopyTH Errno
