{-# LANGUAGE TemplateHaskell #-}
import Data.Monoid
import Control.Applicative
import Generics.DeepCopy
import Generics.DeepCopy.TH
import Generics.DeepCopy.TH.Ty
import Generics.DeepCopy.TH.Class

import Language.Haskell.TH

import Debug

-- $(deepCopyClass )

instance DeepCopyTH a => DeepCopyTH (Maybe a) where
    deepCopyTH (Just a) = Just <$> deepCopyTH a
    deepCopyTH Nothing = return Nothing

instance (DeepCopyTH a, DeepCopyTH b) => DeepCopyTH ((,) a b) where
    deepCopyTH (a, b) = (,) <$> deepCopyTH a <*> deepCopyTH b


main = $(pprintQ $ deepCopyInstance (mkName "DeepCopyT") (mkName "deepCopyT") [TyVar, TyVar] (VarE $ mkName "hai"))
