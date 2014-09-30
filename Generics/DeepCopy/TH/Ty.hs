{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
module Generics.DeepCopy.TH.Ty where

import Control.Applicative
import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

import Debug.Trace

twace a = trace ((show a) ++ "\n\n") a

type TyName = Name

data TyVar = TyVar
           deriving (Eq, Ord, Show)

data DataCtor = DataCtor Name [Either Ty TyVar]
              deriving (Eq, Ord, Show)

data Ty = Ty { tyName :: TyName, tyArgs :: [Ty] }
              deriving (Eq, Ord, Show)

data DataTy = DataTy [DataCtor]
              deriving (Eq, Ord, Show)

data T = T (Maybe Bool) Bool
       | T1 Bool

-- [[Bool]] -> AppT ListT (AppT ListT (ConT GHC.Types.Bool))

-- [] ([] Bool) ->
--    AppT (ConT GHC.Types.[]) (AppT (ConT GHC.Types.[]) (ConT GHC.Types.Bool))

-- (,) Bool Bool ->
--    AppT (AppT (ConT GHC.Tuple.(,)) (ConT GHC.Types.Bool)) (ConT GHC.Types.Bool

typeTy :: Type -> Either Ty TyVar
typeTy (VarT _) = Right TyVar
typeTy t = let (Ty ctor []):args = simplifyApp $ normalizeTy (twace t)
           in Left $ Ty ctor args

normalizeTy :: Type -> Type
normalizeTy (ListT) = ConT ''[]
normalizeTy (TupleT i) = ConT $ tupleTypeName i
normalizeTy (UnboxedTupleT i) = ConT $ unboxedTupleTypeName i
normalizeTy t@(VarT _) = t
normalizeTy t@(ConT _) = t
normalizeTy (AppT t t') = AppT (normalizeTy t) (normalizeTy t')
normalizeTy a = error $ "normalizeTy: unexpected " ++ show a

simplifyApp :: Type -> [Ty]
simplifyApp (AppT t ts) = simplifyApp t ++ simplifyApp ts
simplifyApp (ConT t) = [Ty t []]
simplifyApp (VarT _) = error $ "simplifyApp: VarT"
simplifyApp a = error $ "simplifyApp: unexpected " ++ show a

reifyTy :: Quasi m => TyName -> m DataTy
reifyTy n = do
  TyConI (DataD _ n' _ ctors _) <- qReify n
  return $ DataTy (normalizeCon `map` ctors)

reifyTyVars :: Quasi m => TyName -> m [TyVar]
reifyTyVars n = do
  TyConI (DataD _ _ tvs _ _) <- qReify n
  return $ const TyVar `map` tvs


normalizeCon :: Con -> DataCtor
normalizeCon (NormalC n ts) = DataCtor n $ map (typeTy . snd) ts
normalizeCon (RecC n ts)    = DataCtor n $ map (\(_, s, t) -> typeTy t) ts
normalizeCon (InfixC t1 n t2) = DataCtor n $ map (typeTy . snd) [t1, t2]
normalizeCon (ForallC _ _ con) = normalizeCon con


showQ :: (Show a) => Q a -> Q Exp
showQ x = x >>= (qRunIO . putStrLn . show) >> [|return ()|]
