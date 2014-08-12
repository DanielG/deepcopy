{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts, StandaloneDeriving #-}
module Generics.DeepCopy.TH (
    module Generics.DeepCopy.TH
  , mkName
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Data.Monoid
import Data.Maybe
import Data.Tuple
import Control.Monad
import Control.Arrow
import Control.Applicative

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy (StateT, runStateT)
import Control.Monad.Trans.Writer.Lazy (WriterT, runWriterT)

import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.IO.Class

import qualified Data.Map as M

import Generics.DeepCopy.TH.Class
import Generics.DeepCopy.TH.Ty

type FnName = Name
type ConName = Name

type DataDec = Dec
type CopyDec = Dec

type CaseExp = Exp

instance Quasi m => Quasi (StateT s m) where
  qNewName  	    = lift . qNewName
  qReport   	    = (lift.) . qReport
  qRecover  	    _ _ = error "qRecover"
  qReify    	    = lift . qReify
  qReifyInstances   = (lift.) . qReifyInstances
  qReifyRoles       = lift . qReifyRoles
  qReifyAnnotations = lift . qReifyAnnotations
  qReifyModule      = lift . qReifyModule
  qLookupName       = (lift.) . qLookupName
  qLocation 	    = lift qLocation
  qRunIO    	    = lift . qRunIO
  qAddDependentFile = lift . qAddDependentFile
  qAddTopDecls      = lift . qAddTopDecls
  qAddModFinalizer  = lift . qAddModFinalizer
  qGetQ             = lift qGetQ
  qPutQ             = lift . qPutQ

instance (Quasi m, Monoid w) => Quasi (WriterT w m) where
  qNewName  	    = lift . qNewName
  qReport   	    = (lift.) . qReport
  qRecover  	    _ _ = error "qRecover"
  qReify    	    = lift . qReify
  qReifyInstances   = (lift.) . qReifyInstances
  qReifyRoles       = lift . qReifyRoles
  qReifyAnnotations = lift . qReifyAnnotations
  qReifyModule      = lift . qReifyModule
  qLookupName       = (lift.) . qLookupName
  qLocation 	    = lift qLocation
  qRunIO    	    = lift . qRunIO
  qAddDependentFile = lift . qAddDependentFile
  qAddTopDecls      = lift . qAddTopDecls
  qAddModFinalizer  = lift . qAddModFinalizer
  qGetQ             = lift qGetQ
  qPutQ             = lift . qPutQ


data DCState = DCState {
      fnMap   :: M.Map TyName FnName
    -- | deepCopy functions that have been declared
    , declMap :: M.Map TyName ()
    }

modifyFnMap f = modify $ \(DCState fn dc) -> DCState (f fn) dc
modifyDeclMap f = modify $ \(DCState fn dc) -> DCState fn (f dc)

type DC = StateT DCState Q
runDC :: DC a -> Q a
runDC a = fst <$> (runStateT a $ DCState M.empty M.empty)

fnName :: TyName -> DC FnName
fnName tyn = do
  n <- (M.lookup tyn . fnMap <$> get)
         <|-> (qNewName $ "deepCopy_" ++ descTyn tyn)
  modifyFnMap $ M.insert tyn n
  return n
 where
   descTyn n =
       (fromMaybe "" $ map (repl '.' '_') <$> nameModule n) ++ "_" ++ nameBase n

   repl c r a
       | a == c = r
       | otherwise = a

   infixl 3 <|->
   (<|->) = flip $ liftM2 fromMaybe

fnName' :: TyName -> WriterT [TyName] DC FnName
fnName' tyn = tell [tyn] >> lift (fnName tyn)

makeDeepCopy :: TyName -> Q [Dec]
makeDeepCopy ty = undefined
  -- runDC $ deepCopyFn ty

withDCInScope :: [TyName] -> Exp -> DC Exp
withDCInScope tns e = do
  ds <- deepCopyFn `mapM` tns
  return $ LetE ds e

deepCopyClass :: TyName -> Name -> Q [Dec]
deepCopyClass cn mn = do
  an <- newName "a"
  return $ [ClassD [] cn [PlainTV an] [] [methodDec an]]
 where
   methodDec a =
       SigD mn (AppT (AppT ArrowT (VarT a)) (AppT (ConT ''IO) (VarT a)))

-- | First two arguments are the same as passed to 'deepCopyClass'
deepCopyInstance :: TyName -> Name -> [TyVar] -> Exp -> Q Dec
deepCopyInstance cn mn tyArgs e = do
  ns <- const (newName "a") `mapM` tyArgs
  return $ InstanceD
               (pred `map` ns)
               (foldl AppT (ConT cn) (VarT `map` ns))
               [FunD mn [ Clause (VarP `map` ns) (NormalB e) [] ]]
 where
   pred n = ClassP cn [VarT n]

deepCopyFn :: TyName -> DC Dec
deepCopyFn tyn = do
  v <- qNewName "a"
  fun <$> fnName tyn <*> return v <*> deepCopyExp tyn (VarE v)
 where
   fun f v e = FunD f [Clause [VarP v] (NormalB e) []]

deepCopyExp :: TyName -> Exp -> DC CaseExp
deepCopyExp tyn e = do
  dty <- reifyTy tyn
  (ms, ts) <- runWriterT $ dataTyMatches dty
  withDCInScope ts $ CaseE e ms


dataTyMatches :: DataTy -> WriterT [TyName] DC [Match]
dataTyMatches (DataTy cs) = dataTyMatch `mapM` cs

dataTyMatch :: DataCtor -> WriterT [TyName] DC Match
dataTyMatch (DataCtor cn ts) = do
  ns  <- const (qNewName "p")  `mapM` ts
  ns' <- const (qNewName "p'") `mapM` ts

  stms <- dataTyStm `mapM` zip3 ns ts ns'

  return $ (cn $% ns) --> DoE (stms ++ returnStm ns)
 where
   p --> e = Match p (NormalB e) []
   c $% ns = ConP c $ VarP `map` ns

   returnStm ns = return $ NoBindS $
     AppE (VarE 'return) (foldl AppE (ConE cn) (VarE `map` ns))


dataTyStm :: (Name, Either Ty TyVar, Name) -> WriterT [TyName] DC Stmt
dataTyStm (n', ety, n) = do

  f <- case ety of
         Left (Ty tyn _) -> fnName' tyn
         Right _ -> return 'deepCopyTH
  return $ n' <-- fncall f n
 where
   infix 3 <--
   n <-- e = BindS (VarP n) e

   fncall f n = (AppE (VarE f) (VarE n))
