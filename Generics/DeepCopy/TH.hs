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
import Data.List
import Data.IORef
import Control.Concurrent.MVar
import Control.Monad
import Control.Arrow
import Control.Applicative

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State.Lazy (StateT, runStateT)
import Control.Monad.Trans.Writer.Lazy (WriterT, runWriterT)

import Control.Monad.Reader.Class
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

instance (Quasi m) => Quasi (ReaderT e m) where
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

data ClassInfo = ClassInfo {
      -- | The type level class name
      clName   :: TyName

      -- | The type class' deepCopy method name
    , clMethod :: Name
    }

type R m = ReaderT ClassInfo m
type W m = WriterT [TyName]  m

type WR a = W (R Q) a

runR :: Monad m => ClassInfo -> R m a -> m a
runR = flip runReaderT

runW :: Monad m => W m a -> m (a,[TyName])
runW = runWriterT

makeDeepCopy :: TyName -> Name -> Q [Dec]
makeDeepCopy tyn mn = do
  cn <- newName "DeepCopyTH"
  liftM concat $ sequence $ [ deepCopyClass cn mn
                            , runR (ClassInfo cn mn) $ deepCopyInstance tyn
                            ]

deepCopyClass :: TyName -> Name -> Q [Dec]
deepCopyClass cn mn = do
  an <- newName "a"
  return $ [ClassD [] cn [PlainTV an] [] [methodDec an]]
 where
   methodDec a =
       SigD mn (AppT (AppT ArrowT (VarT a)) (AppT (ConT ''IO) (VarT a)))

-- | First two arguments are the same as passed to 'deepCopyClass'
deepCopyInstance :: TyName -> R Q [Dec]
deepCopyInstance tyn = do
  tvs <- reifyTyVars tyn
  ns <- const (qNewName "a") `mapM` tvs
  v <- qNewName "v"
  (e, ts) <- deepCopyExp tyn (VarE v)

  ClassInfo cn mn <- ask
  let i = InstanceD ((\n -> ClassP cn [VarT n]) `map` ns)
                    (foldl AppT (ConT cn) ([ConT tyn] ++ VarT `map` ns))
                    [FunD mn [ Clause [VarP v] (NormalB e) [] ]]
  is <- concat <$> deepCopyInstance `mapM` nub (filterRefs ts)

  return (i:ioRefInstance cn mn:mVarInstance cn mn:is)
 where
   filterRefs ts = filter f ts
    where f a = not $ a `elem` [''IORef, ''MVar]


   ioRefInstance cn mn = uncurry3 InstanceD
       ( [ClassP cn [VarT (mkName "a")]]
       , AppT (ConT cn) (AppT (ConT ''IORef) (VarT (mkName "a")))
-- $(showQ [d| deepCopyTH = newIORef <=< deepCopyTH <=< readIORef |])
       , [ValD
          (VarP mn)
          (NormalB
           (InfixE
            (Just (VarE 'newIORef))
            (VarE '(<=<))
            (Just (InfixE
                   (Just (VarE mn))
                   (VarE '(<=<))
                   (Just (VarE 'readIORef)))))) []]
       )
   mVarInstance cn mn = uncurry3 InstanceD
       ( [ClassP cn [VarT (mkName "a")]]
       , AppT (ConT cn) (AppT (ConT ''IORef) (VarT (mkName "a")))
-- $(showQ [d| deepCopyTH = newMVar <=< deepCopyTH <=< readMVar |])
       , [ValD
          (VarP mn)
          (NormalB
           (InfixE
            (Just (VarE 'newMVar))
            (VarE '(<=<))
            (Just (InfixE
                   (Just (VarE mn))
                   (VarE '(<=<))
                   (Just (VarE 'readMVar)))))) []]
       )

   uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
   uncurry3 f (a, b, c) = f a b c


-- deepCopyFn :: TyName -> DC Dec
-- deepCopyFn tyn = do
--   v <- qNewName "a"
--   fun <$> fnName tyn <*> return v <*> deepCopyExp tyn (VarE v)
--  where
--    fun f v e = FunD f [Clause [VarP v] (NormalB e) []]

deepCopyExp :: TyName -> Exp -> R Q (CaseExp, [TyName])
deepCopyExp tyn e = do
  dty <- reifyTy tyn
  (ms, ts) <- runWriterT $ dataTyMatches dty
  return (CaseE e ms, ts)


dataTyMatches :: DataTy -> WR [Match]
dataTyMatches (DataTy cs) = dataTyMatch `mapM` cs

dataTyMatch :: DataCtor -> WR Match
dataTyMatch (DataCtor cn ts) = do
  ns  <- const (qNewName "p")  `mapM` ts
  ns' <- const (qNewName "p'") `mapM` ts

  stms <- dataTyStm `mapM` zip3 ns' ts ns

  return $ (cn $% ns) --> DoE (stms ++ returnStm ns')
 where
   p --> e = Match p (NormalB e) []
   c $% ns = ConP c $ VarP `map` ns

   returnStm ns' = return $ NoBindS $
     AppE (VarE 'return) (foldl AppE (ConE cn) (VarE `map` ns'))


dataTyStm :: (Name, Either Ty TyVar, Name) -> WR Stmt
dataTyStm (n', ety, n) = do
  f <- case ety of
         Left (Ty tyn _) -> tell [tyn] >> (clMethod <$> ask)
         Right _ -> clMethod <$> ask
  return $ n' <-- fncall f n
 where
   infix 3 <--
   n <-- e = BindS (VarP n) e

   fncall f n = (AppE (VarE f) (VarE n))
