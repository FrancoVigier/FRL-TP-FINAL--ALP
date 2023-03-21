{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

{-|
Module      : MonadFRL
Description : Mónada con soporte para estado, errores, e IO.
Copyright   : (c) Franco Vallejos Vigier
License     : GPL-3
Maintainer  : francovallejosvigier@gmail.com
Stability   : experimental
Definimos la clase de mónadas 'MonadFRL' que abstrae las mónadas con soporte para estado, errores e IO,
y la mónada 'FRL' que provee una instancia de esta clase.
-}
module FRLMonad ( --Falta implementar un Eraser del State.Listo
       FRL,
       runFRL,
       lookupOp,
       lookupVar,
       addVar,
       addOp,
       failFRL,
       MonadFRL,
       catchErrors,
       printFRL,
       eraseEnv,
       module Control.Monad.Except,
       module Control.Monad.State)
 where

import Lang
import Global
import Errors ( Error(..) )
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import System.IO

-- * La clase 'MonadFRL'

{-| La clase de mónadas 'MonadFRL' clasifica a las mónadas con soporte para una configuración Global 'Global.Conf', 
    para operaciones @IO@, estado de tipo 'Global.GlEnv', y errores de tipo 'Errors.Error'.
Las mónadas @m@ de esta clase cuentan con las operaciones:
   - @ask :: m Conf@
   - @get :: m GlEnv@
   - @put :: GlEnv -> m ()@
   - @throwError :: Error -> m a@
   - @catchError :: m a -> (Error -> m a) -> m a@
   - @liftIO :: IO a -> m a@
y otras operaciones derivadas de ellas, como por ejemplo
   - @modify :: (GlEnv -> GlEnv) -> m ()@
   - @gets :: (GlEnv -> a) -> m a@  
-}
--class (MonadIO m, MonadState GlEnv m, MonadError Error m, MonadReader Conf m) => MonadFRL m where
class (MonadIO m, MonadState GlEnv m, MonadError Error m) => MonadFRL m where

lookupOp :: MonadFRL m => Name -> m(Maybe [Ops]) --Dado un nombre devuelve el Just [Ops] o el Nothing
lookupOp nm = do
                                      s' <- get
                                      case filter (hasName nm) (vfooEnv s') of
                                               (nm,ops):_ -> return (Just ops)
                                               [] -> return Nothing
   where hasName :: Name -> (Name,[Ops])  -> Bool
         hasName nm (nm',_) = nm == nm'

lookupVar :: MonadFRL m => Name -> m(Maybe Exp) --Dado un nombre devuelve el Just Exp o el Nothing
lookupVar nm = do
                                      s' <- get
                                      case filter (hasName nm) (vlistaEnv s') of
                                              (nm,exp):_ -> return (Just exp)
                                              [] -> return Nothing
   where hasName :: Name -> (Name,Exp)  -> Bool
         hasName nm (nm',_) = nm == nm'

eraseEnv :: MonadFRL m => m () --Borro el Env, un hardReset
eraseEnv = modify (\glE -> initialEnv)


addVar ::MonadFRL m => Name -> Exp -> m ()
addVar nm exp = do
                                        venv <-lookupVar nm --Busco a ver si essta en el env correspondiente
                                        let var = DeclVarLista {declName = nm,declBody=exp} --armo el decl
                                        case venv of
                                               Nothing ->modify (\glE -> glE {glb = var : glb glE}) --en caso de no estar lo agrego como si nada
                                               Just _  -> modify (update var) --En el caso de que esté lo voy a pisar

update ::  Decl  ->GlEnv ->GlEnv --Pisa en un Env un Decl especifico
update d (GlEnv [])  = GlEnv []
update d@(DeclVarLista nm exp) (GlEnv (x:xs))  = let glbb = update d (GlEnv xs) 
                                                                                                    in case glbb of 
                                                                                                               GlEnv ys -> case x of
                                                                                                                                             (DeclVarLista nm' exp')->if nm == nm' then (GlEnv (d:ys)) else (GlEnv (x:ys))
                                                                                                                                             _ -> GlEnv (x:ys) --En el caso de update una foo y queremos hacer el update de una lista la pasamos de largo
                                                                                                               --_ -> undefined
update d@(DeclVarFoo nm ops)  (GlEnv (x:xs)) = let glbb = update d (GlEnv xs) 
                                                                                                  in  case glbb of 
                                                                                                              GlEnv ys -> case x of
                                                                                                                                             (DeclVarFoo nm' ops')->if nm == nm' then (GlEnv (d:ys)) else (GlEnv (x:ys))
                                                                                                                                             _ -> GlEnv (x:ys) --En el caso de update una lista y queremos hacer el update de una foo la pasamos de largo
                                                                                                              --_ -> undefined
addOp :: MonadFRL m => Name -> [Ops] -> m()
addOp nm opss =do
                                        fenv <-lookupOp nm --Busco a ver si essta en el env correspondiente
                                        let var = DeclVarFoo {declName = nm,declbody=opss}--armo el decl
                                        case fenv of
                                               Nothing ->modify (\glE -> glE {glb = var : glb glE}) --en caso de no estar lo agrego como si nada
                                               Just _  -> modify (update var) --En el caso de que esté lo voy a pisar let

failFRL :: MonadFRL m => String -> m a
failFRL s = throwError (ErrorDescription s)

catchErrors  :: MonadFRL m => m a -> m (Maybe a) --Catchea el Error y lo muestra 
catchErrors c = catchError (Just <$> c)
                           (\e -> liftIO $ hPrint stderr e
                              >> return Nothing)

printFRL :: MonadFRL m => String -> m ()
printFRL = liftIO . putStrLn
-- | El tipo @FRL@ es un sinónimo de tipo para una mónada construida usando dos transformadores de mónada sobre la mónada @IO@.
-- El transformador de mónad @ExcepT Error@ agrega a la mónada IO la posibilidad de manejar errores de tipo 'Errors.Error'.
-- El transformador de mónadas @StateT GlEnv@ agrega la mónada @ExcepT Error IO@ la posibilidad de manejar un estado de tipo 'Global.GlEnv'.
--type FRL = ReaderT Conf (StateT GlEnv (ExceptT Error IO))
type FRL = StateT GlEnv (ExceptT Error IO)
-- | Esta es una instancia vacía, ya que 'MonadFRL' no tiene funciones miembro.
instance MonadFRL FRL

--SI QUIERO EL MODE CON EL CONF SE LO AGREGO PERO NO. NO VOY A USAR MONARREADER
-- 'runFRL\'' corre una computación de la mónad 'FRL' en el estado inicial 'Global.initialEnv' 
runFRL' :: FRL a  -> IO (Either Error (a, GlEnv))
runFRL' c  =  runExceptT $ runStateT c  initialEnv

runFRL:: FRL a  -> IO (Either Error a)
runFRL c = fmap fst <$> runFRL' c
