{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
{-|
Module      : PPrint
Description : Pretty printer para FRL.
Copyright   : Franco Vallejos Vigier.
License     : GPL-3
Maintainer  : francovallejosvigier@gmail.com
Stability   : experimental
-}

module PPrinterFRL (
  ppDecl,
  ppComm,
  ppExp,
  ppError,
  ppName,
  ppOps,
  ppLista,
  ppEnv
    ) where

import Lang
import Global

import Data.Text ( unpack )
import Prettyprinter.Render.Terminal
  ( renderStrict, italicized, color, colorDull, Color (..), AnsiStyle )
import Prettyprinter
    ( (<+>),
      annotate,
      defaultLayoutOptions,
      layoutSmart,
      sep,
      Doc,
      Pretty(pretty) )

import FRLMonad



--Colores 
opColor :: Doc AnsiStyle -> Doc AnsiStyle --Color de las OPS
opColor = annotate (colorDull Green)

defColor :: Doc AnsiStyle -> Doc AnsiStyle --Color de las DefVar DefFoo
defColor = annotate (colorDull Magenta <> italicized)

nameColor :: Doc AnsiStyle -> Doc AnsiStyle --Color de los nombre de Def
nameColor = annotate (color Yellow)

symbolColor:: Doc AnsiStyle -> Doc AnsiStyle --Color de los symbol = ; ...
symbolColor = annotate (colorDull Green)

errorColor::Doc AnsiStyle -> Doc AnsiStyle --Color de los Errores
errorColor = annotate (color Red)

listColor::Doc AnsiStyle -> Doc AnsiStyle --Color de los nombre de una Lista Base
listColor = id

numberColor::Doc AnsiStyle -> Doc AnsiStyle --Color de los Errores
numberColor = errorColor

exitColor::Doc AnsiStyle -> Doc AnsiStyle --Color del Exit msg
exitColor = errorColor 

--Render
render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

--PP, pretty ppFoo str
--Start Herre
ppComm :: MonadFRL m => Comm -> m String
ppComm (Def decl) =return $ ppDecl decl
ppComm (Eval exp) =return $ ppExp exp
ppComm (ErrorCommando err) =return $ ppError err
ppComm (Exit) = return $ render $ sep [exitColor (pretty "Exit: Saliendo del interprete...")]
ppComm (PrintVar name) = return $ render $ sep[defColor (pretty "Lookup Env: Lista")]
                                                                                                    <+> pretty (ppName name)<+>sep [symbolColor (pretty "=")]
ppComm (PrintFoo name) = return $ render $ sep[defColor (pretty "Lookup Env: Foo")]
                                                                                                    <+> pretty (ppName name)<+>sep [symbolColor (pretty "=")]

--PP DE DECLS --LISTO
ppDecl :: Decl -> String --PPrinter de Decl (hacerlo monad?) 
ppDecl (DeclVarLista n exp) = render $ sep [defColor (pretty "DefList")
                                                                                , nameColor (pretty n)
                                                                                , symbolColor (pretty "=")]
                                                                        <+> pretty (ppExp exp)
ppDecl (DeclVarFoo n opss) = render $ sep [defColor (pretty "DefFoo")
                                                                                , nameColor (pretty n)
                                                                                , symbolColor (pretty "=")]
                                                                        <+> pretty "[" <+> pretty (ppOps opss)<>pretty "]"



--PP DE EXPs
ppExp :: Exp ->  String --Listo, pretty ppX
ppExp (ListaBase xs) = render $ listColor (pretty (show xs)) 
ppExp (Aplication opss exp) = render $ (pretty (ppOps opss)) <+> pretty (ppExp exp)
ppExp (VarEnvList nm) = render $ pretty(ppName nm)
                                             {-do --Hago un inline expansion de las definiciones del name en el env... ej: Env [(y, [1,2])] DefVar x = y ------> DefVar x = [1,2]  
                                                  var<- lookupVar nm
                                                  case var of
                                                          Nothing -> render $ errorColor (pretty ("Error: Var "++nm++ " no encontrada en el Env..."))
                                                          Just exp -> ppExp exp
                                              -}
--PP DE Error --LISTO
ppError :: String ->  String
ppError err = render $ errorColor (pretty err)

--PP DE Name
ppName :: String -> String
ppName nm = render $ nameColor (pretty nm) 

--PP DE OPS
ppOps :: [Ops] -> String
ppOps [] = render $ pretty ""
ppOps ((Rep ops):opss) =render $ opColor (pretty "R") <+>  pretty "[" <+> pretty (ppOps ops)<>pretty "]" <+> pretty (ppOps opss)
ppOps ((VarEnvFoo nm): opss) = render $ nameColor (pretty nm) <+> pretty (ppOps opss)
ppOps (op:opss) = render $ opColor (pretty (show op)) <+> pretty (ppOps opss) --forma Generica
--ppOps (Ol : opss)= render $ opColor (pretty "Ol") <+> pretty (ppOps opss)


--PP DE Listas
ppLista :: [Integer]  -> String
ppLista [] = render $ pretty ""
ppLista [x] = render $ numberColor (pretty (show x))
ppLista (x:xs) = render $ numberColor (pretty (show x)) <> pretty "," <+> pretty (ppLista xs) 

--PP DE ENV GLBAL
ppEnv :: GlEnv -> String
ppEnv (GlEnv []) = render $ pretty ""
ppEnv (GlEnv [d]) = render $ pretty (ppDecl d)
ppEnv (GlEnv (d:ds)) = render $ (pretty (ppDecl d) ) <> pretty ","<+> pretty (ppEnv (GlEnv ds)) 