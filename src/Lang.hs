{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDeriving #-}

{-|
Module      : Lang
Description : AST de términos, declaraciones y tipos
Copyright   : Franco Vallejos Vigier
License     : GPL-3
Maintainer  : francovallejosvigier@gmail.com
Stability   : experimental
Definiciones de distintos tipos de datos:
  - AST de términos
  - Declaraciones
  - Tipos
  - Variables
-}
 module Lang where

data Comm = Exit -- LISTO
                           | Def Decl --Definicion de VAR y LISTAS . LISTO: Declaro una Var como Exp o Foo
                           | PrintVar Name --LISTO:Busco una Var como expresion en el Env. Falta implementar en el main pero esta en el evalCommand
                           | PrintFoo Name -- Lostp
                           | Eval Exp --LISTO:Evaluo una expresion sola
                           | ErrorCommando String --LISTO: ERROR QUE DEVUELVE EL PARSE DEL HAPPY
                           | LoadFile Path --Listo
                           | Flush --Listo: borra el env
                           | Peek --Listo: Muestra el actual env con un geet
                           deriving (Show, Eq) 

data Decl =
     DeclVarLista { declName :: Name, declBody :: Exp }--Declaro una Var. Como una EXP
  |  DeclVarFoo { declName :: Name, declbody :: [Ops] } --Declaro una Var como una Foo, es decir como una seguidilla finita de Ops
  deriving (Show, Eq) 

data Exp = ListaBase [Integer] --Pongo directo la lista Lista Pura y llana
                     | Aplication [Ops] Exp --Aplicacion de una cantidad finita de operadore a una lista [Ops] = [Funcs] 
                     | VarEnvList Name --Buscar una Var en el Env
                      deriving (Show, Eq) 

type Name = String 
type Path = String

data Ops = Ol --Zero a Izq
                       |   Or --Zero a Der
                       |  Sl --Sucesor a Izq
                       |   Sr --Sucesor a Der
                       |   Dl --Eliminar a Izq
                       |   Dr -- Eliminar a Der
                       |   Ml -- Mover a Izq
                       |   Mr --Mover a Der
                       |   DDl --Duplicar a Izq
                       |   DDr --Duplicar a Der
                       |   Swap --Intercambiar extremos 
                       |   Rep [Ops] --Repeticion
                       |   VarEnvFoo Name --Buscar una foo en el env
                          deriving(Show,Eq)


