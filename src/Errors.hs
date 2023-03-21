{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDeriving #-}

{-|
Module      : Errors
Description : Errores del Parser y RunTime
Copyright   : Franco Vallejos Vigier
License     : GPL-3
Maintainer  : francovallejosvigier@gmail.com
Stability   : experimental
Definiciones de distintos tipos de datos:
  - Errores
-}
-- Errores
 module Errors where

data Error = ErrorDescription String  deriving (Show, Eq)
 
