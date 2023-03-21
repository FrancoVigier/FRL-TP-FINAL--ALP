{-|
Module      : Global
Description : Define el estado global del EDSL
Copyright   : Franco Vallejos Vigier
License     : GPL-3
Maintainer  : francovallejosvigier@gmail.com
Stability   : experimental
-}
module Global where
--EL GLOBAL NI SE INMUTA GGWP!!
import Lang
data GlEnv = GlEnv {
                                        glb :: [Decl] --Entorno de declaraciones globales
                                       }

vfooEnv :: GlEnv ->  [(Name,[Ops])]
vfooEnv g =
  let
    f (DeclVarLista _ _) = False
    f (DeclVarFoo _ _) = True
  in (map (\(DeclVarFoo n foo) -> (n, foo))) $ (filter f) $ (glb g)

vlistaEnv :: GlEnv ->  [(Name,Exp)]
vlistaEnv g =
  let
    f (DeclVarLista _ _) = True
    f (DeclVarFoo _ _) = False
  in (map (\(DeclVarLista n xs) -> (n, xs))) $ (filter f) $ (glb g)

-- | Valor del estado inicial
initialEnv :: GlEnv
initialEnv = GlEnv []