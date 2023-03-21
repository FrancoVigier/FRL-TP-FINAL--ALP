{-|
Module      : Eval
Description : Evalúa una Lista, paso a paso
Copyright   : Franco Vallejos Vigier
License     : GPL-3
Maintainer  :  francovallejosvigier@gmail.com
Stability   : experimental
Este módulo evaluá términos siguiendo la semántica big-step
-}

module Eval where
import Lang
import FRLMonad (MonadFRL,lookupOp,lookupVar,addVar,addOp,failFRL,printFRL,catchErrors)


--Verificador que una DefVar x no tenga en su body una var no declarada
verifyVarList:: MonadFRL m => Exp -> m Bool
verifyVarList (ListaBase xs) = return True
verifyVarList (VarEnvList nm) =  do --Falta el componerlo en el lexer
                                                                     var <- lookupVar nm
                                                                     case var of
                                                                             Nothing -> failFRL "Declaracion de una Variable Lista en funcion de una inxesistente"
                                                                             Just exp -> return True
verifyVarList(Aplication _ exp) = verifyVarList exp

--Verificador que una DefFoo f no tenga en su body una foo no declarada
verifyVarFoo:: MonadFRL m => [Ops] -> m Bool --NO Permitimos el Rep []
verifyVarFoo [] = return True
verifyVarFoo ((VarEnvFoo nm) : opss) = do --Falta el componerlo en el lexer
                                                                     var <- lookupOp nm
                                                                     case var of
                                                                             Nothing -> failFRL "Declaracion de una Variable Foo en funcion de una inxesistente"
                                                                             Just ops -> verifyVarFoo opss
verifyVarFoo ((Rep []):opss) =  failFRL "Declaracion de una Variable Foo en con un REP Vacion"
verifyVarFoo ((Rep ops):opss) = do
                                                                  verifyVarFoo ops
                                                                  verifyVarFoo opss
verifyVarFoo (_:opss) = verifyVarFoo opss


evalCommandFile ::MonadFRL m => Comm->  m ()
evalCommandFile (Def decl) = do 
                                                                   evalCommand (Def decl) --Si tira error, si bien aca no lo catcheamos el main con el file SI lo catchea
                                                                   return ()
evalCommandFile (ErrorCommando err) = failFRL err




evalCommand :: MonadFRL m => Comm->  m (Maybe [Integer])
evalCommand (Def decl) = case decl of
                                                                DeclVarLista {declName = nm, declBody= exp} -> do --LISTO: AHORA SI PERMITE...ACA Por eso no se permite el DefVar x = [...], DefVar x = Op $ x; porque agrega la expresion haciendo un bucle ya que siempre hay una ref a x en el cuerpo y se queda buscando en el env ad eternum. Se arregla guardando en el env el EVAL EXP y no el exp en el declBody
                                                                                                                                                                         verifyVarList exp --no me importa el resultado solo me importa encontrar un error y que el main lo catchee
                                                                                                                                                                         reduccion <-catchErrors$evalExp exp -- Evaluo la expresion a igualar y me arrastro sus errores
                                                                                                                                                                         case reduccion of --Evaluo el body de la lista, porque si me guardo en el env la referencia y no el valor trae problemas si quiero redefinir una var en funcion de si misma
                                                                                                                                                                               Nothing-> failFRL "Declaracion de una Variable Foo cuyo body o asignacion no se puede Evaluar"
                                                                                                                                                                               Just xs -> do
                                                                                                                                                                                                      addVar nm (ListaBase xs)--Guardo el valor. call by value
                                                                                                                                                                                                      return Nothing
                                                                DeclVarFoo {declName = nm,declbody = opss} -> do --Las funciones no acarrean el error de la redefinicion en funcion de si misma... porque no necesitamos evaluarlas para reducirlas pero hay que tener cuidado que puede quedar una funcion bucle EJ DefFoo f = Oi ----DefFoo f = Oi f ---- f $ []---- Explota Bucle
                                                                                                                                                                         case opss of
                                                                                                                                                                                  [] -> failFRL "Declaracion de una Variable Foo con cuerpo VACIO" --No entra aca por el ParseError
                                                                                                                                                                                  _ -> do 
                                                                                                                                                                                                verifyVarFoo opss
                                                                                                                                                                                                addOp nm opss
                                                                                                                                                                                                return Nothing
evalCommand (Eval exp) = do
                                                            var <- evalExp exp
                                                            return (Just var)
evalCommand _ =  failFRL "Comando no Reconocido"

evalCommandPrintVar :: MonadFRL m => Comm->  m (Maybe Exp) --Hago en otro eval los print por el entorno ya que no son [Integer]
evalCommandPrintVar (PrintVar nm) = do --Falta el componerlo en el lexer
                                                             var <- lookupVar nm
                                                             case var of
                                                                      Nothing -> failFRL "Variable LIST Inexistente Print"
                                                                      Just exp -> return (Just exp)

evalCommandPrintFoo ::MonadFRL m => Comm->  m (Maybe [Ops]) --Hago en otro eval los print por el entorno ya que no son [Integer]
evalCommandPrintFoo (PrintFoo nm) = do --Falta el componerlo en el lexer
                                                             var <- lookupOp nm
                                                             case var of
                                                                      Nothing -> failFRL "Variable FOO Inexistente Print"
                                                                      Just ops -> return (Just ops)







evalExp :: MonadFRL m => Exp -> m [Integer]
evalExp (ListaBase xs) = return xs
evalExp (VarEnvList nm) = do
                                                           var <-lookupVar nm
                                                           case var of 
                                                                   Nothing -> failFRL "Variable Inexistente Eval"
                                                                   Just exp -> evalExp exp
evalExp (Aplication opss exp) = do
                                                                      xs <- evalExp exp
                                                                      evalExpConOpLista xs opss
                                                                      

evalExpConOpLista ::MonadFRL m =>[Integer] -> [Ops] -> m [Integer]
--evalExpConOpLista [] _ = return []
evalExpConOpLista xs [] = return xs
evalExpConOpLista xs (Ol:opss) = do 
                                                                         valxs <-evalExpConOpLista xs opss
                                                                         return (0:valxs)
evalExpConOpLista xs (Or:opss) = do 
                                                                         valxs <-evalExpConOpLista xs opss
                                                                         return (valxs ++ [0])
evalExpConOpLista xs (Sl:opss) = do 
                                                                         valxs <-evalExpConOpLista xs opss
                                                                         case valxs of
                                                                                 [] ->failFRL $ "Sucesor a Izq de una lista Vacia"
                                                                                 _  -> return ((head valxs +1) : (tail valxs))
evalExpConOpLista xs (Sr:opss) = do 
                                                                         valxs <-evalExpConOpLista xs opss
                                                                         case valxs of
                                                                                 [] ->  failFRL $ "Sucesor a Der de una lista Vacia"
                                                                                 _  ->return (init valxs ++ [last valxs + 1])
evalExpConOpLista xs (Dl:opss) = do 
                                                                         valxs <-evalExpConOpLista xs opss
                                                                         case valxs of
                                                                                 [] -> failFRL $ "Delete a Izq de una lista Vacia"                                                                  
                                                                                 _  -> return (tail valxs)
evalExpConOpLista xs (Dr:opss) = do 
                                                                         valxs <-evalExpConOpLista xs opss
                                                                         case valxs of
                                                                                 [] -> failFRL $ "Sucesor a Der de una lista Vacia"
                                                                                 _  -> return (init valxs)
evalExpConOpLista xs ((Rep rss):opss) = do 
                                                                                      valxs <-evalExpConOpLista xs opss --Recursivo evaluo
                                                                                      if (length rss == 0) 
                                                                                              then (failFRL $ "Rep de una sucesion de operaciones vacia")
                                                                                              else case length valxs of
                                                                                                                0 -> failFRL $ "Rep de una lista de enteros vacia"
                                                                                                                1  -> return valxs --Al ser una lista unitaria, el ultimo es igual al primero               
                                                                                                                _  -> if head valxs == last valxs
                                                                                                                             then return valxs
                                                                                                                             else do
                                                                                                                                            valxsRep  <-evalExpConOpLista valxs rss --Evaluo en todas las op del rep
                                                                                                                                            valxsRecursion <-evalExpConOpLista valxsRep [(Rep rss)] --vuelvo a recursionar
                                                                                                                                            return valxsRecursion 
evalExpConOpLista xs (Mr:opss) = do 
                                                                          valxs <-evalExpConOpLista xs opss
                                                                          case valxs of
                                                                                 [] -> failFRL $ "Mover a Der de una lista Vacia"                                                                  
                                                                                 [x]  -> return valxs
                                                                                 (x:xs) -> return (xs++[x])
evalExpConOpLista xs (Ml:opss) = do 
                                                                          valxs <-evalExpConOpLista xs opss
                                                                          case valxs of
                                                                                 [] -> failFRL $ "Mover a Izq de una lista Vacia"                                                                  
                                                                                 [x]  -> return valxs
                                                                                 (x:xs) -> return ([last valxs] ++ init valxs)
evalExpConOpLista xs (DDl:opss) =  do 
                                                                          valxs <-evalExpConOpLista xs opss
                                                                          case valxs of
                                                                                 [] -> failFRL $ "Duplicar a Izq de una lista Vacia"                                                                  
                                                                                 [x]  -> return ([x]++valxs)
                                                                                 (x:xs) -> return (x:x:xs)
evalExpConOpLista xs (DDr:opss) = do 
                                                                          valxs <-evalExpConOpLista xs opss
                                                                          case valxs of
                                                                                 [] -> failFRL $ "Duplicar a Der de una lista Vacia"                                                                  
                                                                                 [x]  -> return (valxs++[x])
                                                                                 (x:xs) -> return (valxs ++[last valxs])
evalExpConOpLista xs (Swap:opss) = do 
                                                                          valxs <-evalExpConOpLista xs opss
                                                                          case valxs of
                                                                                 [] -> failFRL $ "Swap de Extremos de una lista Vacia"                                                                  
                                                                                 [x]  -> return (valxs)
                                                                                 (x:xs) -> return ([last valxs]++reverse (tail (reverse xs))++[x])
evalExpConOpLista xs ((VarEnvFoo nm):opss) = do
                                                                                                     var <-lookupOp nm
                                                                                                     case var of 
                                                                                                             Nothing -> failFRL "Foo Inexistente Eval"
                                                                                                             Just ops -> do
                                                                                                                                        parcialxs  <- evalExpConOpLista xs opss -- Evaluo la lista parcial en el resto de ops
                                                                                                                                        finalxs <- evalExpConOpLista parcialxs ops --Evaluo la lista en el contenido de la varfoo nm
                                                                                                                                        return finalxs