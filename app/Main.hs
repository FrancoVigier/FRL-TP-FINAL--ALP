module Main where

import           Control.Monad.Except
import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( print )
import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)

import System.IO
import System.Exit ( exitWith, ExitCode(ExitFailure) )


import Lang

import           Parse
import           Eval (evalCommand, evalCommandPrintVar, evalCommandPrintFoo, evalCommandFile )-- Hago la diferencia porque retornan distintas 
import           FRLMonad
import           PPrinterFRL 
import           Errors

---------------------
--- Interpreter
---------------------

prompt :: String
prompt = "FRL> "

getinputs :: (MonadFRL m,MonadMask m) => InputT m ()
getinputs = do 
                            liftIO $ putStrLn  "Entorno interactivo para FRL.\n"
                            loop
      where loop = do
                                       minput <- getInputLine prompt
                                       case minput of
                                                 Nothing -> return()
                                                 Just "" -> loop
                                                 Just line -> do
                                                                             let comm =  parse line
                                                                             case comm of
                                                                                      ErrorCommando err  ->do  --Comando Error
                                                                                                                                          let pperror =  ppError err --ppError no es monadica la llamas directo
                                                                                                                                          (lift $ (printFRL pperror))>> loop  --Si es un error de comando DEAD y vamos a loop
                                                                                                                                          --(lift $failFRL (err))>> loop -- Si pongo asi me mata TODO
                                                                                      Eval _ ->do --Comando Eval
                                                                                                          res<-(lift $ catchErrorsWithPPError $(evalCommand comm))
                                                                                                          case res of -- catchErrors abstrae mas y pone de nuevo un Maybe al resultado de la evaluacion del comm
                                                                                                               Nothing -> do
                                                                                                                                          let pperror =  ppError "Error en el Comm Evaluacion Main" --ppError no es monadica la llamas directo
                                                                                                                                          (lift $ (printFRL pperror))>> loop
                                                                                                               Just (Just xs) -> do
                                                                                                                                        (lift $ printFRL "El PPrinter de la Eval es : ") --EJ PPRINTER
                                                                                                                                        ppCommand <- lift $ (ppComm comm) --EJPPRINTER
                                                                                                                                        (lift$ (printFRL (ppCommand ++" = ["++ ppLista xs ++"]" ) )) >> loop --EJPRIINTER
                                                                                                               Just Nothing ->loop --Para sacar warning
                                                                                              
                                                                                      Def _ -> do --Comando Definicion
                                                                                                     lift $ (printFRL  "La entrada parseada en AST es : ") --Muestra el Data AST
                                                                                                     lift $ (printFRL  (show (comm)))

                                                                                                     --res<-(lift $ catchErrors $(evalCommand comm)) --Catcheo posibles errores del EVAL que es MONADICO PERRA
                                                                                                     res<-(lift $ catchErrorsWithPPError $(evalCommand comm))
                                                                                                     case res of --Resultado de la Eval. Si es Nothing entonces se catcheo un error de mas abajo
                                                                                                             Nothing -> do       
                                                                                                                                          let pperror =  ppError "Error en el Comm Definifion Main" --ppError no es monadica la llamas directo
                                                                                                                                          (lift $ (printFRL pperror))>> loop
                                                                                                             Just _ -> do
                                                                                                                                     (lift $printFRL "La entrada con el PPrinter es : ") --EJ PPRINTER
                                                                                                                                     ppCommand <- lift $ (ppComm comm) --EJPPRINTER
                                                                                                                                     (lift$ (printFRL ppCommand)) >> loop
                                                                                      PrintVar _ -> do --Comando Mostrar Lista..Falta definir en main VER EVAL PRINT VAR MALLLLLLLLLLLLLL
                                                                                                             lift $ (printFRL  "La entrada parseada en AST esss : ") --Muestra el Data AST
                                                                                                             (lift $ (printFRL  (show (comm))))   --Falta definir en main

                                                                                                             res<-(lift $ catchErrorsWithPPError $(evalCommandPrintVar comm)) --Evaluo el PrintVar
                                                                                                             case res of 
                                                                                                                    Nothing -> do      
                                                                                                                                               let pperror =  ppError "Error en el Comm BuscarEnvList Main" --ppError no es monadica la llamas directo
                                                                                                                                               (lift $ (printFRL pperror))>> loop
                                                                                                                    Just (Just e)-> do  --Exp
                                                                                                                                                    (lift $printFRL "El PPrinter de la Busqueda es : ")
                                                                                                                                                    ppCommand <- lift $ (ppComm comm) 
                                                                                                                                                    (lift$ (printFRL (ppCommand ++ ppExp e))) >> loop --ppExp porque retorna el body
                                                                                                                    Just Nothing ->loop --Para sacar warning

                                                                                      PrintFoo _  -> do --Comando Mostrar Foo..
                                                                                                                     lift $ (printFRL  "La entrada parseada en AST es : ") --Muestra el Data AST
                                                                                                                     (lift $ (printFRL  (show (comm))))   --Falta definir en main
                                                                                                                     res<-(lift $ catchErrorsWithPPError $(evalCommandPrintFoo comm)) --Evaluo el PrintVar
                                                                                                                     case res of 
                                                                                                                             Nothing -> do      
                                                                                                                                                        let pperror =  ppError "Error en el Comm BuscarEnvFoo Main" --ppError no es monadica la llamas directo
                                                                                                                                                        (lift $ (printFRL pperror))>> loop
                                                                                                                             Just (Just ops)-> do  -- Lista de OPs
                                                                                                                                                                    (lift $printFRL "El PPrinter de la Busqueda es : ")
                                                                                                                                                                    ppCommand <- lift $ (ppComm comm) 
                                                                                                                                                                    (lift$ (printFRL (ppCommand ++"[ " ++ ppOps ops ++"]"))) >> loop --ppExp porque retorna el body
                                                                                                                             Just Nothing ->loop --Para sacar warning
                                                                                      Exit ->do --Comando exit
                                                                                                       lift $ (printFRL  "La entrada parseada en AST es : ") --Muestra el Data AST
                                                                                                       (lift $ (printFRL  (show (comm))))  

                                                                                                       ppCommand <- lift $ (ppComm comm)
                                                                                                       lift$ (printFRL (ppCommand))

                                                                                                       (lift $failFRL ("")) 
                                                                                      LoadFile path -> do -- SOLO TOMA ARCHIVOS BIEN ESCRITOS Y CON SOLO DEFINICIONES, No es capaz de encontrar errores en las lineas del archivo. No robusto
                                                                                                                             --lift $ (printFRL  "La entrada parseada en AST es : ") --Muestra el Data AST
                                                                                                                             --(lift $ (printFRL  (show (comm))))  
                                                                                                                             lift $ printFRL ("Abriendo el archivo " ++ path ++ "...") --Muestro el path
                                                                                                                             if ".frl" `isSuffixOf` path then --Checkeo la extension del file
                                                                                                                                    do 
                                                                                                                                         lf <-  liftIO $ catchError (do --Leo el archivo, y si me mandan un bad path catcheo el error con catherror de la frl que ya tiene por se instance IO()
                                                                                                                                                                                     lf'<- lines <$> readFile path
                                                                                                                                                                                     return (Just lf'))
                                                                                                                                                                             (\err  -> do 
                                                                                                                                                                                                   let pperror = ppError ("Error al abrir el File "++show (err))-- Abuso y reuso el erro que te tira el openFile... y lo paso por ppError (+Bonito)
                                                                                                                                                                                                   putStrLn $ pperror--Printeo el error acarreado
                                                                                                                                                                                                   --(liftIO $(printFRL pperror)) --Printeo el error acarreado, no uso este porque necesito una IO () pura y printFRL es liftIO.putStrLn
                                                                                                                                                                                                   return Nothing) 
                                                                                                                                         case lf of --matcheo el resultado de abrir el file 
                                                                                                                                            Nothing -> loop --Si hubo un error al abrir ya lo catchea el catch$ y se encarga de mostrarlo, vamos pa loop
                                                                                                                                            Just [] -> do  
                                                                                                                                                                  let pperror = ppError ("Error: Archivo Vacio" )
                                                                                                                                                                  (lift $ (printFRL pperror))>> loop --Printeo el error acarreado
                                                                                                                                            Just lfcontenido -> do --TODO OK y Manejo el contenido del file, que tiene a cada linea en una [[String]]
                                                                                                                                                                                   let ppPrologo = ppError ("Cargando el contenido del archivo: "++ path)
                                                                                                                                                                                   (lift $ (printFRL(ppPrologo))) 
                                                                                                                                                                                        --(lift $ (printFRL(show lfcontenido)))  --FOR DEBUG: Lo imprimo pa ve, si no hay error saco el contenido DE ACA PA ABAJO QUE vrga hago?
                                                                                                                                                                                        
                                                                                                                                                                                        --let ppPrologo = ppError ("El Contenido del Archivo Parseado "++ path++" es:") --FOR DEBUG
                                                                                                                                                                                        --(lift $ (printFRL(ppPrologo))) --FOR DEBUG 
                                                                                                                                                                                   let listaDeComandos = map parse lfcontenido
                                                                                                                                                                                        --(lift $ (printFRL(show listaDeComandos))) --FOR DEBUGMuestro todas las lineas del archivo pasadar una a una por el parser
                                                                                                                                                                                        
                                                                                                                                                                                   contenidoCheckParse<-  (lift$ catchErrorsWithPPError$ controlParsingFile listaDeComandos 1 path) --Control de errores en el parseo del contenido del archivo
                                                                                                                                                                                   case contenidoCheckParse of --Gracias a FailFRL y CatchErrors si no es exitoso el controlEvaluationFile en todas las lineas NO VA A ACTUALIZAR EL ESTADO con todas las declaraciones, fuerza a que tiene que ser exitosa
                                                                                                                                                                                            Nothing -> loop --Significa que hubo errores en el parseo del archivo en alguna linea y el catch de arriba ya lo muestra
                                                                                                                                                                                            Just contenidoOKParse ->(lift$ catchErrorsWithPPError$ controlEvaluationFile contenidoOKParse 1 path)  >> loop --Solo hay Def Decl en la lista
                                                                                                                                                                                                                                                 --(lift$ catchErrorsWithPPError$(mapM (evalCommandFile) contenidoOKParse ))  >> loop--Hago el mapeo monadico de evalCommand (monadica) en la lista. Hacer evalCommandoFile y tirar catch
                                                                                                                             
                                                                                                                             else  --Error de extension no .frl
                                                                                                                                    do
                                                                                                                                         let pperror =  ppError "Error en la extension del file tiene que ser .frl"
                                                                                                                                         (lift $ (printFRL pperror))>> loop
                                                                                      Flush -> do
                                                                                                            let ppPrologo = ppError ("Flush al Env...")
                                                                                                            (lift $ (printFRL(ppPrologo))) 
                                                                                                            (lift $ eraseEnv)>> loop
                                                                                      Peek  -> do
                                                                                                            peekenv <- lift $get --Obtendo el env
                                                                                                            (lift $ (printFRL ("[ "++(ppEnv peekenv)++"]"))) >> loop
                                                            
                                          
main ::IO ()
main =  runOrFail (runInputT defaultSettings getinputs)

runOrFail :: FRL m -> IO m
runOrFail m = do
                                  r <- runFRL m
                                  case r of
                                       Left err -> do
                                                                liftIO $ hPrint stderr err
                                                                exitWith (ExitFailure 1)
                                       Right v -> return v


catchErrorsWithPPError :: MonadFRL m => m a -> m (Maybe a) --La diff con catchErrors es que esta integra el ppError del error que catchea o acarrea para no tener inclusion circular defino esta
catchErrorsWithPPError c = catchError (Just <$> c)
                           (\e -> case e of
                                                ErrorDescription err -> do
                                                                                                  let pperror =  ppError ("Error Eval: "++err) --ppError no es monadica la llamas directo
                                                                                                  printFRL pperror
                                                                                                  return Nothing)

controlParsingFile :: MonadFRL m => [Comm]->Int ->String-> m [Comm] --Controlo que la lista parseada del contenido del archivo sean solo declaracions bien formadas sin errores en parsep
controlParsingFile [] n p =  return []
controlParsingFile ((Def d):xs) n p= do
                                                                           comxs <-controlParsingFile  xs (n+1) p
                                                                           return ((Def d):comxs)
controlParsingFile ((ErrorCommando err):xs) n p = failFRL ("Error parser File "++p++" in line "++show n++" "++err)
controlParsingFile ((_):xs) n p = failFRL ("Error parser File in line "++show n++" :Comando NO DECLARATIVO")

controlEvaluationFile :: MonadFRL m => [Comm]->Int->String ->m ()
controlEvaluationFile [] n p= return ()
controlEvaluationFile ((Def d):xs) n p= do
                                                                                 comxs <- catchErrorsWithPPError $ (evalCommandFile (Def d)) 
                                                                                 case comxs of --Vemos si hay error en la evaluacion de una linea de file
                                                                                       Nothing ->  failFRL ("Error evaluation File "++p++ " in line "++show n) --Aviso la linea de error
                                                                                       Just _ -> controlEvaluationFile xs (n+1) p --la declaracion fue evaluada joya
controlEvaluationFile _ _ _= undefined --Nunca en la evaluacion va a haber algo distinto de la declaracion de una variable, porque la limpie con el controlParsingFile  