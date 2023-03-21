{ 
module Parse where

import Lang
import Data.Char
import Data.Maybe
import Data.List
}

%name func 
%monad { E } { thenE } { returnE }
%tokentype { Token } 

--%lexer {lexer} {TEOF}

%token 

    OL          { TOL }
    OR          { TOR }
    SL          { TSL }
    SR          { TSR }
    DL         { TDL }
    DR          { TDR }
    ML          { TML }
    MR          { TMR }
    DDL         { TDDL }
    DDR         { TDDR }
    SWAP         { TSWAP }
    '<'         { TRepL }
    '>'         { TRepR }
    '['         { TBracketL }
    ']'         { TBracketR }
    ','         { TComa }
    ';'           { TSemiColon }
    '.'          {TDot}
    '/'          {TSlash}
    '$'        {TApply}
    NAT         { TNat $$ }--Num. 
    VAR         { TVar $$ } --Var 
    '='         { TEqual }

    DEFFOO { TFuncion }--Def 
    DEFLIST { TVariable }--Let 

    PRNTLIST { TPrintList } 
    PRNTFOO { TPrintFoo}

    LOADFILE {TLoadFile}

    ERASEENV {TErase}

    PEEKENV{TPeek} 

    EXIT      { TExit }
%%
--Gramatica --
Comm :: {Comm} 
Comm : Defexp {Def $1}
               | ERASEENV {Flush}
               | PEEKENV {Peek}
               |PRNTLIST VAR {PrintVar $2} --Implementar el print
               |PRNTFOO VAR {PrintFoo $2}
               |EXIT {Exit}
               | Exp ';' {Eval $1}
               | LOADFILE '.''/'Path {LoadFile (['.']++['/']++$4)}
               --| LOADFILE VAR'.'VAR {LoadFile ($2++['.']++$4)} --/name.extension, readFile exige el "./" primero
             
Path :: {Path} --Intento De parsear un path global desde la carpeta principal del proyecto. LISTO PARSER DE PATH
Path : VAR'/'Path {$1++['/']++$3}
             | VAR'.'VAR {$1++['.']++$3}

Defexp::{Decl}
Defexp : DEFLIST VAR '=' Exp  { DeclVarLista $2 $4} --Si tengo la declaracion de una lista tengo un DeclVarLista
                |DEFFOO VAR '=' OpsLista {DeclVarFoo $2 $4} 

Exp :: {Exp}
Exp : '[' Lista ']' {ListaBase $2} --[1,2,3]
           | OpsLista '$' Exp {Aplication $1 $3} -- "Sd Oi $ [1,2,3]"
           | VAR { VarEnvList $1}

OpsLista ::{ [Ops]}
OpsLista : Ops {[$1]}
                    |Ops OpsLista {$1:$2}

Ops :: {Ops}
Ops : OL {Ol} --Zero a Izq 
                       |OR   {Or} --Zero a Der
                       |SL  {Sl} --Sucesor a Izq
                       |SR   {Sr} --Sucesor a Der
                       |DL   {Dl} --Eliminar a Izq
                       |DR   {Dr} -- Eliminar a Der
                       |ML   {Ml} -- Mover a Izq
                       |MR   {Mr} --Mover a Der
                       |DDL   {DDl} --Duplicar a Izq
                       |DDR   {DDr} --Duplicar a Der
                       |SWAP   {Swap} --Intercambiar extremos 
                       |'<' OpsLista '>' {Rep $2}
                       | VAR {VarEnvFoo $1} --Usar como operador una Foo YA CREADA

Lista :: {[Integer]}
Lista : {- empty -} {[]}
           | NAT {[$1]}
           | NAT ',' Lista {$1 : $3}
{
data Token =  TOR
            | TOL
            | TSL
            | TSR
            | TDL
            | TDR
            | TML
            | TMR
            | TDDL
            | TDDR
            | TSWAP
            | TRepL
            | TRepR
            | TBracketR
            | TBracketL
            | TApply
            | TComa
            | TSemiColon
            | TNat Integer
            | TVar String
            | TEqual
            | TDot
            | TSlash
            | TExit
            | TPrintList
            | TPrintFoo
            | TErase
            | TPeek
            | TLoadFile
            | TVariable
            | TFuncion
--TOKEN DE ERROR 
            |TNullVar --Token de error 
            | TParserError --Toker de error de Parseo
            |TErrorNumSuelto 
            |TBadNumberList
            deriving (Show, Eq)
-------------------------------
data E a = Ok a | Failed String --https://www.haskell.org/happy/doc/html/sec-monads.html

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =  case m of 
                                       Ok a -> k a
                                       Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =  case m of
                                   Ok a -> Ok a
                                   Failed e -> k e
------------------------------
happyError :: [Token] -> E a --Se usa solo. Grande Happy. Agregar mas token de error de forma de pattern match en los lexer
happyError tokens | elem TNullVar tokens = failE "Error de parseo, nombre de variable  null o palabra reservada missmatch. Revisar gramatica" --NO PONGO TCOMMERROR PORQUE LO HAGO A NIVEL MAIN
                                       | elem TParserError tokens = failE "Error en de Parseo, caracter no reconocido. Revisar LEXER"
                                       | elem TErrorNumSuelto tokens = failE "Error de Parseo, número por fuera de [ ]."
                                       | elem TBadNumberList tokens = failE "Error de Parseo, lista integrada por caracter NO numerico."
                                       | otherwise = failE "Error de parseo"

lexer :: String -> [Token]
lexer [] = []
lexer ('\n':cs) = lexer cs
lexer ('\r':cs) = lexer cs
lexer (c:cs) | isSpace c = lexer cs
                       | isDigit c = [TErrorNumSuelto] --Creo que no va TErrorNumSuelto
                       | isAlpha c = lexerVar (c:cs)
lexer ('<':cs) = TRepL : lexer cs
lexer ('>':cs) = TRepR : lexer cs
lexer txs@('[':cs) = lexerList txs --EL   ']'         { TBracketR } lo como el lexerList
lexer (',':cs) = TComa : lexer cs
lexer (';':cs) = TSemiColon : lexer cs
lexer ('$':cs) = TApply : lexer cs
lexer ('=':cs) = TEqual : lexer cs
lexer ('.':cs) = TDot : lexer cs
lexer ('/':cs) = TSlash : lexer cs
lexer cs = [TParserError] --Error

lexerVar ::String  -> [Token]
lexerVar [] = []
lexerVar cs@(c:cc) = case span isAlphaNum cs of
                                       ("Oi", rest) -> TOL : lexer rest
                                       ("Od", rest) -> TOR : lexer rest
                                       ("Si", rest) -> TSL : lexer rest
                                       ("Sd", rest) -> TSR : lexer rest
                                       ("Di", rest) -> TDL : lexer rest
                                       ("Dd", rest) -> TDR : lexer rest
                                       ("Mi", rest) -> TML : lexer rest
                                       ("Md", rest) -> TMR : lexer rest
                                       ("DDi", rest) -> TDDL : lexer rest
                                       ("DDd", rest) -> TDDR : lexer rest
                                       ("Swap", rest) -> TSWAP : lexer rest
                                       ("DefFoo", rest)   -> TFuncion  : lexer rest
                                       ("DefVar", rest)   -> TVariable : lexer rest
                                       ("Quit", rest) -> TExit : lexer rest
                                       ("LookFoo",rest) -> TPrintFoo : lexer rest
                                       ("LookVar", rest)  -> TPrintList : lexer rest
                                       ("LoadFile", rest)  -> TLoadFile : lexer rest
                                       ("Flush",rest) -> TErase : lexer rest
                                       ("Peek",rest)-> TPeek : lexer rest
                                       (name, rest)  -> if name ==" "  then [TNullVar] else (TVar name):lexer rest--Cambiar TCARERROR en este cas o por un TOpError tipo OP INVALIDA



lexerList ::String  -> [Token]
lexerList [] = []
lexerList (',':cs) = TComa : lexerList cs
lexerList ('[':cs) = TBracketL : lexerList cs
lexerList (']':cs) = TBracketR : lexer cs
lexerList (c:cs) | isDigit c = lexerListNatAux (c:cs)   
                              | otherwise = [TBadNumberList]
                                      where lexerListNatAux xs = case xs of
                                                                                                       [] -> []
                                                                                                       (c:cs) ->let 
                                                                                                                            (nums, rest) = span (isDigit) (c:cs)
                                                                                                                       in (TNat (read nums :: Integer)) : lexerList rest
      

parse :: String -> Comm
parse contents = case func $ lexer contents of
                       Ok ast   -> ast
                       Failed err -> ErrorCommando err
}
