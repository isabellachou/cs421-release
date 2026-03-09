--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- Metadata for autograder
--- -----------------------
tag1 = 36392
tag2 = 13977
tag3 = 68529

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = factk(n - 1) (\r -> k (n * r))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] evenk oddk
    | even x = evenk x
    | otherwise = oddk x
evenoddk (x:xs) evenk oddk
    | even x = evenoddk xs (\r -> evenk (x + r)) oddk
    | otherwise = evenoddk xs evenk (\r -> oddk (x + r))

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (IntExp _) = True
isSimple (VarExp _) = True
isSimple (LamExp _ _) = True
isSimple (IfExp e1 e2 e3) = isSimple e1 && isSimple e2 && isSimple e3
isSimple (OpExp _ e1 e2) = isSimple e1 && isSimple e2
isSimple (AppExp _ _) = False

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)
--- #### Define `cpsExp` for Integer and Variable Expressions
    -- Base cases: simple values just get applied to the continuation
cpsExp e@(IntExp _) k n = (AppExp k e, n)
cpsExp e@(VarExp _) k n = (AppExp k e, n)

    -- Lambda: add a fresh continuation parameter, CPS-transform the body
cpsExp (LamExp x body) k n =
    let (kv, n') = gensym n
        (body', n'') = cpsExp body (VarExp kv) n'
    in (AppExp k (LamExp x (LamExp kv body')), n'')

--- #### Define `cpsExp` for Application Expressions
    -- Application: evaluate function then argument, pass continuation explicitly
cpsExp (AppExp f e) k n
    | isSimple f && isSimple e =
        (AppExp (AppExp f e) k, n)
    | isSimple f =
        let (v, n') = gensym n
        in cpsExp e (LamExp v (AppExp (AppExp f (VarExp v)) k)) n'
    | otherwise =
        let (v, n') = gensym n
            (inner, n'') = cpsExp (AppExp (VarExp v) e) k n'
        in cpsExp f (LamExp v inner) n''

--- #### Define `cpsExp` for Operator Expressions
-- Operator: evaluate left-to-right, binding non-simple sub-expressions
cpsExp(OpExp op e1 e2) k n
    | isSimple e1 && isSimple e2 =
        (AppExp k (OpExp op e1 e2), n)
    | isSimple e1 = 
        let (v, n') = gensym n
        in cpsExp e2 (LamExp v (AppExp k (OpExp op e1 (VarExp v)))) n'
    | otherwise = 
        let (v, n') = gensym n
            (inner, n'') = cpsExp (OpExp op (VarExp v) e2) k n'
        in cpsExp e1 (LamExp v inner) n''

--- #### Define `cpsExp` for If Expressions
    -- If: if the condition is simple, CPS both branches in place
    -- otherwise CPS the condition first, binding its result to a fresh var
cpsExp (IfExp e1 e2 e3) k n
    | isSimple e1 =
        let (e2', n') = cpsExp e2 k n
            (e3', n'') = cpsExp e3 k n'
        in (IfExp e1 e2' e3', n'')
    | otherwise = 
        let (v, n') = gensym n
            (e2', n'') = cpsExp e2 k n'
            (e3', n''') = cpsExp e3 k n''
        in cpsExp e1 (LamExp v (IfExp (VarExp v) e2' e3')) n'''

--- ### Define `cpsDecl`
cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f params body) =
    let kv = "k"
        (body', _) = cpsExp body (VarExp kv) 0
    in Decl f (params ++ [kv]) body'