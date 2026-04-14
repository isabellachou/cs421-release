{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Scheme.Eval where

import Scheme.Core

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Control.Monad.Except

-- ### Evaluation helpers
tag1 = 39236
tag2 = 97713

-- Evaluates a symbol to string
-- Throws an error if value is not a symbol
-- Examples:
--   getSym (Symbol "x")  ==> "x"
--   getSym (Number 1)    ==> Not a symbol: x
getSym :: Val -> EvalState String
getSym (Symbol x) = return x
getSym         v  = throwError $ NotASymbol v

-- `let` and `let*`
getBinding :: Val -> EvalState (String, Val)
getBinding (Pair c (Pair e Nil)) = liftA2 (,) (getSym c) (eval e)
getBinding v = throwError $ NotAListOfTwo v

-- Evaluates a list of two to a tuple
-- Throws an error if value is not a list of two
-- This is useful in special form `cond`, since each clause
-- is expected to be exactly a two-element list
getListOf2 :: Val -> EvalState (Val, Val)
getListOf2 (Pair c (Pair e Nil)) = return (c, e)
getListOf2 v = throwError $ NotAListOfTwo v

-- Evaluates a value representing a list into an actual list
getList :: Val -> EvalState [Val]
getList Nil = return []
getList (Pair v1 v2) =
  do xs <- getList v2
     return (v1 : xs)
getList e = throwError $ InvalidSpecialForm "special" e

--- ### Keywords

-- When evaluating special forms, a list form starting with a keyword
-- is expected to match the special form syntax.
keywords :: [String]
keywords = [ "define"
           , "lambda"
           , "cond"
           , "let"
           , "let*"
           , "define-macro"
           , "quasiquote"
           , "unquote"
           ]

-- ### The monadic evaluator
-- Unlike evaluators in previous MPs, `eval` does not take any environment!
-- This is because the environment is encapsulated in the `EvalState` monad.
-- To access the environment, all you have to do is `get`, `modify` or `put`!
eval :: Val -> EvalState Val

-- Self-evaluating expressions
-- TODO: What's self-evaluating?
-- numbers and booleans evaluate to themselves
eval v@(Number _) = return v
eval v@(Boolean _) = return v

-- Symbol evaluates to the value bound to it
-- TODO
-- throws UndefSymbolError if the symbol is not bound
eval (Symbol sym) = do
  env <- get
  case H.lookup sym env of
    Just v -> return v
    Nothing -> throwError $ UndefSymbolError sym

-- Function closure is also self-evaluating
eval v@(Func _ _ _) = return v

-- We check to see if the pair is a "proper list". If it is,
-- then we try to evaluate it, as one of the following forms:
-- 1. Special form (`define`, `let`, `let*`, `cond`, `quote`, `quasiquote`,
--    `unquote`, `define-macro`, ...)
-- 2. Macro expansion (Macro)
-- 3. Function application (Func)
-- 4. Primitive function application (PrimFunc)
eval expr@(Pair v1 v2) = case flattenList expr of
  Left _ -> throwError $ InvalidExpression expr
  Right lst -> evalList lst where
    --- Evaluator for forms
    invalidSpecialForm :: String -> EvalState e
    invalidSpecialForm frm = throwError $ InvalidSpecialForm frm expr

    evalList :: [Val] -> EvalState Val

    evalList [] = throwError $ InvalidExpression expr

    -- quote
    -- TODO
    -- quote: return the argument unevaluated
    evalList [Symbol "quote", e] = return e

    -- unquote (illegal at surface evaluation)
    -- TODO: since surface-level `unquote` is illegal, all you need to do is
    -- to throw a diagnostic
    evalList [Symbol "unquote", e] = throwError $ UnquoteNotInQuasiquote e

    -- quasiquote
    evalList [Symbol "quasiquote", e] = evalQuasi 1 e where
      evalQuasi :: Int -> Val -> EvalState Val
      evalQuasi 0 (Pair (Symbol "unquote") v) = throwError $ UnquoteNotInQuasiquote v
      evalQuasi 1 (Pair (Symbol "unquote") (Pair v Nil)) = eval v
      evalQuasi n (Pair (Symbol "quasiquote") (Pair v Nil)) =
        do v' <- evalQuasi (n+1) v
           return $ Pair (Symbol "quasiquote") (Pair v' Nil)
      evalQuasi n (Pair (Symbol "unquote") (Pair v Nil)) =
        do v' <- evalQuasi (n-1) v
           return $ Pair (Symbol "unquote") (Pair v' Nil)
      evalQuasi n (Pair x y) = Pair <$> evalQuasi n x <*> evalQuasi n y
      evalQuasi _ v = return v

    -- cond
    -- TODO: Handle `cond` here. Use pattern matching to match the syntax
    -- (cond) with no clauses is an invalid special form
    -- (cond (c1 e1) ...) evaluates c1; if truthy evaluates e1, else continues
    -- (cond (else e)) evaluates e unconditionally
    -- else must only appear as the last clause
    evalList (Symbol "cond" : clauses) = case clauses of
      [] -> invalidSpecialForm "cond"
      _  -> evalCond clauses
      where
        evalCond [] = return Void
        evalCond (clause : rest) = do
          (c, e) <- getListOf2 clause
          case c of
            Symbol "else" ->
              if null rest
                then eval e
                else invalidSpecialForm "cond"
            _ -> do
              cv <- eval c
              case cv of
                Boolean False -> evalCond rest
                _ -> eval e

    -- let
    -- TODO: Handle `let` here. Use pattern matching to match the syntax
    -- let: simultaneous binding — all expressions are evaluated in the
    -- original environment, then added together for evaluating the body.
    -- The original environment is restored afterward.
    evalList [Symbol "let", bindings, body] = do
      bindingList <- getList bindings
      pairs <- mapM getBinding bindingList
      env <- get
      let letEnv = foldr (\(k, v) e -> H.insert k v e) env pairs
      put letEnv
      result <- eval body
      put env
      return result
 
    -- let*: sequential binding — each expression is evaluated in the
    -- environment extended by the previous bindings.
    -- The original environment is restored afterward.
    evalList [Symbol "let*", bindings, body] = do
      env <- get
      bindingList <- getList bindings
      mapM_ (\b -> do { (k, v) <- getBinding b; modify $ H.insert k v }) bindingList
      result <- eval body
      put env
      return result

    -- lambda
    -- TODO: Handle `lambda` here. Use pattern matching to match the syntax
    -- lambda: create an anonymous function closure capturing the current env
    evalList [Symbol "lambda", params, body] = do
      env <- get
      paramList <- getList params
      argNames  <- mapM getSym paramList
      return $ Func argNames body env

    -- define function
    evalList [Symbol "define", Pair (Symbol fname) args, body] =
      do env <- get
         argList <- getList args
         val <- (\argVal -> Func argVal body env) <$> mapM getSym argList
         modify $ H.insert fname val
         return Void

    -- define variable
    -- TODO: Handle `define` for variables here. Use pattern matching
    -- to match the syntax
    -- define variable: evaluate the expression and bind it to the symbol
    evalList [Symbol "define", Symbol var, e] = do
      v <- eval e
      modify $ H.insert var v
      return Void

    -- define-macro
    -- TODO: Handle `define-macro` here. Use pattern matching to match
    -- the syntax
    -- define-macro: like define for functions, but produces a Macro value.
    -- Arguments are NOT evaluated when a macro is applied.
    evalList [Symbol "define-macro", Pair (Symbol fname) params, body] = do
      paramList <- getList params
      argNames  <- mapM getSym paramList
      modify $ H.insert fname (Macro argNames body)
      return Void

    -- invalid use of keyword, throw a diagnostic
    evalList (Symbol sym : _) | elem sym keywords = invalidSpecialForm sym

    -- application
    evalList (fexpr:args) =
      do f <- eval fexpr
         apply f args

eval val = throwError $ InvalidExpression val

-- Function application
apply :: Val -> [Val] -> EvalState Val
  -- Function
    -- TODO: implement function application
    -- Use do-notation!
-- Func (closure) application:
--   1. Evaluate all argument expressions (possibly modifying the environment)
--   2. Save the resulting environment
--   3. Build the call environment: params bound to arg values, merged with
--      the closure's captured environment, merged with the current environment
--   4. Evaluate the body
--   5. Restore the saved environment and return the result
apply (Func params body closureEnv) args = do
  argVals <- mapM eval args
  env <- get
  let callEnv = H.union (H.fromList (zip params argVals))
                        (H.union closureEnv env)
  put callEnv
  result <- eval body
  put env
  return result

  -- Macro
    -- TODO: implement macro evaluation
    -- Use do-notation!
-- Macro application:
--   1. Save the current environment
--   2. Bind arguments (unevaluated!) to macro parameters
--   3. Evaluate the macro body to obtain the expanded form
--   4. Restore the saved environment
--   5. Evaluate the expanded form and return its result
apply (Macro params body) args = do
  env <- get
  let macroEnv = H.union (H.fromList (zip params args)) env
  put macroEnv
  expanded <- eval body
  put env
  eval expanded

  -- Primitive
apply (PrimFunc p) args =
  do argVals <- mapM eval args
     p argVals
  -- Other values are not applicable
apply f args = throwError $ CannotApply f args
