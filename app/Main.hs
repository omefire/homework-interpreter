{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Lib

import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.Error
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
import Data.Stack
import Data.Map.Strict as Map
import Control.Monad (when)



data ByteCode r where
    LoadValue :: Int -> ByteCode ()
    WriteVariable :: String -> ByteCode ()
    ReadVariable :: String -> ByteCode ()
    Add :: ByteCode ()
    Sub :: ByteCode ()
    Div :: ByteCode ()
    Mul :: ByteCode ()
    {-ReturnValue :: ByteCode ()-}
makeEffect ''ByteCode

type InterpreterState = (Stack Int, Map.Map String Int)

runByteCodeInstruction :: InterpreterState -> Eff '[ByteCode] w -> InterpreterState
runByteCodeInstruction interpreterState req = run $ execState interpreterState (runError (reinterpret2 go req))
    where
        go :: ByteCode v -> Eff '[Error (), State InterpreterState] v
        go (LoadValue val) = do
            (st, map) <- get
            let st' = stackPush st val
            put (st', map :: Map.Map String Int) -- This is to solve the previous error: -> /Users/omefire/Projects/homework-interpreter/app/Main.hs:40:26: error:
{-
    • Couldn't match type ‘(,) (Stack Int)’
                     with ‘Eff '[Error (), State InterpreterState]’
      Expected type: Eff
                       '[Error (), State InterpreterState] (Map String Int)
        Actual type: (Stack Int, Map String Int)
    • In a stmt of a 'do' block:
        (st, map) <- get :: (Stack Int, Map String Int)
      In the expression:
        do (st, map) <- get :: (Stack Int, Map String Int)
           let st' = stackPush st val
           put (st', map)
           return ()
      In an equation for ‘go’:
          go (LoadValue val)
            = do (st, map) <- get :: (Stack Int, Map String Int)
                 let st' = ...
                 put (st', map)
                 ....
   |
40 |             (st, map) <- get :: (Stack Int, Map.Map String Int)
   |                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
-}
            return ()
        go (WriteVariable var) = do
            (st, map) <- get
            let map' = case (stackPop st :: Maybe(Stack Int, Int)) of
                            -- TODO: Test this path!
                            Nothing -> error "Error (WriteVariable instruction): the stack is empty" 
                            Just (_, val) -> case (Map.lookup var map) of
                                                Nothing -> Map.insert var val map
                                                Just _ ->  Map.update (\y -> Just y) var map
            put (st, map')
            return ()
        go (ReadVariable var) = do
            (st, map) <- get
            let v = case (Map.lookup var map) of
                        Nothing -> error "Error (ReadVariable instruction): no such variable"
                        Just val -> val
            let st' = stackPush st v
            put (st' :: Stack Int, map :: Map.Map String Int)
            return ()
        go (Add) = do
            (st, map) <- get
            -- when (stackSize st < 2) error "Error (Add instruction): Less than 2 elements in the stack"
            let (stack1, stack2, val1, val2) = case (stackPop st) of
                                Nothing -> error "Error (Add instruction): Less than 2 elements in the stack"
                                Just (st1, v1) -> case (stackPop st1) of
                                                    Nothing -> error "Error (Add instruction): Less than 2 elements in the stack"
                                                    Just (st2, v2) -> (st1, st2, v1, v2)
            let result = val1 + val2
            let stack3 = stackPush stack2 result
            put (stack3 :: Stack Int, map :: Map.Map String Int)
            return ()

        -- TODO: Refactor this so that we don't have duplicated code between this and the handler for the Add operation
        go (Sub) = do
            (st, map) <- get
            -- when (stackSize st < 2) error "Error (Add instruction): Less than 2 elements in the stack"
            let (stack1, stack2, val1, val2) = case (stackPop st) of
                                Nothing -> error "Error (Add instruction): Less than 2 elements in the stack"
                                Just (st1, v1) -> case (stackPop st1) of
                                                    Nothing -> error "Error (Add instruction): Less than 2 elements in the stack"
                                                    Just (st2, v2) -> (st1, st2, v1, v2)
            let result = val1 - val2
            let stack3 = stackPush stack2 result
            put (stack3 :: Stack Int, map :: Map.Map String Int)
            return ()
        go (Div) = do
            (st, map) <- get
            -- when (stackSize st < 2) error "Error (Add instruction): Less than 2 elements in the stack"
            let (stack1, stack2, val1, val2) = case (stackPop st) of
                                Nothing -> error "Error (Add instruction): Less than 2 elements in the stack"
                                Just (st1, v1) -> case (stackPop st1) of
                                                    Nothing -> error "Error (Add instruction): Less than 2 elements in the stack"
                                                    Just (st2, v2) -> (st1, st2, v1, v2)
            let result = val1 `Prelude.div` val2
            let stack3 = stackPush stack2 result
            put (stack3 :: Stack Int, map :: Map.Map String Int)
            return ()
        go (Mul) = do
            (st, map) <- get
            -- when (stackSize st < 2) error "Error (Add instruction): Less than 2 elements in the stack"
            let (stack1, stack2, val1, val2) = case (stackPop st) of
                                Nothing -> error "Error (Add instruction): Less than 2 elements in the stack"
                                Just (st1, v1) -> case (stackPop st1) of
                                                    Nothing -> error "Error (Add instruction): Less than 2 elements in the stack"
                                                    Just (st2, v2) -> (st1, st2, v1, v2)
            let result = val1 * val2
            let stack3 = stackPush stack2 result
            put (stack3 :: Stack Int, map :: Map.Map String Int)
            return ()    

runByteCode :: [ByteCode] -> State InterpreterState ()
runByteCode [] = 
runByteCode (x:xs) = 

main :: IO ()
main = do

  -- LoadValue test
  let interpreterState = (stackNew :: Stack Int, Map.empty)
  let (stack, map) = runByteCodeInstruction interpreterState (send $ LoadValue 13)
  case (stackPop stack) of
      Nothing -> putStrLn "Empty stack"
      Just (_, result) -> putStrLn $ "LoadValue test: " ++ show result

  -- WriteVariable test
  let (stack', map') = runByteCodeInstruction (stack, map) (send $ WriteVariable "x")
  case (Map.lookup "x" map') of
      Nothing -> putStrLn "WriteVariable test: No variable x"
      Just val -> putStrLn $ "WriteVariable test: " ++ show val

  -- ReadVariable test
  let (stack'', map'') = runByteCodeInstruction (stack', map') (send $ ReadVariable "x")
  case (stackPop stack'') of
      Nothing -> putStrLn "Empty stack"
      Just (_, result) -> putStrLn $ "ReadVariable test: " ++ show result

  -- Add test
  -- Sub test
  -- 
