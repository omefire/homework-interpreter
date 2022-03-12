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



data ByteCodeInstruction r where
    LoadValue :: Int -> ByteCodeInstruction ()
    WriteVariable :: String -> ByteCodeInstruction ()
    ReadVariable :: String -> ByteCodeInstruction ()
    Add :: ByteCodeInstruction ()
    Sub :: ByteCodeInstruction ()
    Div :: ByteCodeInstruction ()
    Mul :: ByteCodeInstruction ()
    {-ReturnValue :: ByteCodeInstruction ()-}
makeEffect ''ByteCodeInstruction

type InterpreterState = (Stack Int, Map.Map String Int)

type ByteCode a = [ByteCodeInstruction a]

runByteCodeInstructionInstruction :: InterpreterState -> Eff '[ByteCodeInstruction] w -> InterpreterState
runByteCodeInstructionInstruction interpreterState req = run $ execState interpreterState (runError (reinterpret2 go req))
    where
        go :: ByteCodeInstruction v -> Eff '[Error (), State InterpreterState] v
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
            let map' = case (stackPeek st :: Maybe Int) of
                            Nothing -> error $ "Error (WriteVariable instruction): the stack is empty" <> var
                            Just val -> case (Map.lookup var map) of
                                                Nothing -> Map.insert var val map
                                                Just _ ->  Map.update (\y -> Just y) var map
                            
            put (st, map')
            return ()
        go (ReadVariable var) = do -- let result@(resultingStack, resultingMap) = runByteCode [LoadValue 1, WriteVariable "x", ReadVariable "x", LoadValue 1, Add, ReadVariable "y", Mul]
            (st, map) <- get
            let v = case (Map.lookup var map) of
                        Nothing -> error $ "Error (ReadVariable instruction): no such variable: " <> var
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


runByteCode :: ByteCode a -> InterpreterState
runByteCode [] = (stackNew :: Stack Int, Map.empty)
runByteCode byteCode@(x:xs) = let initialInterpreterState = (stackNew :: Stack Int, Map.empty)
    in Prelude.foldl (\interpreterState byteCodeInstruction -> runByteCodeInstructionInstruction interpreterState (send byteCodeInstruction))
             initialInterpreterState byteCode

main :: IO ()
main = do

  -- Empty stack and map test
  putStrLn "---"
  putStrLn "Empty stack and map test: "
  putStrLn $ show $ (stackNew :: Stack Int)
  putStrLn $ show (Map.empty :: Map.Map String Int)
  putStrLn "---"    

  -- LoadValue test
  let (resultingStackLoadValueTest, resultingMapLoadValueTest) = runByteCode [LoadValue 13]
  putStrLn "LoadValue test: [LoadValue 13]"
  putStrLn $ show resultingStackLoadValueTest
  putStrLn $ show resultingMapLoadValueTest
  putStrLn "---"

  -- WriteVariable test
  let (resultingStackWriteVariableTest, resultingMapWriteVariableTest) = runByteCode [LoadValue 13, WriteVariable "x"]
  putStrLn "WriteVariable test: [LoadValue 13, WriteVariable \"x\"]"
  putStrLn $ show resultingStackWriteVariableTest
  putStrLn $ show resultingMapWriteVariableTest
  putStrLn "---"

  -- ReadVariable test
  let (resultingStackReadVariableTest, resultingMapReadVariableTest) = runByteCode [LoadValue 13, WriteVariable "x", ReadVariable "x"]
  putStrLn "ReadVariable test: [LoadValue 13, WriteVariable \"x\", ReadVariable \"x\"]"
  putStrLn $ show resultingStackReadVariableTest
  putStrLn $ show resultingMapReadVariableTest
  putStrLn "---"

  -- Add test
  let (resultingStackAddTest, resultingMapStackAddTest) = runByteCode [LoadValue 10, LoadValue 3, Mul]
  putStrLn "Add test: [LoadValue 10, LoadValue 3, Mul]"
  putStrLn $ show resultingStackAddTest
  putStrLn $ show resultingMapStackAddTest
  putStrLn "---"

  -- Sub test (TODO)
  -- Mul test (TODO)

  -- 'Running multiple instructions' test
  putStrLn "Multiple instructions test: [LoadValue 1, WriteVariable \"x\", ReadVariable \"x\", LoadValue 1, Add, Mul]"
  let result@(resultingStack, resultingMap) = runByteCode [LoadValue 1, WriteVariable "x", ReadVariable "x", LoadValue 1, Add, Mul]
  putStrLn $ show resultingStack
  putStrLn $ show resultingMap
  return ()
