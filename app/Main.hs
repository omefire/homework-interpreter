{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

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
    {-For :: ByteCodeInstruction r -> ByteCodeInstruction Bool -> ByteCodeInstruction r -> ByteCodeInstruction r -> ByteCodeInstruction ()-}
    {-ReturnValue :: ByteCodeInstruction ()-}
makeEffect ''ByteCodeInstruction

type InterpreterState = (Stack Int, Map.Map String Int)

type ByteCode a = [ByteCodeInstruction a]

runByteCodeInstruction :: InterpreterState -> Eff '[ByteCodeInstruction] w -> InterpreterState
runByteCodeInstruction interpreterState req = run $ execState interpreterState (runError (reinterpret2 go req))
    where
        go :: ByteCodeInstruction v -> Eff '[Error (), State InterpreterState] v
        go (LoadValue val) = do
            (st, map) <- get
            let st' = stackPush st val
            put (st', map :: Map.Map String Int)
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
        go (ReadVariable var) = do
            (st, map) <- get
            let v = case (Map.lookup var map) of
                        Nothing -> error $ "Error (ReadVariable instruction): no such variable: " <> var
                        Just val -> val
            let st' = stackPush st v
            put (st' :: Stack Int, map :: Map.Map String Int)
            return ()
        go (Add) = binaryOp Add (+)
        go (Sub) = binaryOp Sub (-)
        go (Div) = binaryOp Div (Prelude.div)
        go (Mul) = binaryOp Mul (*)

        {- stmt1 is executed once before the execution of the code block
           stmt2 defines the condition for executing the code block
           stmt3 is executed (every time) after the code block has been executed
           stmt4 is the code block to be executed every time the condition is true -}
        {-go (For stmt1 stmt2 stmt3 stmt4) = do
            is@(st, map) <- get
            let is' = runByteCodeInstruction is (send stmt1)
            doWhile is' (send stmt2) (send stmt3) (send stmt4)
            return ()

        doWhile :: Eff '[ByteCodeInstruction] Bool -> Eff '[ByteCodeInstruction] w -> Eff '[ByteCodeInstruction] x -> State InterpreterState ()
        doWhile stmt2 stmt3 stmt4 = do
            is <- get
            --is' <- loopM (\(st, map) -> stackPeek st) is
            --put is'
            return ()-}

        binaryOp :: ByteCodeInstruction v -> (forall a. Integral a => a -> a -> a) -> Eff '[Error (), State InterpreterState] ()
        binaryOp instruction op = do
            (st, map) <- get
            let (stack1, stack2, val1, val2) = case (stackPop st) of
                                Nothing -> error "Error (Add/Sub/Mul/Div instruction): Less than 2 elements in the stack"
                                Just (st1, v1) -> case (stackPop st1) of
                                                    Nothing -> error "Error (Add/Sub/Mul/Div instruction): Less than 2 elements in the stack"
                                                    Just (st2, v2) -> (st1, st2, v1, v2)
            let result = val1 `op` val2
            let stack3 = stackPush stack2 result
            put (stack3 :: Stack Int, map :: Map.Map String Int)
            return ()        


runByteCode :: ByteCode a -> InterpreterState
runByteCode [] = (stackNew :: Stack Int, Map.empty)
runByteCode byteCode@(x:xs) = let initialInterpreterState = (stackNew :: Stack Int, Map.empty)
    in Prelude.foldl (\interpreterState byteCodeInstruction -> runByteCodeInstruction interpreterState (send byteCodeInstruction))
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
