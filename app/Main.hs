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



data ByteCode r where
    LoadValue :: Int -> ByteCode ()
    --WriteVariable :: String -> ByteCode ()
    {-ReadVariable :: String -> ByteCode ()
    Add :: ByteCode ()
    Sub :: ByteCode ()
    Div :: ByteCode ()
    Multiply :: ByteCode ()
    ReturnValue :: ByteCode ()-}
makeEffect ''ByteCode

runByteCode :: Stack Int -> Eff '[ByteCode] w -> Stack Int
runByteCode stack req = run $ execState stack (reinterpret go req)
    where
        go :: ByteCode v -> Eff '[State (Stack Int)] v
        go (LoadValue val) = do
            st <- get
            let st' = stackPush st val
            put st'
            return ()
        {-go (WriteVariable var) = do
            st <- get
            case (stackPop st) of
                Nothing -> throwError "Error (WriteVariable instruction): the stack is empty"
                Just (_, val) -> -}

main :: IO ()
main = do
  let stack = runByteCode (stackNew :: Stack Int) (send $ LoadValue 13)
  case (stackPop stack) of 
      Nothing -> putStrLn "Empty stack"
      Just (_, result) -> putStrLn $ show result
