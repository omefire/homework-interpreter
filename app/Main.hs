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



data ByteCode r where
    LoadValue :: Int -> ByteCode ()
    WriteVariable :: String -> ByteCode ()
    ReadVariable :: String -> ByteCode ()
    Add :: ByteCode ()
    Sub :: ByteCode ()
    Div :: ByteCode ()
    Multiply :: ByteCode ()
    ReturnValue :: ByteCode ()

{- 
data ByteCode = LoadValue Int
    | WriteVariable String 
    | ReadVariable String 
    | Add 
    | Sub
    | Div
    | Multiply 
    | ReturnValue
-}


{- runByteCode :: Eff '[ByteCode, IO] a -> IO a
runByteCode = runM . interpretM (\case
    LoadValue 
    WriteVariable var -> ...
    ReadVariale var ->  ...
    Add -> ...
    Sub -> ...
    Div -> ...
    Multiply -> ...
    ReturnValue -> ...
) -}

main :: IO ()
main = someFunc
