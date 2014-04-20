{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Exception        (bracket)
import           Control.Monad
import           Data.TinyMap
import qualified Data.TinyMap             as M
import           Network                  (PortID (..))
import           System.Exit
import           System.Random
import           System.Remote.Monitoring

type Map = TinyMap Integer String

secs = (*1000000)

main :: IO ()
main = do forkServer "localhost" 8089
          go M.empty
  where
    go :: Map -> IO ()
    go hmap = do
          num <- randomRIO (1,100000000000) :: IO Integer
          let newMap = M.insert num "cool" hmap
          if M.size newMap > 60000
             then exitSuccess
             else print $ M.size newMap
          go newMap
