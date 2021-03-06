{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import ParseShak
import Shak
import VimLike
import System.Environment
import System.IO
import qualified Data.ByteString.Char8 as B
import System.Console.ANSI


runChunks fn = runVimList fn searchLine (showCurrent printChunk) . parseChunks


main :: IO ()
main = do (s:_) <- getArgs
          hSetBuffering stdin NoBuffering
          l <- B.readFile s
          runChunks s l
       
