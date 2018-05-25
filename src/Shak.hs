{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
module Shak
    (
      act
    , scene
    , charactersPresent
    , lastCharacter
    , lineType
    , lastBlank
    , line
    , emptyLine
    , LineType (..)
    , printChunk
    , lineNum
    , searchLine
    , Line
    ) where

import Control.Lens
import System.Console.ANSI
import qualified Data.ByteString.Char8 as B
import Control.DeepSeq
import GHC.Generics

data LineType = Speech | Stage deriving (Show, Eq, Generic, NFData)

data Line = Line {
    _act :: B.ByteString
  , _scene :: B.ByteString
  , _charactersPresent :: [B.ByteString]
  , _lastCharacter :: B.ByteString
  , _lineType :: LineType
  , _line :: B.ByteString
  , _lastBlank :: Bool
  , _lineNum :: Int
  } deriving (Show, Generic, NFData)

makeLenses ''Line
emptyLine = Line "" "" [] "" Stage "" True 1




printChunk :: (Int, [Line]) -> IO ()
printChunk (_, []) = return ()
printChunk (n, l:ls) = do
  B.putStrLn $ "Screen: " `B.append` B.pack (show  n)
  B.putStrLn ""
  colorStrLn Vivid Cyan Dull Black (view act l)
  colorStrLn Vivid Blue Dull Black (pad $ view scene l)
  B.putStrLn ""
  colorStrLn Vivid Yellow Dull Black (("Characters present: " `B.append`) $ showStringList $ view charactersPresent l)
  B.putStrLn ""
  case view lineType l of
    Stage -> B.putStrLn "" >> colorStrLn Vivid Green Dull Black (view line l) >> B.putStrLn ""
    _ -> colorStrLn Vivid Red Dull Black (view lastCharacter l `B.append` ":") >> mapM_ (printLine) (if view line l /= "."  && view line l /= ". " then l : ls else ls)


printLine l = case view lineType l of
  Stage -> B.putStrLn "" >> colorStrLn Vivid Green Dull Black (pad $ view line l) >> B.putStrLn ""
  _ -> B.putStrLn $ pad $ view line l



showStringList [] = ""
showStringList [x] = x
showStringList [x,y] = x `B.append` ", " `B.append` y
showStringList (x:xs) = x `B.append` ", " `B.append` showStringList xs



showLine l = case l ^. lineType of
  Stage -> colorStrLn Vivid Green Dull Black (B.pack $ show $ view line l)
  _ -> B.putStrLn (view line l)



showChunk [] = ""
showChunk (l:ls) = view act l `B.append` "\n" `B.append` view scene l `B.append` "\n" `B.append` (B.pack . show) (view charactersPresent l) `B.append` "\n" `B.append` view lastCharacter l `B.append` "\n\n" `B.append` B.unlines (map (view line) ls)




colorStrLn fgi fg bgi bg str = do
  setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
  B.putStr str
  setSGR []
  B.putStrLn ""

pad = B.append "    "

searchLine s ss' = any (\s' -> s `B.isInfixOf` view line s') ss'


