{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module ParseShak
    ( 
      parseChunks
    ) where

import Shak
import Data.List
import Data.Char
import Data.List.Split
import Control.Monad.State
import Control.Applicative
import Control.Lens
import qualified Data.ByteString.Char8 as B




oprt cs l s = case getAct l s
                <|> getScene l s
                <|> getStage l s
                <|> getCharac cs l s
                <|> (Just (
                        l & line .~ s
                          & lineType .~ Speech
                        )) of
  Just l' -> checkBlank s $ l' & charactersPresent .~ chngChrs cs (l' ^. lastCharacter) s (l' ^. charactersPresent)
  Nothing -> l



getAct l s | "ACT" `B.isPrefixOf` s || "Act" `B.isPrefixOf` s = Just
             (
                 l
               & act .~ s
               & line .~ ""
               & lineType .~ Stage
               & charactersPresent .~ []
             )
           | otherwise = Nothing

           
getScene l s | "Scene" `B.isPrefixOf` s || "SCENE" `B.isPrefixOf` s = Just
               (
                   l
                 & scene .~ s
                 & line .~ ""
                 & lineType .~ Stage
                 & charactersPresent .~ []
               )
             | otherwise = Nothing



getStage l s | B.length s == 0 = Nothing
             | B.head s == '['  = if
  "[Aside" `B.isPrefixOf` s
  then Nothing
  else Just (
                l
              & line .~ s
              & lineType .~ Stage
            )
             | otherwise = Nothing


getCharac = charByLastBlank

charByLastBlank cs l s | s == "" = Nothing
                       | B.length c >= 1 && l ^. lastBlank && isUpper (B.head c) && length (B.words c) <= 3 =
                   Just (l
                    & lastCharacter .~ c
                    & line .~ s')
                       | otherwise = Nothing
                 where (c, s') = B.span (/='.') s

checkBlank s l = l & lastBlank .~ (B.dropWhile (==' ') s == "" || B.dropWhile (==' ') s == "\r")


chngChrs cs c s = sort . nub . filter (\x -> B.length x >= 3 && x `isInCast` cs) . changeCharac c s

isInCast s cs = B.length s >= 2 && isUpper (B.head s) && any ((B.map toLower (B.take 3 s) `B.isInfixOf`) . B.map toLower) cs

changeCharac c s = map B.pack . chngeCharac' (show c) (show s) . map B.unpack

chngeCharac' c s | "[Enter" `isPrefixOf` s = (++(wordsBy (`elemS`"\n[], .") s \\ ["Enter", "and", "And"]))
                 | "Enter" `isInfixOf` s = (++ (wordsBy (`elemS`"\n[], .") s \\ ["Enter", "and", "And"]))
                 | "enter" `isInfixOf` s = (++ (wordsBy (`elemS`"\n[], .") s \\ ["Enter", "and", "And"]))
                 | "appear" `isInfixOf` s = (++ (wordsBy (`elemS`"\n[], .") s \\ ["Enter", "and", "And"]))
                 | "[Exit.]" `isInfixOf` s = filter (not . (take 3 c `isPrefixOf`))
                 | "[Exit" `isPrefixOf` s = (\\ (wordsBy (`elemS`"\n[], .") s ))
                 | "[Exeunt.]" == s = const []
                 | "[Exeunt" `isPrefixOf` s && "but" `isInfixOf` s = (\\  (wordsBy (`elemS`"\n[], .") s \\ ["Exeunt", "all", "but", "the"]))
                 | "[Exeunt" `isPrefixOf` s = (\\ (wordsBy (`elemS`"\n[], .") s ))
                 | otherwise = id


elemS :: Char -> String -> Bool
elemS = elem




parseChunks ls = (splitLong 30 . chunkify . removeBlanks . dropWhile ((=="") . view scene) .  toLines cs . removeBreaks) rs
  where (cs, rs) = getChars ls


splitLong n = concat . map (chunksOf n)


chunkify = groupBy chkEq


chkEq x l =  x ^. lastCharacter == l ^. lastCharacter && x ^. scene == l ^. scene  -- && x ^. lineType == l ^. lineType

toLines cs = scanl (oprt cs) emptyLine


getChars = span (\x -> not  (B.isPrefixOf "SCENE" x || B.isPrefixOf "Scene" x)) . dropWhile (\x -> not (B.isPrefixOf "PERSONS REPRESENTED" x || B.isPrefixOf "DRAMATIS PERS" x || B.isPrefixOf "Persons" x)) . B.lines

removeBlanks = filter ((/="") . view line)



removeBreaks [] = []
removeBreaks ("":ls) = "" : removeBreaks ls
removeBreaks (k:ls) | c=='[' && B.elem ']' l = k : removeBreaks ls
                    | c=='[' = case span (\s -> s == "" || B.last s /=']') ls of
                              (ls', (r:rs')) -> (k `B.append` " " `B.append` B.unlines ls' `B.append` " " `B.append`  r) : removeBreaks (rs')
                              (ls', a) -> (k `B.append` " " `B.append` B.unlines ls') : []
                    | otherwise = k : removeBreaks ls
                          where c = B.head k
                                l = B.tail k



testChunks ls = (dropWhile ((=="") . view scene) .  toLines cs . removeBreaks) rs
  where (cs, rs) = getChars ls


{-


charByList cs l s | l ^. lastBlank && isUpper (head c) && any ((map toLower c `B.isInfixOf`) . map toLower) cs = Just (l
                    & lastCharacter .~ c'
                    & line .~ s)
                  | otherwise = Nothing

  where c = takeWhile (/= '.') sp
        c' = takeWhile (/='.') s
        sp = case s of
          'F':'I':'R':'S':'T':' ':s' -> s'
          'F':'i':'r':'s':'t':' ':s' -> s'
          'S':'E':'C':'O':'N':'D':' ':s' -> s'
          'S':'e':'c':'o':'n':'d':' ':s' -> s'
          'T':'H':'I':'R':'D':' ':s' -> s'
          'T':'h':'I':'R':'D':' ':s' -> s'
          s'' -> s''

getCharac cs l s = case words s of
  w:ws -> case reverse w of
          '.':n -> let r = reverse n in
                    if length r >= 2 && any ((map toLower r `B.isInfixOf`) . map toLower) (cs)
                    then Just (l & lastCharacter .~ (reverse n) & line .~ s)
                    else Nothing
          _ -> Nothing
  _ -> Nothing
-}


{-










  











-}
