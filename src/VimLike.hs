{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module VimLike
  (
    runVimList
  , keepApp
  , showBoth
  , showSearch
  , showSurroundList
  , showCurrent
  , showUsingLine
  ) where

import Rainbow
import System.Process
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B
import Data.List.Split (splitOn)
import System.Console.ANSI
import Control.DeepSeq
import Control.Seq
import Control.Monad

-- Helper functions
applyMany 0 f x = x
applyMany n f x | n >= 0 = (x `using` seqTuple2 (seqListN 1 rdeepseq) (seqListN 1 rdeepseq)) `seq` applyMany (n-1) f (f x)
                | otherwise = x


move n | n >= 0 = applyMany n next
       | otherwise = applyMany (abs n) previous


next a@(_, []) = a
next (xs, ys@[_]) = (xs, ys)
next (xs, (y:ys)) = ((y:xs), ys)


previous (xs@[], ys) = (xs, ys)
previous ((x:xs), ys) = (xs, (x:ys))


beginning a@([], _) = a
beginning (x:xs, ys) = beginning (xs, x:ys)

end a@(_, []) = a
end a@(_, [y]) = a
end a@(xs, y:ys) = end (y:xs, ys)

search se s a@(xs, ys) = case span (not . se s . snd) ys of
  (_, []) -> a
  (ds, rs) -> (reverse ds ++ xs, rs)


searchBackward se s a@(xs, ys) = case span (not . se s . snd) xs of
  (_, []) -> a
  (ds, r:rs) -> (rs , r:reverse ds ++ ys)


-- Main exported function
runVimList fn se f xs = keepApp fn f vimStatemode $
  VimState {
    number = Nothing
  , buffer = M.empty
  , searchTerm = ""
  , currentMode = defaultVim se
  , getValue = ([], zip [1..] xs)
  , lastJumped = 1
  }

-- Printing convenience functions
showSurroundList (fl1, fl2) (xs, (y:ys)) = mapM_ f (take 20 (zip (repeat False) (reverse (take 10 xs)) ++ ((True, y) :zip (repeat False) ys)))
  where f (True, x) = fl1 x
        f (False, x) = fl2 x

showSearch x = (putChunk $ chunk ("Search Term: " :: B.ByteString) & italic) >> case searchTerm x of
  "" -> B.putStrLn ""
  s -> B.putStr s

showBoth (f, fl1, fl2) x = f (getCurrent x) >> replicateM_ 3 (putStrLn "") >> showSurroundList (fl1, fl2) (getValue x) >> putStrLn "" >> showSearch x 

showCurrent f x = showSearch x  >> replicateM_ 2 (putStrLn "") >> (f . getCurrent) x


viewChunkList l (n, b) = B.pack (take 4 (show n ++ ('.' : repeat ' '))) `B.append` l b

printListHighlighted l c = (putChunkLn $ chunk (B.unpack $ viewChunkList l c) & fore red)
printListUnHighlighted l c = B.putStrLn $ viewChunkList l c

showUsingLine (f, l) = showBoth (f, printListHighlighted l, printListUnHighlighted l)
-- Modes


type Mode a = Char -> a -> Maybe a

keepApp :: FilePath -> (a -> IO ()) -> Mode a -> a -> IO ()
keepApp fn f mo a =
  do system "clear" >> hideCursor 
     putChunkLn $ chunk ("     " ++ (map toUpper $ takeWhile (/='.') $ last $ splitOn "/" fn)) & bold
     putStrLn "" >> f a
     c <- getChar
     if (c == 'q')
       then showCursor >> return ()
       else 
         case mo c a of
           Nothing -> keepApp fn f mo a 
           Just z -> keepApp fn f mo z

-- VimState

data VimState a = VimState {
    number :: (Maybe Int)
  , buffer :: (M.Map Char Int)
  , currentMode :: (Mode (VimState a))
  , getValue :: a
  , searchTerm :: B.ByteString
  , lastJumped :: Int
  }

getCurrent = head . snd . getValue

multipleApp f v = v {
  getValue = case number v of
      Nothing -> f (getValue v)
      Just n ->  applyMany n f (getValue v)
  , number = Nothing
  --, lastJumped = fst (getCurrent v)
  }


vimStatemode c a = currentMode a c a



-- Various convenient modes

searchMode se sf mo' c v = Just $
  case c of
    '\n' -> v {currentMode = mo', getValue = sf se (searchTerm v) (getValue v)}
    '\DEL' -> v {searchTerm = B.reverse (tail' (B.reverse (searchTerm v))) }
    '\ESC' -> v {searchTerm = "", currentMode = mo'}
    c' -> v {searchTerm = searchTerm v `B.append` B.pack [c']}
  where tail' "" = ""
        tail' x = B.tail x

mapMode mo' c v = Just $
  v { buffer = (case (snd (getValue v)) of
                ((i,_):_) -> M.insert c i (buffer v)
                _ -> (buffer v))
    , currentMode = mo' }


mapRetrieveMode mo' x v = Just $
  (buffer v) `seq` v {getValue =  (case (snd (getValue v)) of
                                    ((n',_):_) -> move (M.findWithDefault n' x (buffer v) - n') (getValue v)
                                    _ -> getValue v), currentMode = mo'}


setLastJumped v x = Just $ x {lastJumped = fst (getCurrent v)}

defaultVim se c a = case c of
  'j' -> just $ multipleApp next a
  'k' -> just $ multipleApp previous a
  'm' -> just $ a {currentMode = (mapMode (currentMode a)) }
  '`' -> just $ a {currentMode = (mapRetrieveMode (currentMode a)) }
  '/' -> just $ a {searchTerm = "", currentMode = searchMode se search (currentMode a)}
  '?' -> just $ a {searchTerm = "", currentMode = searchMode se searchBackward (currentMode a)}
  'n' -> just $ multipleApp (search se (searchTerm a)) a
  'p' -> just $ multipleApp (searchBackward se (searchTerm a)) a
  'G' -> just $ a {getValue = end (getValue a)}
  'g' -> just $ case number a of Nothing -> a {getValue = beginning (getValue a)}; _ -> multipleApp next (a {getValue = beginning (getValue a), number = (\x -> x - 1) <$> number a})
  'o' -> Just $ multipleApp next (a {getValue = beginning (getValue a), number = Just (lastJumped a - 1)})
  y -> if isDigit y
         then let n' = read [y] in
                Just (case (number a) of
                        Nothing -> a { number = Just n'}
                        Just n'' -> a { number = Just (10*n'' + n')})
         else Nothing
  where just = setLastJumped a
