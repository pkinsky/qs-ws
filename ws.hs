{-# LANGUAGE OverloadedStrings #-}
import Control.Monad      (forever)
import qualified Data.Text          as T
import qualified Network.WebSockets as WS
import Data.Text (Text)
import qualified Data.Text.IO as T
import Control.Exception (finally)
import Data.Array.IO
import Data.Heap (singleton, MinPrioHeap, view, insert)
import Control.Concurrent as CC
import Text.Read as TR

main :: IO ()
main = print "starting" >> WS.runServer "0.0.0.0" 9998 app

app :: WS.PendingConnection -> IO ()
app pending = do conn <- WS.acceptRequest pending
                 meow conn



meow :: WS.Connection -> IO ()
meow conn =  do msg <- WS.receiveData conn
                case TR.readMaybe (T.unpack msg)  of
                  Just(xs) ->
                    do arr <- mkArr xs
                       let disconnect = print "disconnecting" -- I guess there's no dealloc function function for IOArray
                           log t = WS.sendTextData conn (T.pack t) >> CC.threadDelay (20 * 1000)
                       flip finally disconnect $ qs log arr
                  Nothing -> return ()


actionP pivot st end = "{\"type\": \"partition\", \"pivot\": " ++ show pivot ++ ", \"st\": " ++ show st ++ ", \"end\": " ++ show end ++ "}"
actionS i j = "{\"type\": \"swap\", \"i\": " ++ show i ++ ", \"j\": " ++ show j ++ "}"

mkArr :: [Int] -> IO (IOArray Int Int)
mkArr xs = newListArray (0, length xs - 1) xs

type Interval = (Int, Int)
type PrintF = String -> IO ()

qs :: PrintF -> IOArray Int Int -> IO ()
qs pf arr = do (st, end) <- getBounds arr 
               let heap = singleton (end - st, (st, end)) :: MinPrioHeap Int Interval
               loop heap
  where loop h = case view h of
                   (Just ((_, interval), h')) ->
                     do intervals <- partition pf arr interval
                        let insert' h (a,b) = insert (b-a, (a,b)) h
                            h'' = foldl insert' h' intervals
                        loop h''
                   Nothing -> return ()


partition :: PrintF -> IOArray Int Int -> Interval -> IO [Interval]
partition pf arr (st, end) =
    do pivot <- readArray arr st
       pf $ actionP st st end
       loop (st + 1) (st + 1) pivot
  where swap i0 i1 | i0 /= i1 = 
          do pf $ actionS i0 i1
             v0 <- readArray arr i0
             v1 <- readArray arr i1
             writeArray arr i0 v1
             writeArray arr i1 v0
                   | otherwise = return ()
        loop i j p | i <= end =
          do x <- readArray arr i
             if x < p
               then swap i j >> loop (i+1) (j+1) p
               else loop (i+1) j p
                   | otherwise =
          do swap st (j - 1)
             return $ filter (uncurry (<)) [(st, j - 2), (j, end)]


printA :: IOArray Int Int -> IO String
printA a = getBounds a >>= printA' a

printA' :: IOArray Int Int -> Interval -> IO String
printA' a (st,end) = 
           do xs <- tl a st end 
              return $ show xs

tl:: IOArray Int Int -> Int -> Int -> IO [Int]
tl arr a b = mapM (readArray arr) [a..b]
