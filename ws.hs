{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text            as T
import qualified Blaze.ByteString.Builder       as Blaze
import Control.Exception (finally)
import Data.Array.IO
import qualified Data.ByteString.UTF8 as BU
import Data.Heap (singleton, MinPrioHeap, view, insert)
import qualified Control.Concurrent as CC
import qualified Text.Read as TR
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.HTTP.Types as HTTP
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WWS
import qualified System.Environment as Env

main :: IO ()
main = do
    env <- Env.getEnvironment
    let port = maybe 9000 read $ lookup "PORT" env
    putStrLn $ "Listening on port " ++ show port
    page <- readFile "qs.html"
    let app _ respond = respond $ wrap page
        app' = WWS.websocketsOr WS.defaultConnectionOptions ws app
    run port app'


wrap :: String -> Response
wrap page = responseBuilder HTTP.status200 [ ("Content-Type", "text/html") ] $ Blaze.copyByteString $ BU.fromString page

ws :: WS.ServerApp
ws pending = do conn <- WS.acceptRequest pending
                print ("accept connection: " ++ host pending)
                msg <- WS.receiveData conn
                case TR.readMaybe (T.unpack msg)  of
                  Just xs ->
                    do arr <- newListArray (0, length xs - 1) xs
                       let disconnect = print ("disconnect: " ++ host pending) -- todo: deallocate array manually if possible
                           pushWS t = WS.sendTextData conn (T.pack t)
                       flip finally disconnect $ qs pushWS arr
                  Nothing -> return ()

host :: WS.PendingConnection -> String
host pending = maybe "???" BU.toString $ lookup "Host" headers
  where headers = WS.requestHeaders $ WS.pendingRequest pending

type Json = String

pivotJson :: Int -> Int -> Int -> Json
pivotJson pivot st end = "{\"type\": \"partition\", \"pivot\": " ++ show pivot ++ ", \"st\": " ++ show st ++ ", \"end\": " ++ show end ++ "}"

swapJson :: Int -> Int -> Json
swapJson i j = "{\"type\": \"swap\", \"i\": " ++ show i ++ ", \"j\": " ++ show j ++ "}"


type Interval = (Int, Int)
type PrintF = String -> IO ()

qs :: PrintF -> IOArray Int Int -> IO ()
qs pf arr = do (st, end) <- getBounds arr 
               let heap = singleton (end - st, (st, end)) :: MinPrioHeap Int Interval
               loop heap
  where loop h = case view h of
                   (Just ((_, interval), h')) ->
                     do intervals <- partition pf arr interval
                        let h'' = foldl insert' h' intervals
                        loop h''
                   Nothing -> return ()
        insert' h (a,b) = insert (b-a, (a,b)) h

partition :: PrintF -> IOArray Int Int -> Interval -> IO [Interval]
partition pf arr (st, end) =
    do pivot <- readArray arr st
       pf $ pivotJson st st end
       loop (st + 1) (st + 1) pivot
  where swap i0 i1 | i0 /= i1 = 
          do CC.threadDelay (20 * 1000)
             pf $ swapJson i0 i1
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
