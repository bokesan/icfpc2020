module Main where

import System.Environment
import Network.HTTP.Simple
import Data.ByteString.Lazy.UTF8 as BLU
import Control.Exception
import Data.List

import SExpr
import Types

type PlayerKey = SExpr

main = catch (
    do (serverUrl' : playerKey' : _) <- getArgs
       putStrLn ("Server URL: " ++ serverUrl')
       putStrLn ("Player key: " ++ playerKey')
       let serverUrl = serverUrl' ++ "/aliens/send"
       let playerKey = Int (read playerKey')
       gameResponse1 <- send serverUrl (makeJoinRequest playerKey)
       gameResponse2 <- send serverUrl (makeStartRequest playerKey gameResponse1)
       let r = parseResponse gameResponse2
       play serverUrl playerKey r
    ) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex

send :: String -> SExpr -> IO SExpr
send serverUrl body
   = do putStrLn ("REQ:  " ++ show body)
        let body' = modulate body
        putStrLn ("REQ:  " ++ body')
        request' <- parseRequest ("POST " ++ serverUrl)
        let request = setRequestBodyLBS (BLU.fromString body') request'
        response <- httpLBS request
        let statuscode = show (getResponseStatusCode response)
        case statuscode of
            "200" -> do let resp = BLU.toString (getResponseBody response)
                        putStrLn ("RESP: " ++ resp)
                        case demodulate resp of
                          Left err -> do putStrLn ("parse error: " ++ show err)
                                         return serr
                          Right expr -> do putStrLn ("RESP: " ++ show expr)
                                           return expr
            _ -> do putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
                    return serr

serr :: SExpr
serr = Cons (Int 0) Nil

play :: String -> PlayerKey -> GameResponse -> IO ()
play serverUrl playerKey r | not (success r)  = putStrLn "Server returned error response"
                           | stage r == Ended = putStrLn "Game ended"
                           | otherwise = do r' <- turn serverUrl playerKey r
                                            play serverUrl playerKey r'

turn :: String -> PlayerKey -> GameResponse -> IO GameResponse
turn serverUrl playerKey r = let myShips = [n | (ship,cmds) <- shipsAndCommands (state r)
                                              , shipRole ship == myRole (staticInfo r)
                                              , let n = shipId ship ]
                                 cmds = [Accelerate n (Vec 0 0) | n <- myShips]
                    in do response <- send serverUrl (makeCommandsRequest playerKey cmds)
                          putStrLn ("cmds response: " ++ show response)
                          let r = parseResponse response
                          return r

makeCommandsRequest :: PlayerKey -> [Command] -> SExpr
makeCommandsRequest playerKey cmds = list [Int 4, playerKey, list (map toSExpr cmds) ]

makeJoinRequest :: PlayerKey -> SExpr
makeJoinRequest playerKey = list [Int 2, playerKey, Nil]

makeStartRequest :: PlayerKey -> SExpr -> SExpr
makeStartRequest playerKey joinResponse = list [Int 3, playerKey, list [Int (45), Int (-30), Int 5, Int 1] ]

listReq :: Show a => [a] -> String
listReq xs = '(' : intercalate ", " (map show xs) ++ ")"

testSExpr :: String -> IO ()
testSExpr s = do let e = parseSExpr s
                 putStr "ORIG "
                 putStrLn s
                 putStr "PARS "
                 print e