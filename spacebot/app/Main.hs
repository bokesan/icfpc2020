module Main where

import System.Environment
import Network.HTTP.Simple
import Data.ByteString.Lazy.UTF8 as BLU
import Control.Exception
import Data.List

import SExpr
import Types

main = catch (
    do (serverUrl : playerKey : _) <- getArgs
       gameResponse1 <- send serverUrl (makeJoinRequest playerKey)
       putStrLn ("join response: " ++ show gameResponse1)
       gameResponse2 <- send serverUrl (makeStartRequest playerKey gameResponse1)
       putStrLn ("start response: " ++ show gameResponse2)
       let r = parseResponse gameResponse2
       play serverUrl playerKey r
    ) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex

send :: String -> String -> IO SExpr
send serverUrl body
   = do request' <- parseRequest ("POST " ++ serverUrl)
        let request = setRequestBodyLBS (BLU.fromString body) request'
        response <- httpLBS request
        let statuscode = show (getResponseStatusCode response)
        case statuscode of
            "200" -> let resp = BLU.toString (getResponseBody response)
                         sex = parseSExpr resp in
                        case sex of
                          Left err -> do putStrLn ("parse error: " ++ show err)
                                         return serr
                          Right expr -> return expr
            _ -> do putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
                    return serr

serr :: SExpr
serr = Cons (Int 0) Nil

play :: String -> String -> GameResponse -> IO ()
play serverUrl playerKey r | not (success r)  = putStrLn "Server returned error response"
                           | stage r == Ended = putStrLn "Game ended"
                           | otherwise = do r' <- turn serverUrl playerKey r
                                            play serverUrl playerKey r'

turn :: String -> String -> GameResponse -> IO GameResponse
turn serverUrl playerKey r = let myShips = [n | (ship,cmds) <- shipsAndCommands (state r)
                                              , shipRole ship == myRole (staticInfo r)
                                              , let n = shipId ship ]
                                 cmds = [Accelerate n (Vec 0 0) | n <- myShips]
                    in do response <- send serverUrl (makeCommandsRequest playerKey cmds)
                          putStrLn ("cmds response: " ++ show response)
                          let r = parseResponse response
                          return r

makeCommandsRequest :: String -> [Command] -> String
makeCommandsRequest playerKey cmds = "(4, " ++ playerKey ++ ", " ++ listReq cmds ++ ")"

makeJoinRequest :: String -> String
makeJoinRequest playerKey = "(2, " ++ playerKey ++ ", nil)"

makeStartRequest :: String -> SExpr -> String
makeStartRequest playerKey joinResponse = "(3, " ++ playerKey ++ ", (-30, -30, 10, 10))"

listReq :: Show a => [a] -> String
listReq xs = '(' : intercalate ", " (map show xs) ++ ")"

testSExpr :: String -> IO ()
testSExpr s = do let e = parseSExpr s
                 putStr "ORIG "
                 putStrLn s
                 putStr "PARS "
                 print e