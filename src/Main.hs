module Main where

import System.Environment (getArgs)
import qualified System.IO as IO
import WMLParser
import Analysis

main :: IO ()
main = do
  args <- getArgs
  fileContent <- IO.readFile $ args !! 0
  let Right doc = parseWML fileContent
  let (Tag "scenario" val) = (getTagsByName "scenario" doc) !! 0
  let replay = (getTagsByName "replay" doc) !! 1
  putStrLn $ show (map parseSide (getTagsByName "side" val))
  let chat = parseChat replay
  putStrLn $ show chat
  let chatMes (nick, mes) = nick ++ ": "++ mes
  mapM_ (putStrLn.chatMes) chat
