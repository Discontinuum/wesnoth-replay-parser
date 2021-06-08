module Main where

import System.Environment (getArgs)
import Data.List (intercalate)
import qualified System.IO as IO
import WMLParser
import Analysis

main :: IO ()
main = do
  args <- getArgs
  fileContent <- IO.readFile $ args !! 0
  let Right doc = parseWML fileContent
  let (Tag "scenario" scVal) = (getTagsByName "scenario" doc) !! 0
  let sides = map parseSide (getTagsByName "side" scVal)
  let sidesStrings = map (\s -> (player s) ++ "," ++ (faction s)) sides
  let mpTag = (getTagsByName "multiplayer" doc) !! 0
  let mp = parseMultiplayer mpTag
  putStrLn $ intercalate ";" ([scenarioId mp] ++ sidesStrings)
