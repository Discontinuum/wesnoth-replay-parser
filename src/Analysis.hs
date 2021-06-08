module Analysis where
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import WMLParser

data Side = Side {
  player :: String,
  faction :: String,
  leader :: String,
  host :: Bool,
  human :: Bool
} deriving Show

parseSide :: WML_Entity -> Side
parseSide sideTag =
  let
    attrs = getAttrsMap (getTagValue sideTag)
    p = M.findWithDefault "N/A" "current_player" attrs
    f = M.findWithDefault "N/A" "faction" attrs
    l = M.findWithDefault "N/A" "type" attrs
    hmn = (M.findWithDefault "null" "controller" attrs) == "human"
    hst = isTrue $ M.findWithDefault "no" "is_host" attrs
  in
    Side{player = p,faction=f,leader=l,human=hmn,host=hst}
    
parseChat :: WML_Entity -> [(String, String)]
parseChat replayTag =
  let
    commands = getTagsByName "command" (getTagValue replayTag)
    transform (Tag "command" val) = 
      let
        fun speak = (M.findWithDefault "N/A" "id" m, M.findWithDefault "" "message" m)
          where m = getAttrsMap $ getTagValue speak
      in
        fmap fun (getTagByName "speak" val)
  in
    mapMaybe transform commands
   
   
data Multiplayer = MP {
  title :: String,
  scenarioId :: String,
  mapName :: String,
  eraId :: String,
  eraName :: String,
  shuffle :: Bool,
  observers :: Bool,
  timerEnabled :: Bool
}

parseMultiplayer tag =
  let
    attrs = getAttrsMap (getTagValue tag)
    sh = isTrue $ M.findWithDefault "no" "shuffle_sides" attrs
    t = M.findWithDefault "" "scenario" attrs
    scId = M.findWithDefault "" "mp_scenario" attrs
    mN = M.findWithDefault "" "mp_scenario_name" attrs
    eId = M.findWithDefault "" "mp_era" attrs
    eN = M.findWithDefault "" "mp_era_name" attrs
    o = isTrue $ M.findWithDefault "no" "observer" attrs
    tE = isTrue $ M.findWithDefault "no" "mp_countdown" attrs
  in
    MP{title=t, mapName=mN, eraId=eId, eraName=eN, observers=o,timerEnabled=tE, shuffle=sh,scenarioId=scId}
