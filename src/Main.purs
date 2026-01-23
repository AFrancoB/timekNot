module Main where

import Prelude
import Data.Either
import Data.Maybe
import Effect
import Effect.Now
import Effect.Ref
import Effect.Class
import Effect.Console (log)
import Data.Tempo
import Effect.Ref (new,write)
import Data.Traversable (traverse, traverse_)

import Data.Rational
import Data.List.Lazy hiding (many,Pattern)
import Data.Array as A

import Data.List as L

import Data.Map as M
import Data.Tuple

import Foreign 

import Partial.Unsafe
import Data.Enum

-- import Data.DateTime (DateTime(..))

-- import Data.Rational (Rational(..), (%), fromInt, toNumber)
import Data.DateTime
import Data.DateTime.Instant hiding (diff)
import Data.Time.Duration


import Data.Newtype

-- import Halogen as H
-- import Halogen.Aff as HA
-- import Halogen.HTML as HH
-- import Halogen.HTML.Properties as HP
-- import Halogen.HTML.Events as HE
-- import Halogen.VDom.Driver (runUI)

-- import Visualisation
-- import Svg.Parser

import WebDirt

import AST
import TimePacketOps
import Parser
import Voices
import Novus
import AssambleWebdirt

import Parsing

main :: Effect Unit 
main = pure unit


---- Muestra de notacion de convergencia: en el horizonte: convergence expression
-- a[1: 13 <~ 1 <~~ 3 <~ 10 ~> 6 ] <- reloj[20]

-- a-0[6] <- a-3[10]
-- a-1[13] <- reloj[20]
-- a-2[13] <- a-0[1]
-- a-3[13] <- a-1[3]
-- a-4[3] <- a-2[10]

-- 13 <- 1
-- 13 <- 3
-- 3 <- 10
-- 6 <- 10

-- test editor view
-- grid 2 1 [[label 1,code 2 0 []],[[label 5,code 3 0 []],[metre 2666]]]

-- next steps: ask david what to do with clean before I deploy. 

launchDirt :: Effect WebDirt
launchDirt = do
  dirt <- newWebDirt { sampleMapUrl: "./src/samples/sampleMap.json", sampleFolder: "./src/samples" }
  initializeWebAudio dirt
  pure dirt


launch:: {} -> Effect TimekNot
launch _ = do
  log "timekNot: launch"
  launchTime <- nowDateTime
  programs <- new $ M.empty
  tempo <- newTempo (1 % 1) >>= new 
  vantageMap <- new $ (M.empty)
  wS <- new launchTime
  wE <- new launchTime
  pure {tempo, vantageMap, wS, wE, programs}  




{- ISSUE: evaluation happens after you changed the program so, it is all off by one.... when you reset it does not come into effect after the second time you evaluate, this sucks sooo bad! -}

-- { zone :: Int, time :: Number, text :: String }
define:: TimekNot -> { zone :: Int, time :: Number, text :: String } -> Effect { success :: Boolean, error :: String }
define tk args = do
  log "timekNot: evaluate"
  prs <- read tk.programs
  -- program <- read tk.program -- this does not do anything, can be erased...?
  currentVM <- read tk.vantageMap
  log $ "currentVM" <> show currentVM
  -- log $ "programDefined: " <> show program
  tempo <- read tk.tempo
  eval <- nowDateTime
  let pr = check' currentVM $ runParser args.text $ parseProgram
  case pr of
    Left error -> pure $ { success: false, error }
    Right p -> do 
      let newPrograms = zoneToPrograms args.zone eval p prs
      

      write newPrograms tk.programs
      write (processVantage (getVantageMap p) currentVM eval tempo) $ tk.vantageMap
      pure $ { success: true, error: "bad syntax" }

zoneToPrograms:: Int -> DateTime -> Program -> Programs -> Programs -- associates a zone to a program (with an eval time) and passes forward a Map with the programs - the program for that zone
zoneToPrograms z eval pr prs = M.union (M.singleton z $ Tuple eval pr) prs


-- type TimekNot = {
--   programs :: Ref Programs,
--   program :: Ref Program,  -- this gets erased
--   tempo :: Ref Tempo,
--   evalCount :: Ref Int,
--   previousEval :: Ref DateTime,
--   vantageMap :: Ref (Map String DateTime),
--   wS :: Ref DateTime,
--   wE :: Ref DateTime
--   }

-- type Programs = Map Int (Tuple DateTime Program)

-- type Program = List Expression


-- programToZonedPrograms:: 


check':: VantageMap -> Either ParseError Program -> Either String Program
check' vm (Left error) = Left $ parseErrorMessage error
check' vm (Right aProgram) = case check vm aProgram of
                              true -> Right aProgram
                              false -> Left "failed the check, time bites it's own tail"

-- { zone :: Int, windowStartTime :: Number, windowEndTime :: Number }
render:: TimekNot -> {zone :: Int, windowStartTime :: Number, windowEndTime :: Number} -> forall opts. Effect (Array Foreign)
render tk args = do
    let ws = numToDateTime (args.windowStartTime * 1000.0000) -- haskell comes in milliseconds, purescript needs seconds
    let we = numToDateTime (args.windowEndTime * 1000.0000)

    programs <- read tk.programs

    v <- read tk.vantageMap
    -- log $ "vm: " <> show vantageMap
    t <- read tk.tempo 

    let tp = {ws: ws, we: we, origin: origin t, tempo: t, vantageMap: v}


    map A.concat $ traverse (\pr -> programToForeign pr tp) $ A.fromFoldable $ M.values programs

    -- log $ show program
    -- log $ "wsR: " <> show (fromDateTimeToPosix ws)
    -- log $ show we
    -- log $ show t
    -- programToForeign (Tuple eval program) tp
 -- programToForeign:: Program -> TimePacket -> Effect (Array Foreign)

-- type Programs = Map Int (Tuple DateTime Program)

-- getProgramByZone:: Int -> Programs -> Tuple DateTime Program
-- getProgramByZone z prs = 



setTempo:: TimekNot -> ForeignTempo -> Effect Unit
setTempo tk t = do
  -- log $ "setTempo is called" <> show (fromForeignTempo t)
  write (fromForeignTempo t) tk.tempo    

renderStandalone :: TimekNot -> {webdirt:: WebDirt} -> Effect Unit
renderStandalone tk d = do 
  now <- nowDateTime  
  prevWE <- read $ tk.wE  -- 500
  let future = fromMaybe now $ adjust (Milliseconds 400.00) now -- :: Milliseconds -- 400
  if prevWE <= future then do
    let wS = prevWE
    let wE = fromMaybe now $ adjust (Milliseconds 500.0) wS 
    -- y <- log $ "wsS: " <> show (fromDateTimeToPosix wS)
    -- z <- log $ "we: " <> show (fromDateTimeToPosix wE)
    write wS tk.wS
    write wE tk.wE
    t <- read $ tk.tempo -- is this usefull??
    -- pure unit
    playDirty tk d.webdirt
  else
    log $ show "sleep"

playDirty:: TimekNot -> WebDirt -> Effect Unit
playDirty tk dirt = do
  wStart <- read tk.wS
  wEnd <- read tk.wE
  events <- render tk {zone: 0, windowStartTime: fromDateTimeToPosix $ wStart, windowEndTime: fromDateTimeToPosix $ wEnd} -- Effect (Array Foreign)
  x <- traverse_ (\x -> playSample dirt $ unsafeFromForeign x) events  -- type of this?? Unit
  -- _ <- traverse_ (\x -> pianola "pianola" x) events
  pure x