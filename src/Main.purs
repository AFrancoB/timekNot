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
import Data.DateTime
import Effect.Ref (new,write)
import Data.Traversable

import Data.Rational
import Data.List.Lazy hiding (many,Pattern)
import Data.Array as A

import Data.List as L

import Data.Map as M
import Data.Tuple

import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration

import Foreign 

import Partial.Unsafe
import Data.Enum

import Data.Newtype

import WebDirt

import Parsing

import AST
import Rhythmic
import Motor

main :: Effect Unit 
main = pure unit

--launched from js end. provides a wd instance
launchDirt :: Effect WebDirt
launchDirt = do
  dirt <- newWebDirt { sampleMapUrl: "samples/sampleMap.json", sampleFolder: "samples" } -- make this the proper sampleMap!!!!!
  initializeWebAudio dirt
  pure dirt

launch :: Effect TimekNot
launch = do
  log "timekNot-CU: launch"
  launchTime <- nowDateTime
  ast <- new $ Passage (Onsets $ L.fromFoldable []) (L.fromFoldable []) Origin true
  tempo <- newTempo (1 % 1) >>= new -- why the bind? --- break into two lines
  eval <- new launchTime
  wS <- new launchTime
  wE <- new launchTime
  pure { ast, tempo, eval, wS, wE}  

renderStandalone :: TimekNot -> WebDirt -> Effect Unit
renderStandalone tk dirt = do 
  now <- nowDateTime  -- what is this?
  prevWE <- read $ tk.wE 
  let future = fromMaybe now $ adjust (Milliseconds 100.00) now -- :: Milliseconds
  if prevWE <= future then do
    let wS = prevWE
    let wE = fromMaybe now $ adjust (Milliseconds 100.0) wS 
    write wS tk.wS
    write wE tk.wE
    t <- read $ tk.tempo -- is this usefull??
    
    playDirts dirt tk
  else
    log $ show "sleep"

playDirts:: WebDirt -> TimekNot -> Effect Unit
playDirts dirt tk = do
  events <- scheduleEventsStandAlone tk
  x <- traverse_ (\x -> playSample dirt x) events  -- type of this??
  pure x

scheduleEventsStandAlone:: TimekNot -> Effect (Array {whenPosix:: Number, s:: String, n:: Int})
scheduleEventsStandAlone tk = do
    ast <- read tk.ast
    t <- read tk.tempo
    eval <- read tk.eval
    ws <- read tk.wS
    we <- read tk.wE
    pure $ fromPassageToArray ast t ws we eval

evaluate :: TimekNot -> String -> Effect { success :: Boolean, error :: String }
evaluate timekNot str = do
  log "timekNot-CU: evaluate"
  passage <- read timekNot.ast
 -- log $ show passage
  -- placeholder: assume any evaluation yields the valid program
  eval <- nowDateTime
  let pr = pErrorToString $ runParser str topPassageParser -- :: Either String AST 
  case pr of
    Left error -> pure $ { success: false, error }
    Right p -> do
      write eval timekNot.eval --- esto!!!!
      write p timekNot.ast 
      pure $ { success: true, error: "" }    --- here you might add the eval time with purescript. 

pErrorToString:: Either ParseError Passage -> Either String Passage
pErrorToString (Left x) = Left $ parseErrorMessage x
pErrorToString (Right x) = Right x

setTempo :: TimekNot -> ForeignTempo -> Effect Unit
setTempo timekNot t = write (fromForeignTempo t) timekNot.tempo

-- here a func that goes from rhythmicInto (passing through map) Event
scheduleNoteEvents :: TimekNot -> DateTime -> DateTime -> forall opts. Effect (Array Foreign)
scheduleNoteEvents tk ws we = timekNotToForeigns tk ws we

-- make unsafe function and correct with david's advice later
numToDateTime:: Number -> DateTime 
numToDateTime x =
      let asMaybeInstant = instant $ Milliseconds x -- Maybe Instant
          asInstant = unsafeMaybeMilliseconds asMaybeInstant
      in toDateTime asInstant 

unsafeMaybeMilliseconds:: Maybe Instant -> Instant
unsafeMaybeMilliseconds (Just x) = x
unsafeMaybeMilliseconds Nothing = unsafeMaybeMilliseconds $ instant $ Milliseconds 0.0

timekNotToForeigns:: TimekNot -> DateTime -> DateTime -> forall opts. Effect (Array Foreign)
timekNotToForeigns tk ws we = do
--    let ws' = numToDateTime (ws * 1000.0000)
--    let we' = numToDateTime (we * 1000.0000)
    passage <- read tk.ast
    t <- read tk.tempo
    eval <- read tk.eval

    log $ show passage
    log $ show ws
    log $ show we
    let events = fromPassageToArray passage t ws we eval
    log $ show events
    pure $ map unsafeToForeign events

fromPassageToArray:: Passage -> Tempo -> DateTime -> DateTime -> DateTime -> Array {whenPosix:: Number, s:: String, n:: Int}
fromPassageToArray pass t ws we eval = 
    let events' = passageToEvents pass t ws we eval -- List (Maybe Event)
        events = toUnfoldable $ filterMaybe events' -- Array
      in events


filterMaybe:: List (Maybe Event) -> List Event
filterMaybe x = map withoutMaybe $ filter justJust x

withoutMaybe:: Maybe Event -> Event
withoutMaybe (Just x) = x
withoutMaybe Nothing = {whenPosix: 0.0, s: "", n: 0}

justJust:: Maybe Event -> Boolean
justJust (Just _) = true
justJust Nothing = false

coordToEvent:: Coordenada -> {whenPosix:: Number, s:: String, n:: Int}
coordToEvent (Coord num iEv iPas) = {whenPosix: num, s: "cp", n: 0 }

testMaybeInstant:: Number -> Maybe Instant
testMaybeInstant x = instant $ Milliseconds x -- Maybe Instant
