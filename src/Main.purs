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
--import Data.List.Lazy hiding (many,Pattern)
import Data.Array as A

import Data.List

import Data.Map as M
import Data.Tuple

import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration

import Foreign 

import Partial.Unsafe
import Data.Enum

import Data.Newtype
import Debug

import WebDirt

import Parsing

import AST
import Parser 
import Motor

main :: Effect Unit 
main = pure unit

--launched from js end. provides a wd instance
launchDirt :: Effect WebDirt
launchDirt = do
  dirt <- newWebDirt { sampleMapUrl: "./src/samples/sampleMap.json", sampleFolder: "./src/samples" }
  initializeWebAudio dirt
  pure dirt

launch :: Effect TimekNot
launch = do
  log "timekNot-CU: my small change"
  log "prueba otra vez"
  launchTime <- nowDateTime
  ast <- new $ Program O false $ (S ("":Nil) ByEvent : Nil)
  tempo <- newTempo (1 % 1) >>= new -- why the bind? --- break into two lines
  eval <- new launchTime
  wS <- new launchTime
  wE <- new launchTime
  pure { ast, tempo, eval, wS, wE}  

-- handle the case where the previous window end is too much in the past. So: calculates events already past.
-- render all the way until caught up
-- way behind: jump forward <-  ***
renderStandalone :: TimekNot -> WebDirt -> Effect Unit
renderStandalone tk dirt = do 
  now <- nowDateTime  -- what is this?
  -- x <- log $ "now: " <> show now
  prevWE <- read $ tk.wE 
  let future = fromMaybe now $ adjust (Milliseconds 100.00) now -- :: Milliseconds
  if prevWE <= future then do
    let wS = prevWE
    let wE = fromMaybe now $ adjust (Milliseconds 100.0) wS 
    -- y <- log $ "ws: " <> show wS
    -- z <- log $ "we: " <> show wE
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

scheduleEventsStandAlone:: TimekNot -> Effect (Array {whenPosix:: Number, s:: String})
scheduleEventsStandAlone tk = do
    ast <- read tk.ast
    t <- read tk.tempo
    eval <- read tk.eval
    ws <- read tk.wS
    we <- read tk.wE
    pure $ fromProgramToArray ast t ws we eval

evaluate :: TimekNot -> String -> Effect { success :: Boolean, error :: String }
evaluate timekNot str = do
  log "timekNot-CU: evaluate"
  program <- read timekNot.ast
  eval <- nowDateTime
  let pr = pErrorToString $ runParser str parseTop -- :: Either String AST 
  case pr of
    Left error -> pure $ { success: false, error }
    Right p -> do
--      trace ("program: " <> show p) \_ -> p
      log $ show p
      write eval timekNot.eval --- esto!!!!
      write p timekNot.ast 
      pure $ { success: true, error: "" }

pErrorToString:: Either ParseError Program -> Either String Program
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
    program <- read tk.ast
    t <- read tk.tempo
    eval <- read tk.eval

    log $ show program
    log $ show ws
    log $ show we
    let events = fromProgramToArray program t ws we eval
    log $ show events
    pure $ map unsafeToForeign events

fromProgramToArray:: Program -> Tempo -> DateTime -> DateTime -> DateTime -> Array {whenPosix:: Number, s:: String}
fromProgramToArray p t ws we eval = toUnfoldable $ programToWaste t ws we eval p 

filterMaybe:: List (Maybe Waste) -> List Waste
filterMaybe x = map withoutMaybe $ filter justJust x

withoutMaybe:: Maybe Waste -> Waste
withoutMaybe (Just x) = x
withoutMaybe Nothing = {whenPosix: 0.0, s: ""}

justJust:: Maybe Waste -> Boolean
justJust (Just _) = true
justJust Nothing = false

testMaybeInstant:: Number -> Maybe Instant
testMaybeInstant x = instant $ Milliseconds x -- Maybe Instant
