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

import Unleash

import Parsing


launch :: Effect Unleash
launch = do
  log "timekNot-CU: launch"
  ast <- new $ P (Tuple "" 0) $ Q 0.0 0.0
  tempo <- newTempo (1 % 1) >>= new 
  eval <- nowDateTime >>= new
  pure { ast, tempo, eval}  

evaluate :: Unleash -> String -> Effect { success :: Boolean, error :: String }
evaluate unleash str = do
  log "Unleash-Windsor: evaluate"
  program <- read unleash.ast
 -- log $ show passage
  -- placeholder: assume any evaluation yields the valid program
  eval <- nowDateTime
  let pr = pErrorToString $ runParser str parseSample -- :: Either String AST 
  case pr of
    Left error -> pure $ { success: false, error }
    Right p -> do
      write eval unleash.eval --- esto!!!!
      write p unleash.ast 
      pure $ { success: true, error: "" }    --- here you might add the eval time with purescript. 

pErrorToString:: Either ParseError Program -> Either String Program
pErrorToString (Left x) = Left $ parseErrorMessage x
pErrorToString (Right x) = Right x

setTempo :: Unleash -> ForeignTempo -> Effect Unit
setTempo unleash t = write (fromForeignTempo t) unleash.tempo

-- here a func that goes from rhythmicInto (passing through map) Event
scheduleNoteEvents :: Unleash -> Number -> Number -> forall opts. Effect (Array Foreign)
scheduleNoteEvents tk ws we = unleashToForeigns tk ws we

-- make unsafe function and correct with david's advice later
numToDateTime:: Number -> DateTime 
numToDateTime x =
      let asMaybeInstant = instant $ Milliseconds x -- Maybe Instant
          asInstant = unsafeMaybeMilliseconds asMaybeInstant
      in toDateTime asInstant 

unsafeMaybeMilliseconds:: Maybe Instant -> Instant
unsafeMaybeMilliseconds (Just x) = x
unsafeMaybeMilliseconds Nothing = unsafeMaybeMilliseconds $ instant $ Milliseconds 0.0

unleashToForeigns:: Unleash -> Number -> Number -> forall opts. Effect (Array Foreign)
unleashToForeigns un ws we = do
    let ws' = numToDateTime (ws * 1000.0000)
    let we' = numToDateTime (we * 1000.0000)
    program <- read un.ast
    t <- read un.tempo
    eval <- read un.eval

    log $ show program
    log $ show ws
    log $ show we

    let events = fromPassageToArray program t ws' we' eval
    log $ show events
    pure $ map unsafeToForeign events

fromPassageToArray:: Program -> Tempo -> DateTime -> DateTime -> DateTime -> Array {whenPosix:: Number, s:: String, n:: Int}
fromPassageToArray prog t ws we eval = [actualise prog t eval ws we]


