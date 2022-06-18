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

import Data.Rational
import Data.List.Lazy 
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

import Parsing

import Motor
import Rhythmic


-- type AST = Boolean

type TimekNot = {
  ast :: Ref Rhythmic,
  tempo :: Ref Tempo,
  eval :: Ref DateTime
  }



launch :: Effect TimekNot
launch = do
  log "timekNot-CU: launch"
  ast <- new $ Onsets $ fromFoldable [false]
  tempo <- newTempo (4 % 1) >>= new 
  eval <- nowDateTime >>= new
--  eval <- new $ (origin tempo)
  pure { ast, tempo, eval}  

--parseProgram:: String -> Either ParseError (M.Map Int Coordenada)


evaluate :: TimekNot -> String -> Effect { success :: Boolean, error :: String }
evaluate timekNot str = do
  log "timekNot-CU: evaluate"
  -- placeholder: assume any evaluation yields the valid program
  eval <- nowDateTime
  let pr = pErrorToString $ runParser str topRhythmic -- :: Either String AST 
  case pr of
    Left error -> pure $ { success: false, error }
    Right p -> do
      write eval timekNot.eval --- esto!!!!
      write p timekNot.ast 
      pure $ { success: true, error: "" }    --- here you might add the eval time with purescript. 

pErrorToString:: Either ParseError Rhythmic -> Either String Rhythmic
pErrorToString (Left x) = Left $ parseErrorMessage x
pErrorToString (Right x) = Right x

setTempo :: TimekNot -> ForeignTempo -> Effect Unit
setTempo timekNot t = write (fromForeignTempo t) timekNot.tempo

-- here a func that goes from rhythmicInto (passing through map) Event
scheduleNoteEvents :: TimekNot -> Number -> Number -> forall opts. Effect (Array Foreign)
scheduleNoteEvents tk ws we =  do
    x <- timekNotToEvents tk (numToDateTime ws) (numToDateTime we)
    log $ show x
    pure x

-- make unsafe function and correct with david's advice later
numToDateTime:: Number -> DateTime 
numToDateTime x =
      let asMaybeInstant = instant $ Milliseconds x -- Maybe Instant
          asInstant = unsafeMaybeMilliseconds asMaybeInstant
      in toDateTime asInstant 

unsafeMaybeMilliseconds:: Maybe Instant -> Instant
unsafeMaybeMilliseconds (Just x) = x
unsafeMaybeMilliseconds Nothing = unsafeMaybeMilliseconds $ instant $ Milliseconds 0.0

timekNotToEvents:: TimekNot -> DateTime -> DateTime -> forall opts. Effect (Array Foreign)
timekNotToEvents tk ws we = do
    rhy <- read tk.ast
    t <- read tk.tempo
    eval <- read tk.eval
    let events = fromCoordenateToArray rhy t ws we eval
    pure $ map unsafeToForeign events

fromCoordenateToArray:: Rhythmic -> Tempo -> DateTime -> DateTime -> DateTime -> Array {whenPosix:: Number, s:: String, n:: Int}
fromCoordenateToArray x t ws we eval = 
    let coords = fromPassageToCoord x t ws we eval
        coordsfromMapToArray = L.toUnfoldable $ M.values coords -- Array
        events = map coordToEvent coordsfromMapToArray
      in events


coordToEvent:: Coordenada -> {whenPosix:: Number, s:: String, n:: Int}
coordToEvent (Coord num iEv iPas) = {whenPosix: num, s: "cp", n: 0 }

testMaybeInstant:: Number -> Maybe Instant
testMaybeInstant x = instant $ Milliseconds x -- Maybe Instant

-- toDateTime :: Instant -> DateTime

-- instant :: Milliseconds -> Maybe Instant





-----------------------------test-------------------
makeDate :: Int -> Month -> Int -> Date
makeDate y m d = 
    unsafePartial $ fromJust $ 
       canonicalDate <$> toEnum y <@> m <*> toEnum d

makeTime :: Int -> Int -> Int -> Int -> Time
makeTime h min sec milisec = 
    unsafePartial $ fromJust $ Time <$> toEnum h <*> toEnum min <*> toEnum sec <*> toEnum milisec

t:: Tempo
t = {freq: (2%1),time: (DateTime (makeDate 2022 June 3) (makeTime 19 11 25 100)), count: fromInt 0 }

ws:: Int -> Int -> DateTime
ws x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

we:: Int -> Int -> DateTime
we x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

eval:: DateTime
eval = (DateTime (makeDate 2022 June 3) (makeTime 19 13 5 150))