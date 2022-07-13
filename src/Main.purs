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

import Parsing

import AST
import Rhythmic


-- type AST = Boolean

-- instance timekNotShowInstance :: Show TimekNot where
--   show x = show $ read x.ast



launch :: Effect TimekNot
launch = do
  log "timekNot-CU: launch"
  ast <- new $ Passage (Onsets $ L.fromFoldable []) (L.fromFoldable [])
  tempo <- newTempo (1 % 1) >>= new 
  eval <- nowDateTime >>= new
  pure { ast, tempo, eval}  

--parseProgram:: String -> Either ParseError (M.Map Int Coordenada)


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
scheduleNoteEvents :: TimekNot -> Number -> Number -> forall opts. Effect (Array Foreign)
scheduleNoteEvents tk ws we = timekNotToForeigns tk ws we


-- pure $ map unsafeToForeign events
--       where events = [{s: "cp", n: 0}]


-- make unsafe function and correct with david's advice later
numToDateTime:: Number -> DateTime 
numToDateTime x =
      let asMaybeInstant = instant $ Milliseconds x -- Maybe Instant
          asInstant = unsafeMaybeMilliseconds asMaybeInstant
      in toDateTime asInstant 

unsafeMaybeMilliseconds:: Maybe Instant -> Instant
unsafeMaybeMilliseconds (Just x) = x
unsafeMaybeMilliseconds Nothing = unsafeMaybeMilliseconds $ instant $ Milliseconds 0.0

timekNotToForeigns:: TimekNot -> Number -> Number -> forall opts. Effect (Array Foreign)
timekNotToForeigns tk ws we = do
    let ws' = numToDateTime (ws * 1000.0000)
    let we' = numToDateTime (we * 1000.0000)
    passage <- read tk.ast
    t <- read tk.tempo
    eval <- read tk.eval

    log $ show passage
    log $ show ws
    log $ show we
    let events = fromPassageToArray passage t ws' we' eval
    log $ show events
    pure $ map unsafeToForeign events

fromPassageToArray:: Passage -> Tempo -> DateTime -> DateTime -> DateTime -> Array {whenPosix:: Number, s:: String, n:: Int}
fromPassageToArray (Passage rhy aus) t ws we eval = 
    let events' = passageToEvents rhy (fromFoldable aus) t ws we eval -- List (Maybe Event)
--        coordsfromMapToArray = L.toUnfoldable $ M.values coords -- Array
--        events = map coordToEvent coordsfromMapToArray
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

-----------------------------test-------------------
-- makeDate :: Int -> Month -> Int -> Date
-- makeDate y m d = 
--     unsafePartial $ fromJust $ 
--        canonicalDate <$> toEnum y <@> m <*> toEnum d

-- makeTime :: Int -> Int -> Int -> Int -> Time
-- makeTime h min sec milisec = 
--     unsafePartial $ fromJust $ Time <$> toEnum h <*> toEnum min <*> toEnum sec <*> toEnum milisec

-- t:: Tempo
-- t = {freq: (2%1),time: (DateTime (makeDate 2022 June 3) (makeTime 19 11 25 100)), count: fromInt 0 }

-- ws:: Int -> Int -> DateTime
-- ws x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

-- we:: Int -> Int -> DateTime
-- we x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

-- ws':: Number
-- ws' = unwrap $ unInstant $ fromDateTime $ ws 0 0

-- we':: Number
-- we' = unwrap $ unInstant $ fromDateTime $ we 1 0

-- eval:: DateTime
-- eval = (DateTime (makeDate 2022 June 3) (makeTime 19 13 5 150))

-- -- this needs to be sorted as soon as possible!!!!!
-- test :: forall opts. Effect (Array {whenPosix:: Number, s:: String, n:: Int})
-- test = do
--   log "timekNot-CU: launch"
--   ast <- new $ Onsets $ fromFoldable [true,true,false,true]
--   tempo <- newTempo (4 % 1) >>= new 
--   eval <- nowDateTime >>= new
--   x <- timekNotToEvents' {ast,tempo,eval} ws' we'
--   log $ show x
--   pure x

-- timekNotToEvents':: TimekNot -> Number -> Number -> forall opts. Effect (Array {whenPosix:: Number, s:: String, n:: Int})
-- timekNotToEvents' tk ws we = do
--     rhy <- read tk.ast
--     t <- read tk.tempo
--     eval <- read tk.eval
--     let events = fromCoordenateToArray rhy t (numToDateTime ws) (numToDateTime we) eval
--     pure events


-- ws'' = 1655673873.207149

-- whenPosix = 1655673873452000.0