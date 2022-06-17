module Main where

import Prelude
import Data.Either
import Data.Maybe
import Effect
import Effect.Ref
import Effect.Class
import Effect.Console (log)
import Data.Tempo
import Data.DateTime
import Effect.Ref (new,write)

import Data.Rational
import Data.List.Lazy 
import Data.Array as A

import Foreign 

import Parsing

import Motor
import Rhythmic


-- type AST = Boolean

type TimekNot = {
  ast :: Ref Rhythmic,
  tempo :: Ref Tempo
  }


launch :: Effect TimekNot
launch = do
  log "testLang: launch"
  ast <- new $ Onsets $ fromFoldable [false]
  tempo <- newTempo (4 % 1) >>= new
  pure { ast, tempo }  

--parseProgram:: String -> Either ParseError (M.Map Int Coordenada)


--- here a func that turns string into RHythmic
--evaluate :: TimekNot -> String -> DateTime -> Effect { success :: Boolean, error :: String }
evaluate :: TimekNot -> String -> DateTime -> Effect { success :: Boolean, error :: String }
evaluate timekNot str eval = do
  log "testLang: evaluate"
  -- placeholder: assume any evaluation yields the valid program
  let pr = pErrorToString $ runParser str topRhythmic -- :: Either String AST 
  case pr of
    Left error -> pure $ { success: false, error }
    Right p -> do
      write p timekNot.ast 
      pure $ { success: true, error: "" }    --- here you might add the eval time with purescript. 

pErrorToString:: Either ParseError Rhythmic -> Either String Rhythmic
pErrorToString (Left x) = Left $ parseErrorMessage x
pErrorToString (Right x) = Right x

setTempo :: TimekNot -> ForeignTempo -> Effect Unit
setTempo timekNot t = write (fromForeignTempo t) timekNot.tempo

-- here a func that goes from rhythmicInto (passing through map) Event
scheduleNoteEvents :: TimekNot -> DateTime -> DateTime -> forall opts. Effect (Array Foreign)
scheduleNoteEvents tk ws we = pure $ map unsafeToForeign events
--    where events = fromCoordenateToEffect
      where events = [{ s: "cp", n: 0 }, { s: "bd", n: 1}]



-- fromCoordenateToEffect:: Rhythmic -> DateTime -> DateTime -> Array {whenPosix:: Number, s:: String, n:: Int}
-- fromCoordenateToEffect x t ws we eval = 
--     let coords = fromPassageToCoord $ fromRhythmicToList x t ws we eval
--         coordsfromMapToArray = ... -- transform the map into an array
--         events = map xxx coordsfromMapToArray
--       in events


coordToEvent:: Coordenada -> {whenPosix:: Number, s:: String, n:: Int}
coordToEvent (Coord num iEv iPas) = {whenPosix: num, s: "cp", n: 0 }
