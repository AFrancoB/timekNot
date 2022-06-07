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
import Data.List
import Data.Array

import Foreign 

type AST = Boolean

type TimekNot = {
  ast :: Ref AST,
  tempo :: Ref Tempo
  }


launch :: Effect TimekNot
launch = do
  log "testLang: launch"
  ast <- new false
  tempo <- newTempo (1 % 2) >>= new
  pure { ast, tempo }  


evaluate :: TimekNot -> String -> DateTime -> Effect { success :: Boolean, error :: String }
evaluate timekNot _ _ = do
  log "testLang: evaluate"
  -- placeholder: assume any evaluation yields the valid program
  let pr = Right true -- :: Either String AST 
  case pr of
    Left error -> pure $ { success: false, error }
    Right p -> do
      write p timekNot.ast 
      pure $ { success: true, error: "" }    --- here you might add the eval time with purescript. 


setTempo :: TimekNot -> ForeignTempo -> Effect Unit
setTempo timekNot t = write (fromForeignTempo t) timekNot.tempo


scheduleNoteEvents :: TimekNot -> DateTime -> DateTime -> forall opts. Effect (Array Foreign)
scheduleNoteEvents _ _ _ = pure $ map unsafeToForeign events
  where events = [{ s: "cp", n: 0 }, { s: "bd", n: 1}]
