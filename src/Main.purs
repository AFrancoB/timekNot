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

import AST
import Parser
import Calculations

import Parsing

-- test editor view
-- grid 2 1 [[label 1,code 2 0 []],[[label 5,code 3 0 []],[metre 2666]]]

-- command for copying into estuary devstaging: cp -Rf ~/Documents/repos/tk/timekNot /home/alejandro/Documents/repos/estuary/dev-staging/Estuary.jsexe

-- Expression = TimeExpression (Map String Temporal)
launch :: Effect TimekNot
launch = do
  log "timekNot: launch"
  ast <- new $ L.fromFoldable [TimeExpression  M.empty]
  tempo <- newTempo (1 % 1) >>= new 
  eval <- nowDateTime >>= new
  pure { ast, tempo, eval}  

evaluate :: TimekNot -> String -> Effect { success :: Boolean, error :: String }
evaluate tk str = do
  log "timekNot: evaluate"
  program <- read tk.ast
  eval <- nowDateTime
  let pr = check' $ runParser str parseProgram
  case pr of
    Left error -> pure $ { success: false, error }
    Right p -> do
      write eval tk.eval 
      write p tk.ast 
      pure $ { success: true, error: "bad syntax" }

check':: Either ParseError Program -> Either String Program
check' (Left error) = Left $ parseErrorMessage error
check' (Right aProgram) = case check (getTemporalMap aProgram) of
                              true -> Right aProgram
                              false -> Left "failed the check, time bites it's own tail"

setTempo :: TimekNot -> ForeignTempo -> Effect Unit
setTempo tk t = do
  -- log $ "setTempo is called" <> show (fromForeignTempo t)
  write (fromForeignTempo t) tk.tempo

-- setTempo :: RE.RenderEngine -> ForeignTempo -> Effect Unit
-- setTempo re t = do
--   rEnv <- read re.renderEnvironment
--   write (rEnv { tempo = fromForeignTempo t } ) re.renderEnvironment


scheduleNoteEvents :: TimekNot -> Number -> Number -> forall opts. Effect (Array Foreign)
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

timekNotToForeigns:: TimekNot -> Number -> Number -> forall opts. Effect (Array Foreign)
timekNotToForeigns tk ws we = do
    let ws' = numToDateTime (ws * 1000.0000) -- haskell comes in milliseconds, purescript needs seconds
    let we' = numToDateTime (we * 1000.0000)
    program <- read tk.ast
    t <- read tk.tempo
    eval <- read tk.eval


    -- log $ show program
    -- log $ show ws
    -- log $ show we
    -- log $ show t

    events <- programToWaste program ws' we' eval t
    -- log $ show events
    pure $ map unsafeToForeign events