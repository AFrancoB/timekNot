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

import Foreign 

import Partial.Unsafe
import Data.Enum

import Data.DateTime (DateTime(..))

import Data.Newtype

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Visualisation
-- import Svg.Parser

import AST
import TimePacketOps
import Parser
import Voices
import Novus

import Parsing

-- test editor view
-- grid 2 1 [[label 1,code 2 0 []],[[label 5,code 3 0 []],[metre 2666]]]

-- command for copying into estuary devstaging: cp -Rf ~/Documents/repos/tk/timekNot /home/alejandro/Documents/repos/estuary/dev-staging/Estuary.jsexe

-- do not forget branch name: exolang-pathway

launch :: Effect TimekNot
launch = do
  log "timekNot: launch"
  ast <- new $ L.fromFoldable [TimeExpression  M.empty]
  tempo <- newTempo (1 % 1) >>= new 
  eval <- nowDateTime >>= new
  vantageMap <- new $ (M.empty)
  pure { ast, tempo, eval, vantageMap}  

evaluate :: TimekNot -> String -> Effect { success :: Boolean, error :: String }
evaluate tk str = do
  log "timekNot: evaluate"
  -- program <- read tk.ast -- this does not do anything, can be erased...?
  currentVM <- read tk.vantageMap
  log $ "currentVM" <> show currentVM
  tempo <- read tk.tempo
  eval <- nowDateTime
  let pr = check' currentVM $ runParser str parseProgram
  case pr of
    Left error -> pure $ { success: false, error }
    Right p -> do
      write eval tk.eval 
      write p tk.ast 
      write (processVantage (getVantageMap p) currentVM eval tempo) $ tk.vantageMap
      pure $ { success: true, error: "bad syntax" }

check':: VantageMap -> Either ParseError Program -> Either String Program
check' vm (Left error) = Left $ parseErrorMessage error
check' vm (Right aProgram) = case check vm aProgram of
                              true -> Right aProgram
                              false -> Left "failed the check, time bites it's own tail"

scheduleNoteEvents:: TimekNot -> Number -> Number -> forall opts. Effect (Array Foreign)
scheduleNoteEvents tk ws' we' = do
    let ws = numToDateTime (ws' * 1000.0000) -- haskell comes in milliseconds, purescript needs seconds
    let we = numToDateTime (we' * 1000.0000)
    program <- read tk.ast
    vantageMap <- read tk.vantageMap
    -- log $ "vm: " <> show vantageMap
    t <- read tk.tempo
    eval <- read tk.eval
    let tp = assambleTimePacket ws we eval t vantageMap
    -- log $ show program
    -- log $ show ws
    -- log $ show we
    -- log $ show t
    programToForeign program tp
    
    -- programToForeign program tp
    
    
    -- events <- programToWaste program tp
    -- log $ show events
    -- pure $ map unsafeToForeign events

setTempo :: TimekNot -> ForeignTempo -> Effect Unit
setTempo tk t = do
  -- log $ "setTempo is called" <> show (fromForeignTempo t)
  write (fromForeignTempo t) tk.tempo