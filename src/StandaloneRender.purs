module StandaloneRender where

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

import Data.Rational (Rational(..), (%), fromInt, toNumber)
import Data.DateTime
import Data.DateTime.Instant hiding (diff)
import Data.Time.Duration


-- render:: TimekNot -> {zone :: Int, windowStartTime :: Number, windowEndTime :: Number} -> forall opts. Effect (Array Foreign)
-- render tk args = do
--     let ws = numToDateTime (args.windowStartTime * 1000.0000) -- haskell comes in milliseconds, purescript needs seconds
--     let we = numToDateTime (args.windowEndTime * 1000.0000)
--     program <- read tk.ast
--     vantageMap <- read tk.vantageMap
--     -- log $ "vm: " <> show vantageMap
--     t <- read tk.tempo
--     eval <- read tk.eval
--     let tp = assambleTimePacket ws we eval t vantageMap
--     -- log $ show program
--     -- log $ show ws
--     -- log $ show we
--     -- log $ show t
--     programToForeign program tp


-- standalone model in combjelly's repo

type Engine = {
  wS :: Ref DateTime,
  wE :: Ref DateTime
  }

initialise:: Effect Engine
initialise = do
  wS <- nowDateTime >>= new
  wE <- nowDateTime >>= new
  pure {wS, wE}  

-- :: TimekNot -> Engine -> {zone :: Int, windowStartTime :: Number, windowEndTime :: Number} -> WebDirt -> Effect something... 
renderStandalone :: Engine -> Effect String
renderStandalone engine = do 
  t <- nowDateTime 
  prevW <- read $ engine.wE 
  let futureTime = fromMaybe t $ adjust (Milliseconds 400.00) t -- :: Milliseconds
  if prevW <= futureTime then do
    let wS = prevW
    let wE = fromMaybe t $ adjust (Milliseconds 500.0) wS 
    
    pure $ "wS: " <> show wS <> " wE: " <> show wE
  else
    pure $ show "sleepy time"


-- -- figure out wakeup time, for each wake up time figure out way/method on how to change store variables (map)
-- whatDoINeedToDoThisRenderFrame :: EngineRecord -> Program -> Tempo -> Number -> Number -> Effect (List SampleEvent)
-- whatDoINeedToDoThisRenderFrame er p t cycleStart cycleEnd = do
--   maybeInitializeGlobalVars er p
--   dealWithLoops er p t cycleStart cycleEnd 

-- dealWithLoops :: EngineRecord -> Program -> Tempo -> Number -> Number -> Effect (List SampleEvent)
-- dealWithLoops er p t wStart wEnd = do 
--     -- for each loop statement, figure out when they each activate. 
--         -- collect all loops. Program -> (List Loop)
--         varMap <- liftEffect $ read er.variables 
--         seqMap <- liftEffect $ read er.sequences 

--         let ls = collectLoops p -- ::  (List Loop)
--         let looptimes = concat $ map (getLoopTimes er wStart wEnd varMap seqMap) ls -- :: List (Tuple Loop Number)
--         let sortedList = sortBy (comparing snd) looptimes -- :: List (Tuple Number Loop)
--         events <- traverse (performLoop er) sortedList
--         --log $ show looptimes
--         pure $ concat events
--       -- calculate effect of all activations 

-- getLoopTimes :: EngineRecord -> Number -> Number -> Map.Map String Number  -> Map.Map String (List Number)  -> Loop -> List (Tuple Loop Number) 
-- getLoopTimes er wStart wEnd varMap seqMap (Loop n la)  = 
--   -- calculate cycle time of occurences between wstart and wend. 
--   -- for each number , produce a tuple 

--     let l = (Loop n la)
--         newN = resolveExpression n varMap seqMap-- converts loops expression to number 
--         safeN = limitLoopTime newN   
--         xs = cycleIntervalList wStart wEnd safeN -- List Number
--         in map (Tuple l) xs

-- limitLoopTime :: Number -> Number
-- limitLoopTime n = do
--   if n <= 0.001 then do 
--     0.001
--   else
--     n

-- cycleIntervalList :: Number -> Number -> Number -> List Number
-- cycleIntervalList wStart wEnd n = 
--   let pFirst = (toNumber $ ceil (wStart/n)) * n
--   --let pFirst =  (wStart/n) * n
--   in if pFirst < wEnd then
--     (pFirst : cycleIntervalList (pFirst+n) wEnd n )
--   else
--     Nil 

