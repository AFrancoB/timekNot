module PianoRollTesting (draw, getHeightForTag) where

import Prelude


import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either
import Data.Tuple
import Data.Traversable (traverse, traverse_)
import Graphics.Canvas (CanvasElement(..),Context2D(..), Arc(..), clearRect, rect, arc, lineTo, moveTo, beginPath, stroke, fillPath, setFillStyle, setStrokeStyle, setFont, setLineWidth, getContext2D,
                        getCanvasElementById, getCanvasWidth, getCanvasHeight, strokeText, fillText)
import Partial.Unsafe (unsafePartial)
import Data.Array ((..), length, zip, zipWith,concat, fromFoldable, sort, last, head, (!!), catMaybes)

import Foreign

import Data.TraversableWithIndex(traverseWithIndex)

import Data.Number (pi)
import Data.Int (toNumber, fromString)
import Data.String (split, splitAt, stripPrefix, Pattern(..))
import Data.String (length) as St
import Data.Map as M
import Data.Map (Map(..))
import Data.List.Lazy as Lz


import AST
import TimePacketOps (fromDateTimeToPosix)
-- import Control.Monad.Except (runExcept)


-- addJrSuffix :: forall r. { name :: String | r } -> { name :: String | r }
-- addJrSuffix hasName = hasName { name = hasName.name <> ", Jr." }


-- {when,  s, n}

-- foreignToNumber:: Foreign -> Either String Number
-- foreignToNumber f = 
--   let result = runExcept $ readNumber f
--       x = case result of 
--             Right num -> Right num
--             Left err -> Left $ show err
--   in x

-- main = unit
-- ANCHOR: main
-- main:: Number -> Number -> Event -> Effect Unit
-- main wS wE evs =
-- draw:: Map String Visual -> TimePacket -> Effect (Array Unit)


-- draw _ _ = pure unit

draw:: (Map String (Array Visual)) -> TimePacket -> Effect Unit
draw visualMap' tp = void $ unsafePartial do 
  Just canvas <- getCanvasElementById "pianola"
  ctx <- getContext2D canvas
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas

  let visualMap = voiceTagToVisualMap visualMap'

  _ <- clearRect ctx {height: height, width: width, x: 0.0, y: 0.0}

  let ws = (fromDateTimeToPosix tp.ws)
  let we = (fromDateTimeToPosix tp.we)

  let heightMap = getHeightForTag visualMap height

  log ("ws++ " <> show ws)
  log ("we-- " <> show we)

  -- let whens = map (\v -> v.when) v

  -- traverse_ log $ map show whens

    -- present:
  setLineWidth ctx 1.0
  setStrokeStyle ctx "white"
  beginPath ctx
  moveTo ctx ((width/2.0)*0.89) 0.0
  lineTo ctx ((width/2.0)*0.89) height
  stroke ctx

  fromFoldable <$> M.values <$> traverseWithIndex (draw' canvas ctx width height ws we heightMap) visualMap

draw':: CanvasElement -> Context2D -> Number -> Number -> Number -> Number -> Map VoiceTag Number -> VoiceTag -> Array Visual -> Effect (Array Unit)
draw' canvas ctx width height ws we heightMap tag v = drawEvents ctx tagHeight $ map (\v -> genCoordinates ws we v width height tagHeight voiceSize) $ sort v
  where 
  -- heightMap' = M.mapMaybeWithKey (\k v ->  map (\k' -> Tuple k' v) $ Just $ intoVoiceTag k) heightMap
        tagHeight = Tuple tag $ fromMaybe 0.0 $ M.lookup tag heightMap
        voiceSize = toNumber $ M.size heightMap


voiceTagToVisualMap:: (Map String (Array Visual)) -> (Map VoiceTag (Array Visual)) 
voiceTagToVisualMap m = M.fromFoldable $ zip (catMaybes $ map intoVoiceTag $ fromFoldable $ M.keys m) $ fromFoldable $ M.values m

getHeightForTag:: (Map VoiceTag (Array Visual)) -> Number -> Map VoiceTag Number
getHeightForTag visualMap height = M.fromFoldable $ zip (fromFoldable $ M.keys visualMap) $ fromFoldable x
  where sz = M.size visualMap 
        x = Lz.scanlLazy (+) 0.0 $ Lz.replicate sz (height / toNumber sz)

intoVoiceTag:: String -> Maybe VoiceTag
intoVoiceTag x = 
        let x' = split (Pattern "-") x
            caso = (((x'!!1)) == Nothing)
        in case caso of 
        true -> Just $ NonCanonic x
        false -> do 
                    indx' <- last x'
                    indx <- fromString indx'
                    let r = Canonic x indx
                    pure r

  -- pure $ unit

  -- log $ show $ map typeOf x

  -- let uF = map unsafeFromForeign x

  -- -- _ <- traverse (\x -> log $ show x) uF

  -- let evs = map (\e -> e.when :: Number) uF

  -- Just canvas <- getCanvasElementById "pianola"
  -- ctx <- getContext2D canvas
  -- width <- getCanvasWidth canvas
  -- height <- getCanvasHeight canvas
  -- drawEvents ctx $ map (\ev -> genCoordinates ws we ev width height) evs

  -- drawEvalTime ctx width height
  -- drawConvergence ctx width height $ mapRange (106.0 - ws) (Tuple 0.0 (we - ws)) $ Tuple 0.0 width 


drawEvents:: Context2D  -> Tuple VoiceTag Number -> Array (Maybe Coordinates) -> Effect (Array Unit)
drawEvents ctx tagHeight coords = traverse (\coord -> drawEvent ctx coord) coords

drawEvent:: Context2D -> Maybe Coordinates -> Effect Unit
drawEvent ctx (Just coord) = do 
  log $ "coordX " <> show coord.x
  beginPath ctx
  setLineWidth ctx 2.0
  setFont ctx "20px Arial"
  setFillStyle ctx "rgba(200,0,50,1)"
  setStrokeStyle ctx "rgba(200,0,50,1)"
  fillText ctx (onsetXO coord.onset) (coord.x) (coord.y)
  -- arc ctx $ circle coord.x (coord.y) coord.radius
  stroke ctx
  
drawEvent _ Nothing = do
  log "nothings"


onsetXO true = "X"
onsetXO false = "O"

genCoordinates:: Number -> Number -> Visual -> Number -> Number -> Tuple VoiceTag Number -> Number -> Maybe Coordinates
genCoordinates ws we v cWidth cHeight (Tuple tag height) voiceSize
  | (v.when >= ws) && (v.when < we) = Just {x: x, y: y, width: 30.0, height: 30.0, radius: 5.0, s: v.s, n: v.n, onset: v.onset}
      where x = mapRange (v.when - ws) (Tuple 0.0 (we - ws)) $ Tuple 0.0 cWidth
            y = mapRange height (Tuple (cHeight+10.0) (0.0-10.0)) $ Tuple 0.0 cHeight
  | otherwise = Nothing


drawEchoic ctx w h val (Tuple e1 e2) (Tuple y1 y2) = do
  setLineWidth ctx 2.0
  setFont ctx "10px Arial"
  setFillStyle ctx "green"
  setStrokeStyle ctx "green"
  let splitV = if val < 0.0 then 4 else 3
  let dist = (splitAt splitV $ show val)
  fillText ctx dist.before (1.01*(e1 + ((e2-e1)/2.0))) (y1 + ((y2-y1)/2.0))
  beginPath ctx
  moveTo ctx e1 y1
  lineTo ctx e2 y2
  stroke ctx


drawEvalTime ctx width height = do 
  setLineWidth ctx 1.0
  setFont ctx "15px Arial"
  setFillStyle ctx "red"
  setStrokeStyle ctx "red"
  fillText ctx "eval time" 35.0 120.0
  beginPath ctx
  moveTo ctx 100.0 0.0
  lineTo ctx 100.0 height
  stroke ctx

drawConvergence ctx width height valX = do 
  setLineWidth ctx 2.0
  setFont ctx "15px Arial"
  setFillStyle ctx "gray"
  setStrokeStyle ctx "gray"
  fillText ctx "convergence point" (valX+5.0) 120.0
  beginPath ctx
  moveTo ctx valX 0.0
  lineTo ctx valX height
  stroke ctx



circle:: Number -> Number -> Number -> Arc
circle x y r = {x: x, y: y, radius: r, start: 0.0, end: 2.0 * pi, useCounterClockwise: false}

mapRange:: Number -> Tuple Number Number -> Tuple Number Number -> Number 
mapRange value (Tuple inMin inMax) (Tuple outMin outMax) = outMin + (outMax - outMin) * ((value - inMin) / (inMax - inMin))

type Coordinates =  {x:: Number, y:: Number, width:: Number, height:: Number, radius:: Number, s:: String, onset :: Boolean, n:: Int}


