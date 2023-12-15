module Visualisation where

import Prelude

import Effect (Effect)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Data.Map as M
import Data.Int (toNumber)
import Data.Array ((:))
import Data.Array (fromFoldable) as A
import Data.List
import Data.Number.Format (toString)


import Svg.Parser

import AST


-- definitions:
---- layer is a voice in a sequence (looped). So, a layer defines an array of voices that are identical except their position in time
---- voice is a rhythmic pattern of onsets
---- onset a moment in time when a sound is instantiated

---- process-oriented index: an int identifier for each onset on a flow of onsets. 
---- structure-oriented index: an int identifier for each voice on a layer and an array to identifier internal events in a voice: The head is the 'natural' subdivisions of the voice, each new element in the array is a new subdivision

type Coordinate = {
  x1:: Number,
  x2:: Number,
  y:: Number,
  label:: String,
  onset:: Boolean
  }

-- drawProgram:: M.Map String Temporal -> Number -> Number -> Number -> HH.HTML p i
-- drawProgram mapa ws we eval = svgNodeToHtml $ svgFrame ws we h (calculateVoiceSVG $ mapToVoiceCoordinates mapa ws we eval) (calculateOnsetSVG $ mapToOnsetCoordinates mapa ws we eval) $ calculateLabelSVG $ mapToOnsetCoordinates mapa ws we eval
--   where h = toNumber $ length $ M.values mapa

svgFrame:: Number -> Number -> Number -> Array SvgNode -> Array SvgNode -> Array SvgNode -> SvgNode
svgFrame ws we h voices onsets labels = SvgElement { 
    name: "svg"
  , attributes: fromFoldable [ 
      SvgAttribute "xmlns" "http://www.w3.org/2000/svg"
    , SvgAttribute  "viewBox" (show ws <>" -0.25 " <> show (we - ws) <> " " <> show (h))
    , SvgAttribute "preserveAspectRatio" "none"
    , SvgAttribute "height" "500"
    , SvgAttribute "width" "1000"
    ]
  , children: concat $ fromFoldable [fromFoldable [defs],background ws we h, fromFoldable voices, fromFoldable onsets, fromFoldable labels]
      
  }

defs = SvgElement {
    name: "defs"
  , attributes: fromFoldable []
  , children: fromFoldable [grad]
}

grad = SvgElement {
    name: "linearGradient"
  , attributes: fromFoldable [
        SvgAttribute "id" "grad"
      , SvgAttribute "x1" "0%"
      , SvgAttribute "y1" "0%"
      , SvgAttribute "x2" "100%"
      , SvgAttribute "y2" "0%"
  ]
  , children: fromFoldable [stop1,stop2]
}

stop1 = SvgElement {
    name: "stop"
  , attributes: fromFoldable [
      SvgAttribute "offset" "3%"
    , SvgAttribute "style" "stop-color:yellow; stop-opacity:100%"
  ] 
  , children: fromFoldable []
}

stop2 = SvgElement {
    name: "stop"
  , attributes: fromFoldable [
      SvgAttribute "offset" "200%"
    , SvgAttribute "style" "stop-color:transparent; stop-opacity:100%"
  ] 
  , children: fromFoldable []
}

background ws we h = fromFoldable [SvgElement {
    name: "rect"
  , attributes: fromFoldable [
      SvgAttribute "x" $ show ws
    , SvgAttribute "y" "-0.25"
    , SvgAttribute "width" $ show $ we - ws
    , SvgAttribute "height" $ show $ (h + 1.0) + 0.5
    , SvgAttribute "opacity" "0.55"
    , SvgAttribute "fill" "black"
    ]
  , children: fromFoldable []
}]

calculateVoiceSVG:: Array Coordinate -> Array SvgNode
calculateVoiceSVG coords = map (\ c -> drawVoice c.x1 c.x2 c.y 0.5) coords

drawVoice:: Number -> Number -> Number -> Number -> SvgNode
drawVoice x1 x2 y wStroke = SvgElement {name: "rect"
  , attributes: fromFoldable [ 
      SvgAttribute "x" $ toString x1
    , SvgAttribute "width" $ toString $ x2 - x1
    , SvgAttribute "y" $ toString y
    , SvgAttribute "height" $ toString 0.1
    , SvgAttribute "fill" "url(#grad)"
    , SvgAttribute "opacity" "75%"]
  , children: fromFoldable []
}

calculateOnsetSVG:: Array Coordinate -> Array SvgNode
calculateOnsetSVG coords = map (\ x -> drawOnset x.x1 x.y 0.5) coords

drawOnset:: Number -> Number -> Number -> SvgNode
drawOnset x1 y wStroke = SvgElement {name: "circle"
  , attributes: fromFoldable [ 
      SvgAttribute "cx" $ toString x1
    , SvgAttribute "cy" $ toString (y + 0.05)
    , SvgAttribute "r" $ toString 0.065
    , SvgAttribute "fill" "red"]
  , children: fromFoldable []
}

calculateLabelSVG:: Array Coordinate -> Array SvgNode
calculateLabelSVG coords = map (\ c -> drawLabel c.x1 c.y c.label) coords

drawLabel:: Number -> Number -> String -> SvgNode
drawLabel x y label = SvgElement {name: "text"
  , attributes: fromFoldable [ 
      SvgAttribute "x" $ toString x
    , SvgAttribute "y" $ toString y
    , SvgAttribute "font-size" "0.8%"
    , SvgAttribute "font-weight" "bold"
    , SvgAttribute "font-variant" "numeric-fraction-values"
    ]
  , children: fromFoldable [SvgText label]
}

--
ns :: HH.Namespace
ns = HH.Namespace "http://www.w3.org/2000/svg"

svgAttributeToProp :: forall r i. SvgAttribute -> HP.IProp r i
svgAttributeToProp (SvgAttribute k v) = HP.attr (HH.AttrName k) v

svgElementToHtml :: forall p i. Element -> HH.HTML p i
svgElementToHtml ele = svgElementToHtmlWithAttrs ele []

svgElementToHtmlWithAttrs :: forall p r i. Element -> Array (HP.IProp r i) -> HH.HTML p i
svgElementToHtmlWithAttrs ele newAttrs =
  HH.elementNS ns (HH.ElemName ele.name) (attrs <> newAttrs) children
  where
    attrs = A.fromFoldable $ svgAttributeToProp <$> ele.attributes
    children = A.fromFoldable $ svgNodeToHtml <$> ele.children

svgNodeToHtml :: forall p i. SvgNode -> HH.HTML p i
svgNodeToHtml (SvgElement element) = svgElementToHtml element
svgNodeToHtml (SvgText str) = HH.text str
svgNodeToHtml (SvgComment _str) = HH.text ""
