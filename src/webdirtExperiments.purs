module WebDirtExperiments where

import Prelude
import Effect (Effect)
import WebDirt

launch :: Effect WebDirt
launch = do
  wd <- newWebDirt { sampleMapUrl: "./src/samples/sampleMap.json", sampleFolder: "./src/samples" }
  initializeWebAudio wd
  pure wd

makeSound :: WebDirt -> Effect Unit
makeSound wd = playSample wd { s: "808Kicks", n: "0"}