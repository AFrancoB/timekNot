module AssambleWebdirt (objectWithWhenSN, addGain, addPan, addSpeed, addBegin, addEnd, addVowel, addMaxW, addMinW, addInter, addLegato, addOrbit, addCutOff, addCutOffH, addNote, addVal) where

import Prelude
import Effect (Effect)
import Foreign

-- foreign
foreign import objectWithWhenSN :: Number -> String -> Int -> Effect Foreign
-- export objectWithWhenSN = when => s => n => () => { return { when: when, s: s, n: n }; }

foreign import addGain :: Foreign -> Number -> Effect Foreign
-- export addGain = o => gain => () => { o.gain = gain; return o; }

foreign import addPan :: Foreign -> Number -> Effect Foreign
-- export addPan = o => pan => () => { o.pan = pan; return o; }

foreign import addSpeed :: Foreign -> Number -> Effect Foreign
-- export addSpeed = o => speed => () => { o.speed = speed; return o; }

foreign import addBegin :: Foreign -> Number -> Effect Foreign
-- export addBegin = o => begin => () => { o.begin = begin; return o; }

foreign import addEnd :: Foreign -> Number -> Effect Foreign
-- export addEnd = o => end => () => { o.end = end; return o; }

foreign import addVowel :: Foreign -> String -> Effect Foreign
-- export addVowel = o => vowel => () => { o.vowel = vowel; return o; }

foreign import addCutOff :: Foreign -> Number -> Effect Foreign
-- export addCutoff = o => cutoff => () => { o.cutoff = cutoff; return o; }

foreign import addCutOffH :: Foreign -> Number -> Effect Foreign
-- export addCutoffH = o => cutoffh => () => { o.hcutoff = cutoffh; return o; }

foreign import addMaxW :: Foreign -> Number -> Effect Foreign
-- export addCutoffH = o => cutoffh => () => { o.hcutoff = cutoffh; return o; }

foreign import addMinW :: Foreign -> Number -> Effect Foreign
-- export addCutoffH = o => cutoffh => () => { o.hcutoff = cutoffh; return o; }

foreign import addInter :: Foreign -> Number -> Effect Foreign
-- export addCutoffH = o => cutoffh => () => { o.hcutoff = cutoffh; return o; }

foreign import addLegato :: Foreign -> Number -> Effect Foreign
-- export addCutoffH = o => cutoffh => () => { o.hcutoff = cutoffh; return o; }

foreign import addOrbit :: Foreign -> Int -> Effect Foreign
-- export addCutoffH = o => cutoffh => () => { o.hcutoff = cutoffh; return o; }

foreign import addNote :: Foreign -> Number -> Effect Foreign
-- export const addNote = o => note => () => { o.note = note; return o; }

foreign import addVal :: Foreign -> String -> Number -> Effect Foreign


-- foreign import addVal :: String -> Number -> Foreign -> Effect Foreign
--export const addVal =  valId  =>  val   => o => () => { o[valId] = val; return o; };