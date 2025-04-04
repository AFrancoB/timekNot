module Erv where

import AST (CPSNote)

foreign import ratioToCents :: Number -> Number

foreign import makeCPSScale :: Int -> Array Int -> Array CPSNote

-- foreign import makeMOSScale :: Int -> Int -> Array MOSNote




-- foreign import pruebilla :: Int -> Int -> Int


--- objeto resultante de cps.make , typear..?
-- {
--   meta: {
--     scale: 'cps',
--     period: 2,
--     size: 2,
--     factors: [ 1, 3, 5, 7 ],
--     'normalized-by': 1,
--     type: '2)4'
--   },
--   scale: [
--     {
--       set: [Array],
--       'archi-set': [Array],
--       ratio: 35,
--       'bounded-ratio': 1.09375,
--       'bounding-period': 2
--     },
--     {
--       set: [Array],
--       'archi-set': [Array],
--       ratio: 5,
--       'bounded-ratio': 1.25,
--       'bounding-period': 2
--     },
--     {
--       set: [Array],
--       'archi-set': [Array],
--       ratio: 21,
--       'bounded-ratio': 1.3125,
--       'bounding-period': 2
--     },
--     {
--       set: [Array],
--       'archi-set': [Array],
--       ratio: 3,
--       'bounded-ratio': 1.5,
--       'bounding-period': 2
--     },
--     {
--       set: [Array],
--       'archi-set': [Array],
--       ratio: 7,
--       'bounded-ratio': 1.75,
--       'bounding-period': 2
--     },
--     {
--       set: [Array],
--       'archi-set': [Array],
--       ratio: 15,
--       'bounded-ratio': 1.875,
--       'bounding-period': 2
--     }
--   ],
--   nodes: [
--     {
--       set: [Array],
--       'archi-set': [Array],
--       ratio: 3,
--       'bounded-ratio': 1.5,
--       'bounding-period': 2
--     },
--     {
--       set: [Array],
--       'archi-set': [Array],
--       ratio: 5,
--       'bounded-ratio': 1.25,
--       'bounding-period': 2
--     },
--     {
--       set: [Array],
--       'archi-set': [Array],
--       ratio: 7,
--       'bounded-ratio': 1.75,
--       'bounding-period': 2
--     },
--     {
--       set: [Array],
--       'archi-set': [Array],
--       ratio: 15,
--       'bounded-ratio': 1.875,
--       'bounding-period': 2
--     },
--     {
--       set: [Array],
--       'archi-set': [Array],
--       ratio: 21,
--       'bounded-ratio': 1.3125,
--       'bounding-period': 2
--     },
--     {
--       set: [Array],
--       'archi-set': [Array],
--       ratio: 35,
--       'bounded-ratio': 1.09375,
--       'bounding-period': 2
--     }
--   ],
--   graphs: {
--     full: {
--       '{:set #{1 3}, :archi-set #{:a :b}, :ratio 3, :bounded-ratio 1.5, :bounding-period 2}': [Array],
--       '{:set #{1 5}, :archi-set #{:a :c}, :ratio 5, :bounded-ratio 1.25, :bounding-period 2}': [Array],
--       '{:set #{1 7}, :archi-set #{:a :d}, :ratio 7, :bounded-ratio 1.75, :bounding-period 2}': [Array],
--       '{:set #{3 5}, :archi-set #{:b :c}, :ratio 15, :bounded-ratio 1.875, :bounding-period 2}': [Array],
--       '{:set #{3 7}, :archi-set #{:b :d}, :ratio 21, :bounded-ratio 1.3125, :bounding-period 2}': [Array],
--       '{:set #{5 7}, :archi-set #{:c :d}, :ratio 35, :bounded-ratio 1.09375, :bounding-period 2}': [Array]
--     },
--     simple: {
--       '#{1 3}': [Array],
--       '#{1 5}': [Array],
--       '#{1 7}': [Array],
--       '#{3 5}': [Array],
--       '#{3 7}': [Array],
--       '#{5 7}': [Array]
--     }
--   }
-- }
