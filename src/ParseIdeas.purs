module ParseIdeas where

import Prelude

import Data.Either
import Data.Identity
import Data.List hiding (many)
import Data.Foldable (foldl)
import Data.Int
import Data.Tuple
import Data.String (singleton, joinWith, take)
import Data.Maybe hiding (optional)
import Data.Functor
import Control.Monad

import Effect (Effect)
import Effect.Console (log)

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)



type P = ParserT String Identity

data Sub = Sub MyRhythms Number

instance Show Sub where
  show (Sub x d) = show x <> " dur " <> show d

data MyRhythms = 
  XO Boolean Number |
  RhythmList (List MyRhythms) 

instance Show MyRhythms where
  show (XO bool dur) = (if bool then "(x" else "(o") <> " d " <> (take 6 $ show dur) <> ")"
  show (RhythmList xs) = show xs

-- data MyRhythms = 
--   XO Boolean Number |
--   Sd MyRhythms Number |
--   RhythmList (List MyRhythms) 

-- instance Show MyRhythms where
--   show (XO bool dur) = (if bool then "(x" else "(o") <> " d " <> (take 6 $ show dur) <> ")"
--   show (Sd xs dur) = "sub " <> show xs <> " dur "<> show dur
--   show (RhythmList xs) = show xs

-- parseTop:: P MyRhythms
-- parseTop = do
--   _ <- pure 1
--   x <- parseGroup
--   eof
--   pure x

-- parseGroup:: P MyRhythms
-- parseGroup = do
--   _ <- pure 1
--   x <- chainl parse1 (whitespace $> manyXO) $ (RhythmList (Nil))
--   pure x

-- parse1:: P MyRhythms
-- parse1 = do
--   _ <- pure 1
--   choice [parseBottom']

-- parseBottom':: P MyRhythms
-- parseBottom' = do
--   _ <- pure 1
--   x <- choice [char 't' *> pure (XO true 1.0), char 'f' *> pure (XO false 1.0)]
--   pure x


-- manyXO:: MyRhythms -> MyRhythms -> MyRhythms
-- manyXO (XO a b) (XO a' b') = RhythmList $ (XO a b: XO a' b': Nil)
-- manyXO (XO a b) (RhythmList xs) = RhythmList $ (XO a b) : xs
-- manyXO (RhythmList xs) (XO a b) = RhythmList $ reverse ((XO a b) : (reverse xs))

-- -- manyXO (XO) (RhythmList) =
-- -- manyXO (RhythmList) (XO) = 
-- -- manyXO (RhythmList) (RhythmList) =

-- manyXO _ _ = RhythmList (Nil)

----
parseSD:: P Sub
parseSD = do
  _ <- pure 1
  _ <- char '['
  x <- chainl parse2 (whitespace $> manyRhythms) $ (Sub (RhythmList (Nil)) 0.0)
  _ <- char ']'
  eof
  pure $ x

parse2:: P Sub
parse2 = do
  _ <- pure 1
  x <- choice [parseBottom, parseSD]
  pure $ x

parseBottom:: P Sub
parseBottom = do
  _ <- pure 1
  x <- chainl (choice [char 'x' *> pure (XO true 1.0), char 'o' *> pure (XO false 1.0)]) (whitespace $> manyXOf) $ (XO false 2.666)
  pure $ Sub x 1.0

manyXOf:: MyRhythms -> MyRhythms -> MyRhythms
manyXOf (XO bool n) (XO bool' n') = RhythmList (XO bool n : XO bool' n' : Nil) 
manyXOf (XO bool n) (RhythmList xs) = RhythmList ((XO bool n) : xs) 
manyXOf (RhythmList xs) (XO bool n) = RhythmList $ attachLast (XO bool n) xs
manyXOf (RhythmList xs) (RhythmList xs') = RhythmList $ concat $ fromFoldable [xs,xs']
manyXOf _ _ = RhythmList (Nil)

manyRhythms:: Sub -> Sub -> Sub 
manyRhythms _ _ = Sub (RhythmList (Nil)) 266.6

---
-- parseSD:: P MyRhythms
-- parseSD = do  
--   _ <- pure 1
--   _ <- char '['
--   x <- chainl parse2 (whitespace $> manySUBXO) $ (Sd (Nil) 0.0)
--   _ <- char ']'
--   pure $ Sd (duraciones (f x) $ fromFoldable [x]) 1.0

-- f:: MyRhythms -> Int
-- f (RhythmList xs) = (length xs)
-- f (Sd xs d) = (length xs)
-- f (XO (Tuple bool n)) = (floor n) + 1 
-- f _ = 16

-- parse2:: P MyRhythms
-- parse2 = do
--   _ <- pure 1
--   choice [parseBottom, parseSD]

-- parseBottom:: P MyRhythms
-- parseBottom = do
--   _ <- pure 1
--   x <- choice [char 'x' *> pure (Tuple true 1.0), char 'o' *> pure (Tuple false 1.0)]
--   whitespace
--   pure $ XO x

-- manySUBXO:: MyRhythms -> MyRhythms -> MyRhythms
-- manySUBXO (Sd xs d) (Sd xs' d') = Sd (concat $ fromFoldable $ [duraciones' (d) xs, duraciones' (d') xs']) d
-- manySUBXO (Sd xs d) (XO (Tuple bool n)) = Sd (attachLast (XO (Tuple bool n)) $ duraciones' d xs) (d*10.0)

-- manySUBXO _ _ = RhythmList (Nil)


-- duraciones':: Number -> List MyRhythms -> List MyRhythms
-- duraciones' d xs = map (\x -> dur' d x) xs

-- dur':: Number -> MyRhythms -> MyRhythms
-- dur' d (XO (Tuple bool n)) = XO $ Tuple bool (d)
-- dur' d (RhythmList xs) = RhythmList $ duraciones' (d) xs
-- dur' d (Sd xs d') = Sd (duraciones' (d') xs) (d')


-----


-- parseSD:: P MyRhythms
-- parseSD = do  
--   _ <- pure 1
--   _ <- char '['
--   x <- chainl parse2 (whitespace $> manySUBXO) $ (Sd (Nil) 0.0)
--   _ <- char ']'
--   pure $ Sd (duraciones (f x) $ fromFoldable [x]) 1.0

-- f:: MyRhythms -> Int
-- f (RhythmList xs) = length xs
-- f (Sd xs d) = length xs
-- f _ = 1

-- parse2:: P MyRhythms
-- parse2 = do
--   _ <- pure 1
--   choice [parseBottom, parseSD]

-- parseBottom:: P MyRhythms
-- parseBottom = do
--   _ <- pure 1
--   x <- choice [char 'x' *> pure (Tuple true 1.0), char 'o' *> pure (Tuple false 1.0)]
--   whitespace
--   pure $ XO x

-- manySUBXO:: MyRhythms -> MyRhythms -> MyRhythms
-- -- manySUBXO (Sd xs d) (Sd xs' d') = RhythmList $ concat $ fromFoldable [duraciones' d xs,duraciones' d' xs']
-- manySUBXO (Sd xs d) (XO (Tuple bool n)) = RhythmList $ attachLast (XO (Tuple bool n)) $ duraciones' d xs 
-- manySUBXO (XO (Tuple bool n)) (Sd xs d) = RhythmList $ (XO (Tuple bool n)) : duraciones' d xs 
-- -- manySUBXO (XO (Tuple bool n)) (XO (Tuple bool' n')) = RhythmList $ duraciones' n $ (XO (Tuple bool (n)): XO (Tuple bool' (n')): Nil)

-- manySUBXO _ _ = RhythmList (Nil)


-- duraciones':: Number -> List MyRhythms -> List MyRhythms
-- duraciones' d xs = map (\x -> dur' d x) xs

-- dur':: Number -> MyRhythms -> MyRhythms
-- dur' d (XO (Tuple bool n)) = XO $ Tuple bool d
-- dur' d (RhythmList xs) = RhythmList $ duraciones' d xs
-- dur' d (Sd xs d') = Sd (duraciones' d' xs) d'


---




-- manySUBXO:: MyRhythms -> MyRhythms -> MyRhythms
-- manySUBXO (XO (Tuple bool n)) (XO (Tuple bool' n')) = RhythmList $ duraciones 2 $ (XO (Tuple bool (n)): XO (Tuple bool' (n')): Nil)
-- manySUBXO (XO (Tuple bool n)) (RhythmList xs) = RhythmList $ duraciones (1 + length xs) $ (XO (Tuple bool n)) : xs
-- manySUBXO (RhythmList xs) (XO (Tuple bool n)) = RhythmList $ duraciones (1 + length xs) $ attachLast (XO (Tuple bool n)) xs
-- manySUBXO (RhythmList xs) (RhythmList xs') = RhythmList $ duraciones (length xs + length xs') $ concat $ fromFoldable [xs,xs']
-- manySUBXO (Sd xs d) (XO (Tuple bool n)) = RhythmList $ ((XO (Tuple bool n)) : (Sd (duraciones' n xs) d) : Nil)

-- manySUBXO (Sd xs d) (XO (Tuple bool n)) = RhythmList $ attachLast (XO (Tuple bool n)) $ duraciones' n xs
-- manySUBXO (Sd xs d) (Sd xs' d') = RhythmList $ concat $ fromFoldable [duraciones' d xs, duraciones' d' xs']
--  duracion's length needs the +1


-- manySUBXO (SD) (XO)
-- manySUBXO (SD) (SD)
-- manySUBXO _ _ = RhythmList (Nil)

attachLast::forall a. a -> List a -> List a 
attachLast a xs = reverse (a :reverse xs)

-- duraciones:: Int -> List MyRhythms -> List MyRhythms
-- duraciones len' xs = map (\x -> dur len x) xs
--   where len = toNumber len'

-- dur:: Number -> MyRhythms -> MyRhythms
-- dur len (XO (Tuple bool n)) = XO $ Tuple bool $ 1.0/len
-- dur len (RhythmList xs) = RhythmList $ duraciones (length xs) xs
-- dur _ _ = RhythmList (Nil)
-- dur len (Sd xs d) = Sd (duraciones' d xs) d

-- duraciones':: Number -> List MyRhythms -> List MyRhythms
-- duraciones' d xs = map (\x -> dur' d x) xs

-- dur':: Number -> MyRhythms -> MyRhythms
-- dur' d (XO (Tuple bool n)) = XO $ Tuple bool d
-- dur' d (RhythmList xs) = RhythmList $ duraciones' d xs
-- dur' d (Sd xs d') = Sd (duraciones' d' xs) d'


-- parseTop':: P MyRhythms
-- parseTop' = do
--   _ <- pure 1
--   x <- parseGroup'
--   eof
--   pure x

-- parseGroup':: P MyRhythms
-- parseGroup' = do
--   _ <- pure 1
--   x <- chainl parse1 (whitespace $> manyXO) $ (RhythmList (Nil))
--   pure x

-- parse1:: P MyRhythms
-- parse1 = do
--   _ <- pure 1
--   choice [parseBottom, parseSD]

-- parseGroup:: P MyRhythms
-- parseGroup = do
--   _ <- pure 1
--   _ <- char '['
--   whitespace
--   x <- chainl (choice [parseBottom, parseGroup]) (whitespace $> manyXO) $ (RhythmList (Nil))
--   _ <- char ']'
--   whitespace
--   pure x -- add duraciones aqui

-- parseBottom:: P MyRhythms
-- parseBottom = do
--   _ <- pure 1
--   x <- choice [char 'x' *> pure (Tuple true 1.0), char 'o' *> pure (Tuple false 1.0)]
--   whitespace
--   pure $ XO x


-- manyXO:: MyRhythms -> MyRhythms -> MyRhythms
-- manyXO (XO (Tuple bool n)) (XO (Tuple bool' n')) = RhythmList ((XO (Tuple bool n)) : (XO (Tuple bool' n')) :Nil)
-- manyXO (XO (Tuple bool n)) (RhythmList xs) = RhythmList ((XO (Tuple bool n)):xs)
-- manyXO (RhythmList xs) (XO (Tuple bool n)) = RhythmList (reverse $ ((XO (Tuple bool n)) : reverse xs))
-- manyXO (RhythmList xs) (RhythmList xs') = RhythmList (duraciones (length xxss) xxss)
--   where xxss = concat (xs:xs':Nil) -- List MyRhythms

-- manyXO _ _ = RhythmList (Nil)


-- duraciones:: Int -> List MyRhythms -> List MyRhythms
-- duraciones len' xs = map (\x -> dur len x) xs
--   where len = toNumber len'

-- dur:: Number -> MyRhythms -> MyRhythms
-- dur len (XO (Tuple bool n)) = XO $ Tuple bool $ n/len
-- dur len (RhythmList xs) = RhythmList $ duraciones (length xs) xs

-- listRhythm:: List MyRhythms -> MyRhythms
-- listRhythm xs = foldl manyXO (fromMaybe (RhythmList (Nil)) $ head xs) $ fromMaybe (RhythmList (Nil) : Nil) $ tail xs

---------------





-- parseGroup:: P MyRhythms
-- parseGroup = do
--   _ <- pure 1
--   _ <- char '['
--   xs <- many $ choice [char 'x' *> pure (Tuple true 1.0), char 'o' *> pure (Tuple false 1.0)]
--   _ <- char ']'
--   pure $ XOs $ duraciones xs


-- duraciones:: List (Tuple Boolean Number) -> List (Tuple Boolean Number)
-- duraciones xs = map (\x -> Tuple (fst x) $ (snd x * (1.0/len))) xs
--   where len = toNumber $ length xs

-- parseBottom:: P MyRhythms
-- parseBottom = do
--   _ <- pure 1
--   x <- choice [char 'x' *> pure (Tuple true 1.0), char 'o' *> pure (Tuple false 1.0)]
--   pure $ XO x

-- manyXO:: MyRhythms -> MyRhythms -> MyRhythms
-- manyXO (XO (Tuple bool1 n)) (XO (Tuple bool2 n')) = XOs ((Tuple bool1 n):(Tuple bool2 n'):Nil)
-- manyXO (XO (Tuple bool n)) (XOs xs) = XOs ((Tuple bool n):xs)
-- manyXO (XOs xs) (XO (Tuple bool n)) = XOs $ reverse ((Tuple bool n) : (reverse xs))
-- manyXO (XOs xs) (XOs xs') = XOs $ concat $ fromFoldable [xs,xs']
-- manyXO (XO (Tuple bool n)) (RhythmList xs) = manyXO (XO (Tuple bool n)) $ listRhythm xs
-- manyXO (RhythmList xs) (XO (Tuple bool n)) = manyXO (listRhythm xs) $ XO (Tuple bool n) 
-- manyXO _ _ = RhythmList (Nil)

-- listRhythm:: List MyRhythms -> MyRhythms
-- listRhythm xs = foldl manyXO (fromMaybe (XOs Nil) $ head xs) $ fromMaybe ((XOs Nil:Nil)) $ tail xs


tokenParser = makeTokenParser haskellStyle
parens      = tokenParser.parens
braces      = tokenParser.braces
identifier  = tokenParser.identifier
reserved    = tokenParser.reserved
naturalOrFloat = tokenParser.naturalOrFloat
float = tokenParser.float
whitespace = tokenParser.whiteSpace
colon = tokenParser.colon
brackets = tokenParser.brackets
comma = tokenParser.comma
semi = tokenParser.semi
integer = tokenParser.integer
stringLit = tokenParser.stringLiteral