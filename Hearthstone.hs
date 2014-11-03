module Hearthstone where

import System.IO
import Data.Array

{-
Denis Zhadan
2014-11-03
-}
-- structure of file 
type File = [Card] 
type Card = (Name, Cost, CardType)
type Name = String
type Cost = Int

-- on kahte tüüpi kaarte: olendid ja loitsud
type Deck = [CardType]
data CardType = MinionCard [Effect] HealthPoint AttackPoint IsTaunt (Maybe MinionType)
              | SpellCard [Effect]

--instance Show CardType where
--  show MinionCard e h a t m = "MinionCard" ++ show h
--  show (SpellCard Effect) = "SpellCard" ++ show 5 

type HealthPoint = Int -- number of health points
type AttackPoint = Int -- number of attack points
type IsTaunt = Bool -- taunting the creature

-- olenditel võivad olla sellised tüübid
data MinionType = Beast | Murloc 
                deriving (Show, Eq, Ord, Enum)

data Effect = OnPlay     [EventEffect]  -- effekt kaardi käimisel
            | UntilDeath [EventEffect]  -- effekt mis kestab välja käimisest kuni olendi surmani
            | OnDamage   [EventEffect]  -- effekt mis tehakse olendi vigastamisel
            | OnDeath    [EventEffect]  -- effekt mis tehakse olendi tapmisel (elupunktid <= 0)


-- toime on kaardi võtmine või olendite mõjutamine --- vastavalt filtrile
data EventEffect = All [Filter] [CreatureEffect]   -- mõjutatake kõiki filtrile vastavaid olendeid
                 | Choose [Filter] [CreatureEffect]  -- mõjutatake üht kasutaja valitud filtrile vastavat olendeit
                 | Random [Filter] [CreatureEffect]  -- mõjutatake üht juhuslikku filtrile vastavat olendeit
                 | DrawCard  -- toime olendi omanik võtab kaardi

data CreatureEffect = Health Type HealthPoint  -- elupunktide muutmine
                    | Attack Type AttackPoint  -- ründepunktide muutmine
                    | Taunt IsTaunt         -- mõnituse muutmine
                    deriving (Read)
instance Show CreatureEffect where
  show (Health t p) = "Health: " ++ show t ++ " " ++ show p
  show (Attack t p) = "Attack: " ++ show t ++ " " ++ show p 
  show (Taunt t) = "Taunt: " ++ show t 

{-
instance Read CreatureEffect where
  readsPrec _ v = readHealth v ++ readAttack v ++ readTaunt v
    where 
      tryParse [] = []
      tryParse ((attempt, result):xs) =
            if (take (length attempt) value) == attempt
            then [(result, drop (length attempt) value)]
            else tryParse xs
-}
{-
instance (Read a) => Read (SomeType a) where
    readsPrec d r = readMix r ++ readType r
      where
        readMix = readParen True $ \r -> do
            (v1, r'') <- readsPrec d r
            (v2, r')  <- readsPrec d r''
            return (Mix v1 v2, r')

        readType r = do
            (v, r') <- readsPrec d r
            return (Type v, r')
-}
-- muutuse tüüp
data Type = Relative -- negatiivne relatiivne muutmine on vigastamine 
          | Absolute -- absoluutne negatiivne muutmine ei ole vigastamine
          deriving (Show, Eq, Ord, Enum, Read)

-- filtri list on konjunktsioon, välja arvatud "Any" puhul
data Filter = AnyCreature     -- olendid
            | AnyHero         -- kangelased
            | AnyFriendly     -- "omad" 
            | Type MinionType -- kindlat tüüpi olendid
            | Self            -- mõjutav olend ise
            | Not [Filter]    -- eitus
            | Any [Filter]    -- disjunktsioon: kui üks tingimus on taidetud
readDeck name 
  = readFile name 

readDeck2 name 
  = do
    content <- readFile name
    putStr content
    return content

readDeck3 name 
  = do
    content <- readFile name
    return content --(read content :: CardType) 

--main = do
--    content <- readFile "deck1.txt"
    --length content

    