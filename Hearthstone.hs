module Hearthstone where

--import System.IO
--import Data.Array
--import Data.Char
--import Data.Maybe
--import Control.Monad

{-
Denis Zhadan
2014-11-05
-}

-- structure of file 
type File = [Card] 
type Card = (Name, Cost, CardType)
type Name = String
type Cost = Int

type CardInGame = (Name, Cost, CardType, Color, HealthPoint, AttackPoint, IsTaunt) 

-- on kahte tüüpi kaarte: olendid ja loitsud
type Deck = [CardType]
data CardType = MinionCard [Effect] HealthPoint AttackPoint IsTaunt (Maybe MinionType)
              | SpellCard [Effect]
              deriving (Show, Eq, Ord, Read)

type HealthPoint = Int -- number of health points
type AttackPoint = Int -- number of attack points
type IsTaunt = Bool -- taunting the creature

-- olenditel võivad olla sellised tüübid
data MinionType = Beast | Murloc 
                deriving (Show, Eq, Ord, Enum, Read)

-- Heroes colors
data Color = Red | Blue 
                deriving (Show, Eq, Ord, Enum, Read)


data Effect = OnPlay     [EventEffect]  -- effekt kaardi käimisel
            | UntilDeath [EventEffect]  -- effekt mis kestab välja käimisest kuni olendi surmani
            | OnDamage   [EventEffect]  -- effekt mis tehakse olendi vigastamisel
            | OnDeath    [EventEffect]  -- effekt mis tehakse olendi tapmisel (elupunktid <= 0)
            deriving (Show, Eq, Ord, Read)

-- toime on kaardi võtmine või olendite mõjutamine --- vastavalt filtrile
data EventEffect = All [Filter] [CreatureEffect]   -- mõjutatake kõiki filtrile vastavaid olendeid
                 | Choose [Filter] [CreatureEffect]  -- mõjutatake üht kasutaja valitud filtrile vastavat olendeit
                 | Random [Filter] [CreatureEffect]  -- mõjutatake üht juhuslikku filtrile vastavat olendeit
                 | DrawCard  -- toime olendi omanik võtab kaardi
                 deriving (Show, Eq, Ord, Read)

data CreatureEffect = Health Type HealthPoint  -- elupunktide muutmine
                    | Attack Type AttackPoint  -- ründepunktide muutmine
                    | Taunt  IsTaunt           -- mõnituse muutmine
                    deriving (Show, Eq, Ord, Read)
{-
instance Show CreatureEffect where
  show (Health t p) = "Health: " ++ show t ++ " " ++ show p
  show (Attack t p) = "Attack: " ++ show t ++ " " ++ show p 
  show (Taunt t) = "Taunt: " ++ show t 
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
            deriving (Show, Eq, Ord, Read)

readDeck name 
  = do 
    x <- readFile name
    let y = read x :: File
    let z = length y
    --putStrLn ((show.head) y)
    --putStrLn ((show.length) y)
    --if z > 0 
    --then putStrLn (show (z))
    --else putStrLn "Null" 
    return y

cardToGame card @(a, b, c)
  = do
    let t = case c of      
              MinionCard _ _ _ isTaunt _ -> isTaunt
              --otherwise -> True
    putStrLn ((show) t)
    putStrLn ((show) c)
    let z = (a, b, c, Red::Color, 0 :: HealthPoint, 0 :: AttackPoint, t :: IsTaunt) :: CardInGame
    return z 

--takeCard :: [Card] -> Card
putCard deck
  = do
    let y = (head (deck :: File)) :: Card  -- take first card
    --putStrLn ((show) y)
    z @(a,b,c,d,e,f,g) <- cardToGame y
    putStrLn ((show) a)
    --putStrLn ((show) g)
--    putStrLn (show (y))
--    firstCard <- head (deck)
--    putStrLn (show (firstCard))
--    let card = cardToGame firstCard
--    putStrLn (show (card))
    return z

game 
  = do
    deck1 <- readDeck "deck1.txt"
    fcard <- putCard deck1
    return fcard
 
--readFromFile
--  = do
    --content <- toDeck text
    --let content = text
    --putStr text
    --return content
    --    return text 
    --y <- (read (content :: Char) :: [CardType])
    --a <- (readFile "deck0.txt") :: IO [Char]
    --let a = "[MinionCard [] 6 7 False Nothing,MinionCard [OnPlay [Choose [] [Health Relative (-1)]]] 4 5 False Nothing,MinionCard [OnPlay [DrawCard]] 2 4 False Nothing]" 
    --b <- read "[MinionCard [] 6 7 False Nothing,MinionCard [OnPlay [Choose [] [Health Relative (-1)]]] 4 5 False Nothing,MinionCard [OnPlay [DrawCard]] 2 4 False Nothing]" :: [CardType]
    --a <- readFile "deck0.txt" 
    --b <- read c :: [CardType]
    --return b


--main 
--d  = do
    --content <- readFile "deck1.txt"
    --length content

    