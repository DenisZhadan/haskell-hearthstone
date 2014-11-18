module Hearthstone where

import System.IO
import Data.Char
--import Data.Array
--import Data.Maybe
--import Control.Monad

{-
Denis Zhadan
2014-11-05
-}

-- heroes colors
data Color = Red | Blue deriving (Show, Eq, Ord, Enum, Read)
-- get another color - need for easy code
next :: Color -> Color
next Red = Blue
next Blue = Red

-- hero
type Hero = (Color, HealthPoint)
--data Hero = Color HealthPoint
--          deriving (Show, Eq, Ord, Read)
type Heroes = [Hero]

type Crystals = Int -- hero power 1..10
type Turn = Int -- quantity of turns which the player made

type Creature = (Name, Color, CreatureStatus, CardType) 
type Creatures = [Creature] 

type CreatureStatus = (HealthPoint, AttackPoint, IsTaunt)

type CardInHand = Card
type CardInDeck = Card
type Player = ([CardInHand], [CardInDeck], Crystals, Turn)

-- structure of file 
type File = [Card] 
type Card = (Name, Cost, CardType)
type Name = String
type Cost = Int

-- on kahte tüüpi kaarte: olendid ja loitsud
data CardType = MinionCard [Effect] HealthPoint AttackPoint IsTaunt (Maybe MinionType)
              | SpellCard [Effect]
              deriving (Show, Eq, Ord, Read)

type HealthPoint = Int -- number of health points
type AttackPoint = Int -- number of attack points
type IsTaunt = Bool -- taunting the creature

-- olenditel võivad olla sellised tüübid
data MinionType = Beast | Murloc 
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

-- show line for separate data
showLine
  = putStrLn "-------------------------------------------------------------------------------"

-- show small line for separate data
showSmallLine
  = putStrLn "---------------------"

-- read cards from file to deck
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

-- init data for start game
game 
  = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    -- read decks for players
    deck1 <- readDeck "deck1.txt" 
    deck2 <- readDeck "deck1.txt"

    -- player Red take 3 cards
    let cardsInHand1 = take 3 deck1
    let newDeck1 = drop 3 deck1

    -- player Blue take 4 cards, because second
    let cardsInHand2 = take 4 deck2
    let newDeck2 = drop 4 deck2

    newTurn (Red :: Color)
             ([(Red, 30), (Blue, 30)] :: Heroes)
             ([] :: Creatures) 
             (cardsInHand1, newDeck1, 0 :: Crystals, 0 :: Turn) 
             (cardsInHand2, newDeck2, 0 :: Crystals, 0 :: Turn) 
    return True

initTurn player @(cardsInHand, cardsInDeck, crystals, turn)
  = do
    let newCardsInHand = cardsInHand ++ (take 1 cardsInDeck)
    let newCardsInDeck = drop 1 cardsInDeck
    let x = (newCardsInHand, newCardsInDeck, (if turn < 10 then turn + 1 else 10), turn + 1) :: Player
    return x

-- start new turn for player
newTurn color heroes creatures player1 player2
  = do     
    player <- initTurn (if (color == Red) then player1 else player2)
    --print player
    nowTurn color heroes creatures 
            (if (color == Red) then player else player1)
            (if (color /= Red) then player else player2)

nowTurn color heroes creatures player1 player2
  = do
    result <- showTable color heroes creatures (if (color == Red) then player1 else player2)
    
    case result of
      0 -> do
           newTurn (next color) heroes creatures player1 player2
      1 -> do
           putCardToTable color heroes creatures player1 player2            
      otherwise -> do 
                   nowTurn color heroes creatures player1 player2
    return 0

getHeroByColor color heroes
  = do 
    let x@(a,b) = head  (filter (\ (heroColor, _) -> heroColor == color) heroes)
    --putStrLn (show a ++ show b)
    return x

getCardsByCost cards cost
  = do 
    let x = filter (\ (_, cardCost, _) -> cardCost <= cost) cards
    return x 

isMyCreature creature color
  = do
    let x = filter (\ (_, creatureColor, _, _) -> creatureColor == color) creature
    return x 

-- show data for player
showTable color heroes creatures player@(cardsInHand, deck, crystals, turn)
  = do
    showLine
    putStrLn ("Turn: " ++ show turn ++ " You have crystals: " ++ show crystals) 
    -- show my hero health 
    myHero @(_, hp1) <- getHeroByColor color heroes
    putStrLn ("My hero("++show color ++") HP is: " ++ show hp1)

    -- show enemy hero health     
    enemyHero @(_, hp2) <- getHeroByColor (next color) heroes
    putStrLn ("Enemy hero HP is: "  ++ show hp1)
    
    showSmallLine
    -- show my creatures
    myCreatures <- isMyCreature creatures color
    putStrLn ("My creatures is: " ++  ((show.length) myCreatures)) 
    mapM_ print myCreatures

    -- show enemy creatures
    enemyCreatures <- isMyCreature creatures (next color)
    putStrLn ("Enemy creatures is: " ++  ((show.length) enemyCreatures)) 
    mapM_ print enemyCreatures    

    showSmallLine
    -- show my cards in hand
    putStrLn ("My cards in hand is: " ++ ((show.length) cardsInHand))
    mapM_ print cardsInHand

    -- init allowed actions
    allowedCards <- ((getCardsByCost cardsInHand crystals)) --  check cost of cards <= crystals
    let 
      allowedActions = do 
        [0] ++ 
          if (length allowedCards > 0)  
             then [1]
             else [] 
          ++
          if (length myCreatures > 0)  -- ! need after set filter on creatures by has attack
             then [2]
             else []

    -- ask from player choice
    result <- showMainActions allowedActions
    return result

showMainActions allowedActions
  = do 
    showLine 
    putStrLn "Please make your choice:"
    if (elem 1 allowedActions) 
       then putStrLn "1 - Play card"
       else putStr ""
    if (elem 2 allowedActions) 
       then putStrLn "2 - Attack"
       else putStr ""
    putStrLn "0 - End turn"  
    result <- readAction allowedActions
    return result

readAction allowedActions
  = do 
    let 
      getAction = do
        c <- getChar
        let d = ord (c) - 48
        if (elem d allowedActions)
        then do
          putStrLn ""
          return d
        else getAction
    hSetEcho stdout False
    action <- getAction
    hSetEcho stdout True
    return action

showCardsInHand (x@(name, cost, ctype) : xs) i crystals
  = do
    putStrLn ((if (cost <= crystals) then (show i) else "X") ++ " - " ++ (show x))
    a <- (showCardsInHand xs (i+1) crystals)
    let b = a ++ if (cost <= crystals) then [i] else []
    return b
    
showCardsInHand _ _ _ = return []

putCardToTable color heroes creatures player1 player2
  = do
    let player@(cardsInHand, deck, crystals, turn) = (if (color == Red) then player1 else player2)
    -- ask card number from user
    putStrLn "Please select card number:"
    allowedActions <- showCardsInHand cardsInHand 1 crystals
    --print allowedActions
    result <- readAction allowedActions 

    let card = cardsInHand !! (result - 1)
    let newCardsInHand = drop 1 cardsInHand
    let x @(a, cost, c) = card
    z <- cardToGame x color
    let newCreatures = creatures ++ [z :: Creature]
    let newPlayer = (newCardsInHand, deck, crystals - cost, turn) 
    nowTurn color heroes newCreatures 
            (if (color == Red) then newPlayer else player1)
            (if (color /= Red) then newPlayer else player2)    
            
cardToGame card @(a, b, c) color
  = do
    let t = case c of      
              MinionCard _ _ _ isTaunt _ -> isTaunt
              --otherwise -> True
    putStrLn ((show) t)
    putStrLn ((show) c)
    let z = (a, color :: Color, (0 :: HealthPoint, 0 :: AttackPoint, t :: IsTaunt), c :: CardType) :: Creature
    return z 
{-

--takeCard :: [Card] -> Card
putCard deck
  = do
    let y = (head (deck :: File)) :: Card  -- take first card
    --putStrLn ((show) y)
    z @(a,c,d,e,f,g) <- cardToGame y
    putStrLn ((show) a)
    --putStrLn ((show) g)
--    putStrLn (show (y))
--    firstCard <- head (deck)
--    putStrLn (show (firstCard))
--    let card = cardToGame firstCard
--    putStrLn (show (card))
    return z
-}

 
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

    