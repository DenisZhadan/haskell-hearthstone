module Hearthstone where

import System.IO
import Data.Char
import System.Random (getStdRandom, randomR)
--import Data.Array
--import Data.Maybe
--import Control.Monad

{-
Denis Zhadan
2014-11-30
-}

-- creature colors
data Color = Red | Blue deriving (Show, Eq, Ord, Enum, Read)
-- get another color - need for easy code
next :: Color -> Color
next Red = Blue
next Blue = Red

type Crystals = Int -- hero power 1..10
type Turn = Int -- quantity of turns which the player made

type Creature = (CreatureId, Name, Color, CreatureStatus, CardType) 
type CreatureId = Int
type Creatures = [Creature] 
type CanAttack = Bool
type IsHero = Bool
type CreatureStatus = (CanAttack, HealthPoint, AttackPoint, IsTaunt, IsHero)

type CardInHand = Card
type CardInDeck = Card
type Player = ([CardInHand], [CardInDeck], Crystals, Turn)

-- structure of file 
type File = [Card] 
type Card = (Name, Cost, CardType)
type Name = String
type Cost = Int

-- on kahte t√º√ºpi kaarte: olendid ja loitsud
data CardType = MinionCard [Effect] HealthPoint AttackPoint IsTaunt (Maybe MinionType)
              | SpellCard [Effect]
              deriving (Show, Eq, Ord, Read)

type HealthPoint = Int -- number of health points
type AttackPoint = Int -- number of attack points
type IsTaunt = Bool -- taunting the creature

-- olenditel v√µivad olla sellised t√º√ºbid
data MinionType = Beast | Murloc 
                deriving (Show, Eq, Ord, Enum, Read)

data Effect = OnPlay     [EventEffect]  -- effekt kaardi k√§imisel
            | UntilDeath [EventEffect]  -- effekt mis kestab v√§lja k√§imisest kuni olendi surmani
            | OnDamage   [EventEffect]  -- effekt mis tehakse olendi vigastamisel
            | OnDeath    [EventEffect]  -- effekt mis tehakse olendi tapmisel (elupunktid <= 0)
            deriving (Show, Eq, Ord, Read)

-- toime on kaardi v√µtmine v√µi olendite m√µjutamine --- vastavalt filtrile
data EventEffect = All [Filter] [CreatureEffect]   -- m√µjutatake k√µiki filtrile vastavaid olendeid
                 | Choose [Filter] [CreatureEffect]  -- m√µjutatake √ºht kasutaja valitud filtrile vastavat olendeit
                 | Random [Filter] [CreatureEffect]  -- m√µjutatake √ºht juhuslikku filtrile vastavat olendeit
                 | DrawCard  -- toime olendi omanik v√µtab kaardi
                 deriving (Show, Eq, Ord, Read)

data CreatureEffect = Health Type HealthPoint  -- elupunktide muutmine
                    | Attack Type AttackPoint  -- r√ºndepunktide muutmine
                    | Taunt  IsTaunt           -- m√µnituse muutmine
                    deriving (Show, Eq, Ord, Read)
{-
instance Show CreatureEffect where
  show (Health t p) = "Health: " ++ show t ++ " " ++ show p
  show (Attack t p) = "Attack: " ++ show t ++ " " ++ show p 
  show (Taunt t) = "Taunt: " ++ show t 
-}

-- muutuse t√º√ºp
data Type = Relative -- negatiivne relatiivne muutmine on vigastamine 
          | Absolute -- absoluutne negatiivne muutmine ei ole vigastamine
          deriving (Show, Eq, Ord, Enum, Read)

-- filtri list on konjunktsioon, v√§lja arvatud "Any" puhul
data Filter = AnyCreature     -- olendid
            | AnyHero         -- kangelased
            | AnyFriendly     -- "omad" 
            | Type MinionType -- kindlat t√º√ºpi olendid
            | Self            -- m√µjutav olend ise
            | Not [Filter]    -- eitus
            | Any [Filter]    -- disjunktsioon: kui √ºks tingimus on taidetud
            deriving (Show, Eq, Ord, Read)

type CreatureFromId = Int
type CreatureToId = Int           
type OnDeath = (CreatureFromId, CreatureToId, [CreatureEffect])
type OnDeaths = [OnDeath]

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
   
    let takeCardsForPlayer1 = 3
    let takeCardsForPlayer2 = 4

    -- player Red take 3 cards
    let cardsInHand1 = take takeCardsForPlayer1 deck1
    let newDeck1 = drop takeCardsForPlayer1 deck1

    -- player Blue take 4 cards, because second
    let cardsInHand2 = take takeCardsForPlayer2 deck2
    let newDeck2 = drop takeCardsForPlayer2 deck2
    let creatureMaxId = 2

    newTurn (Red :: Color) 
            ([(0 :: CreatureId, "Red Hero", Red, (False, 30 - 10 * (takeCardsForPlayer1 - length cardsInHand1), 0, False, True), MinionCard [] 30 0 False Nothing),
              (1 :: CreatureId, "Blue Hero", next Red, (False, 30 - 10 * (takeCardsForPlayer2 - length cardsInHand2), 0, False, True), MinionCard [] 30 0 False Nothing)] :: Creatures) 
            (cardsInHand1, newDeck1, 0 :: Crystals, 0 :: Turn)
            (cardsInHand2, newDeck2, 0 :: Crystals, 0 :: Turn)
            creatureMaxId ([] :: OnDeaths)
    return True

setCreaturesCanAttack [] _ = []
setCreaturesCanAttack (x@(creatureId, name, creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : xs) color
  | (creatureColor == color) = (creatureId, name, creatureColor, (True :: CanAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : setCreaturesCanAttack xs color
  | otherwise = x : setCreaturesCanAttack xs color

disableCreatureAttackById [] _ = []
disableCreatureAttackById (x@(creatureId, name, creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : xs) id
  | id == creatureId = (creatureId, name, creatureColor, (False :: CanAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : disableCreatureAttackById xs id
  | otherwise = x : disableCreatureAttackById xs id

changeCreatureHealthById [] _ _ = []
changeCreatureHealthById (x@(creatureId, name, creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : xs) changeHealth id
  | id == creatureId = (creatureId, name, creatureColor, (canAttack, (healthPoint + changeHealth), attackPoint, isTaunt, isHero), ctype) : changeCreatureHealthById xs changeHealth id
  | otherwise = x : changeCreatureHealthById xs changeHealth id

removeCreatureDead [] = []
removeCreatureDead (x@(creatureId, name, creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : xs)
  | healthPoint <= 0 = removeCreatureDead xs
  | otherwise = x : removeCreatureDead xs  

changeHeroHealthByColor [] _ _ = []
changeHeroHealthByColor (x@(creatureId, name, creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : xs) changeHealth color
  | (creatureColor == color) && (isHero == True) = (creatureId, name, creatureColor, (canAttack, (healthPoint + changeHealth), attackPoint, isTaunt, isHero), ctype) : changeHeroHealthByColor xs changeHealth color
  | otherwise = x : changeHeroHealthByColor xs changeHealth color

eventName2Int a
  = case a of
     OnPlay _ -> 1
     UntilDeath _ -> 2
     OnDamage _ -> 3
     OnDeath _ -> 4
     otherwise -> 0
  
effectsByEventNameId [] _ = []
effectsByEventNameId (x : xs) allowed
  | (elem (eventName2Int x) allowed) = x : effectsByEventNameId xs allowed
  | otherwise = effectsByEventNameId xs allowed

takeCardFromDeck color creatures player@(cardsInHand, cardsInDeck, crystals, turn)
  = do
    if (length cardsInDeck == 0)
    then do
      let newCreatures = changeHeroHealthByColor creatures (-10) color
      putStrLn ("Attention: -10 points from " ++ (show color) ++ " Hero! (No cards in deck)")
      return (player, newCreatures)
    else do
      let newCardsInHand = cardsInHand ++ (take 1 cardsInDeck) 
      let newCardsInDeck = drop 1 cardsInDeck 
      let newPlayer = (newCardsInHand, newCardsInDeck, crystals, turn) :: Player
      return (newPlayer, creatures)

initTurn color creatures player@(cardsInHand, cardsInDeck, crystals, turn)
  = do
    x <- takeCardFromDeck color creatures (cardsInHand, cardsInDeck, if turn < 10 then turn + 1 else 10, turn + 1)
    return x

-- start new turn for player
newTurn color creatures player1 player2 creatureMaxId onDeaths
  = do     
    (player, newCreatures1) <- initTurn color creatures (if (color == Red) then player1 else player2)
    let newCreatures2 = setCreaturesCanAttack newCreatures1 color --set "CanAttack" = True for creatures of this player
    --print player
    nowTurn color newCreatures2 
            (if (color == Red) then player else player1)
            (if (color /= Red) then player else player2) 
            creatureMaxId onDeaths

isGameEnd creatures
  = do
    hero1 @(_, _, _, (_, hp1, _, _, _),_) <- getHeroByColor Red creatures
    hero2 @(_, _, _, (_, hp2, _, _, _),_) <- getHeroByColor (next Red) creatures
    if ((hp1 > 0) && (hp2 > 0)) 
    then do 
      return False
    else do
      if ((hp1 <= 0) && (hp2 <= 0)) 
      then putStrLn "Dead heat"
      else do
        if ((hp2 <= 0) && (hp1 > 0)) then putStrLn "Red is won!"
        else putStrLn "Blue is won!"
      return True

nowTurn color creatures0 player1 player2 creatureMaxId onDeaths
  = do 
    b <- isGameEnd creatures0
    if (b == True) 
    then do
      return True
    else do
      let creatures = removeCreatureDead creatures0
      result <- showTable color creatures (if (color == Red) then player1 else player2)
    
      case result of
        0 -> do
             newTurn (next color) creatures player1 player2 creatureMaxId onDeaths
        1 -> do
             putCardToTable color creatures player1 player2 creatureMaxId onDeaths
        2 -> do
             attackWithCreature color creatures player1 player2 creatureMaxId onDeaths
        otherwise -> do 
                     nowTurn color creatures player1 player2 creatureMaxId onDeaths
      return True

getHeroByColor color creatures
  = do 
    let x = head  (filter (\ (_, _, heroColor, (_, _, _, _, isHero),_) -> (heroColor == color) && (isHero == True)) creatures)
    --putStrLn (show a ++ show b)
    return x

getCardsByCost cards cost
  = do 
    let x = filter (\ (_, cardCost, _) -> cardCost <= cost) cards
    return x 

getCreaturesByColor creatures color
  = do
    let x = filter (\ (_, _, creatureColor, _, _) -> creatureColor == color) creatures
    return x 

getCreatureById creatures id
  = do
    let x = head(filter (\ (creatureId, _, _, _, _) -> creatureId == id) creatures)  
    return x 

isCreatureCanAttack creature
  = do
    let x = filter (\ (_, _, _, (canAttack, _, attackPoint, _, _), _) -> (canAttack == True) && (attackPoint > 0)) creature
    return x 

isCreatureTaunt creature
  = do
    let x = filter (\ (_, _, _, (_, _, _, isTaunt, _), _) -> isTaunt == True) creature
    return x 
    
-- show data for player
showTable color creatures player@(cardsInHand, deck, crystals, turn)
  = do
    showLine
    putStrLn ("Turn: " ++ show turn ++ " You have crystals: " ++ show crystals) 
    -- show my hero health 

    myHero @(_, _, _, (_, hp1, _, _, _),_) <- getHeroByColor color creatures
    putStrLn ("My hero("++show color ++") HP is: " ++ show hp1)

    -- show enemy hero health     
    enemyHero @(_, _, _, (_, hp2, _, _, _),_) <- getHeroByColor (next color) creatures
    putStrLn ("Enemy hero HP is: "  ++ show hp2)
    
    showSmallLine
    -- show my creatures
    myCreatures <- getCreaturesByColor creatures color
    putStrLn ("My creatures is: " ++  ((show.length) myCreatures)) 
    mapM_ print myCreatures

    -- show enemy creatures
    enemyCreatures <- getCreaturesByColor creatures (next color)
    putStrLn ("Enemy creatures is: " ++  ((show.length) enemyCreatures)) 
    mapM_ print enemyCreatures    

    showSmallLine
    -- show my cards in hand
    putStrLn ("My cards in hand is: " ++ ((show.length) cardsInHand))
    mapM_ print cardsInHand

    -- init allowed actions
    cardsCanUse <- ((getCardsByCost cardsInHand crystals)) --  check cost of cards <= crystals
    creaturesCanAttack <- (isCreatureCanAttack myCreatures) -- filter on creatures CanAttack
    let 
      allowedActions = do 
        [0] ++ (if (length cardsCanUse > 0) then [1] else []) 
        ++ (if ((length creaturesCanAttack) > 0) then [2] else [])

    -- ask from player choice
    --print allowedActions
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

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

strToIntDef x d
  = if (isInteger x) 
    then read x :: Int
    else d
    
readAction allowedActions
  = do 
    let 
      getAction 
        = do
          s <- getLine
          let d = strToIntDef s (-1)
          -- putStrLn (show d)
          if (elem d allowedActions)
             then do
               putStrLn ""
               return d
          else do
            putStrLn ("Wrong input: '"++ s ++"'! Allowed only: " ++ (show allowedActions))
            y <- getAction  
            return y

    hSetEcho stdout False
    action <- getAction
    hSetEcho stdout True
    return action

showCardsInHand (x@(name, cost, ctype) : xs) i crystals
  | cost <= crystals = do
    putStrLn ( (show i) ++ " - " ++ (show x))
    a <- (showCardsInHand xs (i+1) crystals)
    let b = [i] ++ a
    return b
  | otherwise = do
    putStrLn ( "X" ++ " - " ++ (show x))
    a <- (showCardsInHand xs (i+1) crystals)
    return a
{-  = do
    putStrLn ((if (cost <= crystals) then (show i) else "X") ++ " - " ++ (show x))
    a <- (showCardsInHand xs (i+1) crystals)
    let b = a ++ if (cost <= crystals) then [i] else []
    return b
-}
        
showCardsInHand _ _ _ = return []

removeCardById [] _ = []
removeCardById (y:ys) i
  | i == 0 = removeCardById ys (i - 1)
  | otherwise = y : removeCardById ys (i - 1)

putCardToTable color creatures player1 player2 creatureMaxId onDeaths
  = do
    showLine
    let player@(cardsInHand, deck, crystals, turn) = (if (color == Red) then player1 else player2)
    -- ask card number from user
    putStrLn "Please select card number:"
    allowedActions <- showCardsInHand cardsInHand 1 crystals
    --print allowedActions
    result <- readAction allowedActions 

    let card = cardsInHand !! (result - 1)
    let newCardsInHand = removeCardById cardsInHand (result - 1)

    let x @(a, cost, c) = card
    (z, m) <- cardToGame x color creatureMaxId onDeaths
    let newCreatures = creatures ++ z :: Creatures
    let newCreatureMaxId = if length z > 0 then creatureMaxId + 1 else creatureMaxId
    let newPlayer = (newCardsInHand, deck, crystals - cost, turn) 

    --print (getOnPlayEventEffects m)
    magicEffect (getOnPlayEventEffects m) color newCreatures 
            (if (color == Red) then newPlayer else player1)
            (if (color /= Red) then newPlayer else player2) 
            newCreatureMaxId onDeaths
{-
getCreaturesByFilter [] f = []
getCreaturesByFilter (c:cs) f
  | (filterApplies f c) == True = c : getCreaturesByFilter cs f
  | otherwise  let x =  creatures
    return x
-}
getCreaturesByFilter creatures f
  = do 
    let r = filter (\x -> filterApplies f x) creatures
    return r 

filterApplies :: [Filter] -> Creature -> Bool
filterApplies [] c
 = True

filterApplies (f : fs) c@(creatureId, name, creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype)
 | f == AnyCreature = not isHero && filterApplies fs c
 | f == AnyHero = isHero && filterApplies fs c
 | f == AnyFriendly = creatureColor == Blue && filterApplies fs c -- ! Blue for test, after set by param
-- | "Type" MinionType -- ·„È•·‚¢† ÆØ‡•§•´•≠≠Æ£Æ ‚®Ø†
-- | "Self" -- ¢´®Ô•¨Æ• ·„È•·‚¢Æ
-- | f == Not = not filterApplies fs c
-- | f == Any =  || filterApplies fs c
 | otherwise = False
 
magicEffect [] color creatures player1 player2 creatureMaxId onDeaths
  = do nowTurn color creatures player1 player2 creatureMaxId onDeaths

magicEffect (m@(x : []) : ms) color creatures player1 player2 creatureMaxId onDeaths
  = do
    --showLine
    --print m
    --print ms
    --print x
    --print xs
    --showLine
    case x of
      All x y -> do
                 --filterApplies :: Creature -> Filter -> Creature -> Bool
                 magicEffect ms color creatures player1 player2 creatureMaxId onDeaths
      Choose x y ->do
                    --chooseCreature :: [Creature] -> IO (Creature, [Creature])
                    magicEffect ms color creatures player1 player2 creatureMaxId onDeaths
      Random x y ->do
                   --randomCreature :: [Creature] -> IO (Creature, [Creature])
                   qt <- getCreaturesByFilter creatures x
                   print (qt) 
                   --n <- random 0 (length(creatures) -1)
                   --print (creatures !! n)
                   magicEffect ms color creatures player1 player2 creatureMaxId onDeaths

      DrawCard -> do
                  (player, newCreatures) <- takeCardFromDeck color creatures (if (color == Red) then player1 else player2)
                  magicEffect ms color newCreatures 
                       (if (color == Red) then player else player1)
                       (if (color /= Red) then player else player2)
                       creatureMaxId onDeaths

    --putStrLn "=== > < ==="
    --magicEffect ms color creatures player1 player2 creatureMaxId onDeaths
    
magicEffect (m@(x : xs) : ms) color creatures player1 player2 creatureMaxId onDeaths
  = do
    --showLine
    --putStrLn "m@(x : xs) : ms" 
    --print x
    --print xs
    magicEffect ([x] : xs : ms) color creatures player1 player2 creatureMaxId onDeaths


getOnPlayEventEffects [] = []
getOnPlayEventEffects (a : as)
  = case a of
      OnPlay b -> b : getOnPlayEventEffects as
      _        -> getOnPlayEventEffects as

{-
isSpellCard a
  = case a of
     SpellCard _ -> True
     _ -> False     
-}
cardToGame card @(a, b, c @(SpellCard effects)) color creatureMaxId onDeaths
  = do
    let z = [] :: Creatures
    let m = effectsByEventNameId effects [1]
    return (z, m)
{-
  | isSpellCard c = do
--  | otherwise = do
    --otherwise -> True
-}

cardToGame card @(a, b, c @(MinionCard effects hp ap isTaunt _)) color creatureMaxId onDeaths
  = do
    --putStrLn ((show) isTaunt)
    --putStrLn ((show) c)
    let z = [(creatureMaxId :: CreatureId, a, color :: Color, (False :: CanAttack, hp :: HealthPoint, ap :: AttackPoint, isTaunt :: IsTaunt, False :: IsHero), c :: CardType)] :: Creatures
    let m = effectsByEventNameId effects [1]
    return (z, m) 

showCreaturesAsAttacker (c@(creatureId, name, creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : cs)
  | (canAttack == True) && (attackPoint > 0) = do
    putStrLn ( (show creatureId) ++ " - " ++ (show c))
    a <- showCreaturesAsAttacker cs
    let b = a ++ [creatureId]
    return b
  | otherwise = do
    putStrLn ( "X" ++ " - " ++ (show c))
    a <- showCreaturesAsAttacker cs
    return a
       
showCreaturesAsAttacker _ = return []

showTargetAsDefender (c@(creatureId, name, creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : cs) onlyTaunt
  | ((isTaunt == False) && (onlyTaunt)) = do
    putStrLn ( "X" ++ " - " ++ (show c))
    a <- showTargetAsDefender cs onlyTaunt
    return a
  | otherwise = do
    putStrLn ( (show creatureId) ++ " - " ++ (show c))
    a <- showTargetAsDefender cs onlyTaunt
    let b = a ++ [creatureId]
    return b
       
showTargetAsDefender _ _ = return []

attackWithCreature color creatures player1 player2 creatureMaxId onDeaths
  = do
    showLine
    myCreatures <- getCreaturesByColor creatures color
    enemyCreatures <- getCreaturesByColor creatures (next color)

    -- ask creature number from user
    putStrLn "Please select creature(attacker) number:"
    actions1 <- showCreaturesAsAttacker myCreatures
    --print actions1
    attckingId <- readAction actions1

    enemyCreaturesTaunt <- (isCreatureTaunt enemyCreatures) -- filter on creatures isTaunt
    putStrLn "Please select target(defender) number:"
    if ((length enemyCreaturesTaunt) > 0) 
    then putStrLn "Enemy has taunt creatures!" 
    else putStr ""
 
    actions2 <- showTargetAsDefender enemyCreatures ((length enemyCreaturesTaunt) > 0)
    --print actions2
    targetId <- readAction actions2
    
    x@(_, name, creatureColor, (_, _, attackPoint1, _, _), _) <- getCreatureById creatures attckingId 
    y@(_, name, creatureColor, (_, _, attackPoint2, _, _), _) <- getCreatureById creatures targetId

    let newMyCreatures = changeCreatureHealthById (disableCreatureAttackById myCreatures attckingId) (-attackPoint2) attckingId      
    let newEnemyCreatures = changeCreatureHealthById enemyCreatures (-attackPoint1) targetId
    --print newMyCreatures
    --print attackPoint2 
    nowTurn color (newMyCreatures ++ newEnemyCreatures) player1 player2 creatureMaxId onDeaths

     
random a b 
  = do
    r <- getStdRandom ( randomR (a, b))
    let i =  r :: Int
    return i
 
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

    