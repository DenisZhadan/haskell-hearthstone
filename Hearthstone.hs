module Hearthstone where

import System.IO
import Data.Char
import System.Random (getStdRandom, randomR)
--import Data.Array
--import Data.Maybe
--import Control.Monad

{-
Denis Zhadan
2014-11-29
-}

-- creature colors
data Color = Red | Blue deriving (Show, Eq, Ord, Enum, Read)
-- get another color - need for easy code
next :: Color -> Color
next Red = Blue
next Blue = Red

type Crystals = Int -- hero power 1..10
type Turn = Int -- quantity of turns which the player made

type Creature = (Name, Color, CreatureStatus, CardType) 
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
            ([("Red Hero", Red, (False, 30, 0, False, True), MinionCard [] 30 0 False Nothing),
              ("Blue Hero", next Red, (False, 30, 0, False, True), MinionCard [] 30 0 False Nothing)] :: Creatures) 
            (cardsInHand1, newDeck1, 0 :: Crystals, 0 :: Turn) 
            (cardsInHand2, newDeck2, 0 :: Crystals, 0 :: Turn) 
    return True

setCreaturesCanAttack [] _ = []
setCreaturesCanAttack (x@(name, creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : xs) color
  | (creatureColor == color) = (name, creatureColor, (True :: CanAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : setCreaturesCanAttack xs color
  | otherwise = x : setCreaturesCanAttack xs color

disableCreatureAttackById [] _ = []
disableCreatureAttackById (x@(name, creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : xs) id
  | id == 0 = (name, creatureColor, (False :: CanAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : disableCreatureAttackById xs (id - 1)
  | otherwise = x : disableCreatureAttackById xs (id - 1)

changeCreatureHealthById [] _ _ = []
changeCreatureHealthById (x@(name, creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : xs) changeHealth id
  | id == 0 = (name, creatureColor, (canAttack, (healthPoint + changeHealth), attackPoint, isTaunt, isHero), ctype) : changeCreatureHealthById xs changeHealth (id - 1)
  | otherwise = x : changeCreatureHealthById xs changeHealth (id - 1)

removeCreatureDead [] = []
removeCreatureDead (x@(name, creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : xs)
  | healthPoint <= 0 = removeCreatureDead xs
  | otherwise = x : removeCreatureDead xs  

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

takeCardFromDeck player@(cardsInHand, cardsInDeck, crystals, turn)
  = do
    let newCardsInHand = cardsInHand ++ (take 1 cardsInDeck) 
    let newCardsInDeck = drop 1 cardsInDeck 
    let x = (newCardsInHand, newCardsInDeck, crystals, turn) :: Player
    return x

initTurn player@(cardsInHand, cardsInDeck, crystals, turn)
  = do
    x <- takeCardFromDeck (cardsInHand, cardsInDeck, if turn < 10 then turn + 1 else 10, turn + 1)
    return x

-- start new turn for player
newTurn color creatures player1 player2
  = do     
    player <- initTurn (if (color == Red) then player1 else player2)
    let newCreatures = setCreaturesCanAttack creatures color --set "CanAttack" = True for creatures of this player
    --print player
    nowTurn color newCreatures 
            (if (color == Red) then player else player1)
            (if (color /= Red) then player else player2)

isGameEnd creatures
  = do
    hero1 @(_, _, (_, hp1, _, _, _),_) <- getHeroByColor Red creatures
    hero2 @(_, _, (_, hp2, _, _, _),_) <- getHeroByColor (next Red) creatures
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

nowTurn color creatures0 player1 player2
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
             newTurn (next color) creatures player1 player2
        1 -> do
             putCardToTable color creatures player1 player2
        2 -> do
             attackWithCreature color creatures player1 player2
        otherwise -> do 
                     nowTurn color creatures player1 player2
      return True

getHeroByColor color creatures
  = do 
    let x = head  (filter (\ (_, heroColor, (_, _, _, _, isHero),_) -> (heroColor == color) && (isHero == True)) creatures)
    --putStrLn (show a ++ show b)
    return x

getCardsByCost cards cost
  = do 
    let x = filter (\ (_, cardCost, _) -> cardCost <= cost) cards
    return x 

getCreaturesByColor creatures color
  = do
    let x = filter (\ (_, creatureColor, _, _) -> creatureColor == color) creatures
    return x 

isCreatureCanAttack creature
  = do
    let x = filter (\ (_, _, (canAttack, _, attackPoint, _, _), _) -> (canAttack == True) && (attackPoint > 0)) creature
    return x 

isCreatureTaunt creature
  = do
    let x = filter (\ (_, _, (_, _, _, isTaunt, _), _) -> isTaunt == True) creature
    return x 
    
-- show data for player
showTable color creatures player@(cardsInHand, deck, crystals, turn)
  = do
    showLine
    putStrLn ("Turn: " ++ show turn ++ " You have crystals: " ++ show crystals) 
    -- show my hero health 

    myHero @(_, _, (_, hp1, _, _, _),_) <- getHeroByColor color creatures
    putStrLn ("My hero("++show color ++") HP is: " ++ show hp1)

    -- show enemy hero health     
    enemyHero @(_, _, (_, hp2, _, _, _),_) <- getHeroByColor (next color) creatures
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
    let b = a ++ [i]
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

putCardToTable color creatures player1 player2
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
    (z, m) <- cardToGame x color
    let newCreatures = creatures ++ z :: Creatures
    let newPlayer = (newCardsInHand, deck, crystals - cost, turn) 

    --print (getOnPlayEventEffects m)
    magicEffect (getOnPlayEventEffects m) color newCreatures 
            (if (color == Red) then newPlayer else player1)
            (if (color /= Red) then newPlayer else player2)    

magicEffect [] color creatures player1 player2
  = do nowTurn color creatures player1 player2

magicEffect (m@(x : []) : ms) color creatures player1 player2
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
                 magicEffect ms color creatures player1 player2
      Choose x y ->do
                    --chooseCreature :: [Creature] -> IO (Creature, [Creature])
                    magicEffect ms color creatures player1 player2
      Random x y ->do
                   --randomCreature :: [Creature] -> IO (Creature, [Creature])
                   n <- random 0 (length(creatures) -1)
                   --print (creatures !! n)
                   magicEffect ms color creatures player1 player2

      DrawCard -> do
                  player <- takeCardFromDeck (if (color == Red) then player1 else player2)
                  magicEffect ms color creatures 
                       (if (color == Red) then player else player1)
                       (if (color /= Red) then player else player2)

    --putStrLn "=== > < ==="
    --magicEffect ms color creatures player1 player2

magicEffect (m@(x : xs) : ms) color creatures player1 player2
  = do
    --showLine
    --putStrLn "m@(x : xs) : ms" 
    --print x
    --print xs
    magicEffect ([x] : xs : ms) color creatures player1 player2


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
cardToGame card @(a, b, c @(SpellCard effects)) color
  = do
    let z = [] :: Creatures
    let m = effectsByEventNameId effects [1]
    return (z, m)
{-
  | isSpellCard c = do
--  | otherwise = do
    --otherwise -> True
-}

cardToGame card @(a, b, c @(MinionCard effects hp ap isTaunt _)) color   
  = do
    --putStrLn ((show) isTaunt)
    --putStrLn ((show) c)
    let z = [(a, color :: Color, (False :: CanAttack, hp :: HealthPoint, ap :: AttackPoint, isTaunt :: IsTaunt, False :: IsHero), c :: CardType)] :: Creatures
    let m = effectsByEventNameId effects [1]
    return (z, m) 

showCreaturesAsAttacker (x@(name, creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : xs) i
  | (canAttack == True) && (attackPoint > 0)= do
    putStrLn ( (show i) ++ " - " ++ (show x))
    a <- (showCreaturesAsAttacker xs (i+1))
    let b = a ++ [i]
    return b
  | otherwise = do
    putStrLn ( "X" ++ " - " ++ (show x))
    a <- (showCreaturesAsAttacker xs (i+1))
    return a
       
showCreaturesAsAttacker _ _ = return []

showTargetAsDefender (x@(name, creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype) : xs) i onlyTaunt
  | ((isTaunt == False) && (onlyTaunt)) = do
    putStrLn ( "X" ++ " - " ++ (show x))
    a <- (showTargetAsDefender xs (i+1) onlyTaunt)
    return a
  | otherwise = do
    putStrLn ( (show i) ++ " - " ++ (show x))
    a <- (showTargetAsDefender xs (i+1) onlyTaunt)
    let b = a ++ [i]
    return b
       
showTargetAsDefender _ _ _ = return []

attackWithCreature color creatures player1 player2
  = do
    showLine
    myCreatures <- getCreaturesByColor creatures color
    enemyCreatures <- getCreaturesByColor creatures (next color)

    -- ask creature number from user
    putStrLn "Please select creature(attacker) number:"
    actions1 <- showCreaturesAsAttacker myCreatures 0
    --print actions1
    attckingId <- readAction actions1

    enemyCreaturesTaunt <- (isCreatureTaunt enemyCreatures) -- filter on creatures isTaunt
    putStrLn "Please select target(defender) number:"
    if ((length enemyCreaturesTaunt) > 0) 
    then putStrLn "Enemy has taunt creatures!" 
    else putStr ""
 
    actions2 <- showTargetAsDefender enemyCreatures 0 ((length enemyCreaturesTaunt) > 0)
    --print actions2
    targetId <- readAction actions2
    
    let x@(name, creatureColor, (_, _, attackPoint1, _, _), _) = myCreatures !! attckingId  
    let y@(name, creatureColor, (_, _, attackPoint2, _, _), _) = enemyCreatures !! targetId

    let newMyCreatures = changeCreatureHealthById (disableCreatureAttackById myCreatures attckingId) (-attackPoint2) attckingId      
    let newEnemyCreatures = changeCreatureHealthById enemyCreatures (-attackPoint1) targetId
    --print newMyCreatures
    --print attackPoint2 
    nowTurn color (newMyCreatures ++ newEnemyCreatures) player1 player2       

     
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

    