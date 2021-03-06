module Hearthstone where

import System.IO
import Data.Char
import System.Random (getStdRandom, randomR)
import System.Environment 
--import Data.List 
--import Data.Array
--import Data.Maybe
--import Control.Monad

{-
Denis Zhadan
2014-12-14
-}

-- creature colors
data Color = Red | Blue deriving (Show, Eq, Ord, Enum, Read)
-- get another color - need for easy code
next :: Color -> Color
next Red = Blue
next Blue = Red

type Crystals = Int -- hero power 1..10
type Turn = Int -- quantity of turns which the player made

type Creature = ((CreatureId, Name), Color, CreatureStatus, CardType, LogEvents) 
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

{- just I tried to write SHOWer
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

-- types for calculation after UntilDeath event
type FromCreatureId = Int
type IsUntil = Bool
type LogEvent = (FromCreatureId, IsUntil, CreatureEffect)
type LogEvents = [LogEvent]

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
game fileName1 fileName2
  = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    -- read decks for players
    deck1 <- readDeck (if fileName1 == "" then "deck1.txt" else fileName1)
    deck2 <- readDeck (if fileName2 == "" then "deck2.txt" else fileName2)
   
    let takeCardsForPlayer1 = 3
    let takeCardsForPlayer2 = 4

    -- player Red take 3 cards
    let cardsInHand1 = take takeCardsForPlayer1 deck1
    let newDeck1 = drop takeCardsForPlayer1 deck1

    -- player Blue take 4 cards, because second
    let cardsInHand2 = take takeCardsForPlayer2 deck2
    let newDeck2 = drop takeCardsForPlayer2 deck2
    let creatureMaxId = 2 :: Int

    newTurn (Red :: Color) 
            ([((0 :: CreatureId, "Red Hero"), Red, (False, 30 - 10 * (takeCardsForPlayer1 - length cardsInHand1), 0, False, True), MinionCard [] 30 0 False Nothing, [] :: LogEvents),
              ((1 :: CreatureId, "Blue Hero"), next Red, (False, 30 - 10 * (takeCardsForPlayer2 - length cardsInHand2), 0, False, True), MinionCard [] 30 0 False Nothing, [] :: LogEvents)] :: Creatures) 
            (cardsInHand1, newDeck1, 0 :: Crystals, 0 :: Turn)
            (cardsInHand2, newDeck2, 0 :: Crystals, 0 :: Turn)
            creatureMaxId
    return True

-- mark that creature can attack
setCreaturesCanAttack [] _ = []
setCreaturesCanAttack (x@((creatureId, name), creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype, logEvents) : xs) color
  | (creatureColor == color) = ((creatureId, name), creatureColor, (True :: CanAttack, healthPoint, attackPoint, isTaunt, isHero), ctype, logEvents) : setCreaturesCanAttack xs color
  | otherwise = x : setCreaturesCanAttack xs color

-- mark that creature can not attack
disableCreatureAttackById [] _ = []
disableCreatureAttackById (x@((creatureId, name), creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype, logEvents) : xs) id
  | id == creatureId = ((creatureId, name), creatureColor, (False :: CanAttack, healthPoint, attackPoint, isTaunt, isHero), ctype, logEvents) : disableCreatureAttackById xs id
  | otherwise = x : disableCreatureAttackById xs id

-- change creature health after attack/defense
changeCreatureHealthById [] _ _ _ = []
changeCreatureHealthById (x@((creatureId, name), creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype, logEvents) : xs) changeHealth id fromId
  | id == creatureId = ((creatureId, name), creatureColor, (canAttack, (healthPoint + changeHealth), attackPoint, isTaunt, isHero), ctype, logEvents ++ [(fromId, False, Health Relative changeHealth)]) : changeCreatureHealthById xs changeHealth id fromId
  | otherwise = x : changeCreatureHealthById xs changeHealth id fromId

-- remove creature from list
removeCreatureById [] _ = []
removeCreatureById (x@((creatureId, _), _, (_, healthPoint, _, _, isHero), _, logEvents) : xs) id
  | creatureId == id = removeCreatureById xs id
  | otherwise = x : removeCreatureById xs id

--get list of death creatures
getIsDeathCreatureIds [] _ = []
getIsDeathCreatureIds (x@((creatureId, _), _, (_, healthPoint, _, _, isHero), _, logEvents) : xs) inList
  | (healthPoint <= 0) && (not (elem creatureId inList)) && (isHero == False) = creatureId : getIsDeathCreatureIds xs inList
  | otherwise = getIsDeathCreatureIds xs inList

-- damage for Hero, can be set by color, used if no cards in deck, -10 points from hero
changeHeroHealthByColor [] _ _ = []
changeHeroHealthByColor (x@((creatureId, name), creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype, logEvents) : xs) changeHealth color
  | (creatureColor == color) && (isHero == True) = ((creatureId, name), creatureColor, (canAttack, (healthPoint + changeHealth), attackPoint, isTaunt, isHero), ctype, logEvents ++ [(-1, False, Health Relative changeHealth)]) : changeHeroHealthByColor xs changeHealth color
  | otherwise = x : changeHeroHealthByColor xs changeHealth color

-- for group events by ID(Int)
eventName2Int a
  = case a of
     OnPlay _ -> 1
     UntilDeath _ -> 2
     OnDamage _ -> 3
     OnDeath _ -> 4
     otherwise -> 0

--  takes only specified events  defined by parameter
effectsByEventNameId [] _ = []
effectsByEventNameId (x : xs) allowed
  | (elem (eventName2Int x) allowed) = x : effectsByEventNameId xs allowed
  | otherwise = effectsByEventNameId xs allowed

-- take card from deck
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

-- take 1 card from deck, before each new turn
initTurn color creatures player@(cardsInHand, cardsInDeck, crystals, turn)
  = do
    x <- takeCardFromDeck color creatures (cardsInHand, cardsInDeck, if turn < 10 then turn + 1 else 10, turn + 1)
    return x

-- start new turn for player
newTurn color creatures player1 player2 creatureMaxId
  = do     
    (player, newCreatures1) <- initTurn color creatures (if (color == Red) then player1 else player2)
    let newCreatures2 = setCreaturesCanAttack newCreatures1 color --set "CanAttack" = True for creatures of this player
    --print player
    nowTurn color newCreatures2 
            (if (color == Red) then player else player1)
            (if (color /= Red) then player else player2) 
            creatureMaxId

-- verify: is game over?
isGameEnd creatures
  = do
    hero1 @((_, _), _, (_, hp1, _, _, _), _, _) <- getHeroByColor Red creatures
    hero2 @((_, _), _, (_, hp2, _, _, _), _, _) <- getHeroByColor (next Red) creatures
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

-- just one step in player turn
nowTurn color creatures_ player1_ player2_ creatureMaxId
  = do 
    b <- isGameEnd creatures_
    if (b == True) 
    then do
      return True
    else do
      --let creatures = removeCreatureDead creatures_
      (creatures, player1, player2) <- onDeathEvent (getIsDeathCreatureIds creatures_ []) creatures_ player1_ player2_
      b2 <- isGameEnd creatures
      if (b2 == True) 
      then do
        return True
      else do
        result <- showTable color creatures (if (color == Red) then player1 else player2)
    
        case result of
          0 -> do
               newTurn (next color) creatures player1 player2 creatureMaxId
          1 -> do
               putCardToTable color creatures player1 player2 creatureMaxId
          2 -> do
               attackWithCreature color creatures player1 player2 creatureMaxId
          otherwise -> do 
                     nowTurn color creatures player1 player2 creatureMaxId
        --return True

-- get Hero by color
getHeroByColor color creatures
  = do 
    let x = head  (filter (\ ((_, _), heroColor, (_, _, _, _, isHero), _, logEvents) -> (heroColor == color) && (isHero == True)) creatures)
    --putStrLn (show a ++ show b)
    return x

-- filter cards by cost
getCardsByCost cards cost
  = do 
    let x = filter (\ (_, cardCost, _) -> cardCost <= cost) cards
    return x 

-- get creatures only for one player, filter by color
getCreaturesByColor creatures color
  = do
    let x = filter (\ ((_, _), creatureColor, _, _, logEvents) -> creatureColor == color) creatures
    return x 

-- get creature by uniq ID
getCreatureById creatures id
  = do
    let x = head(filter (\ ((creatureId, _), _, _, _, logEvents) -> creatureId == id) creatures)  
    return x 

--check, is creature can attack?
isCreatureCanAttack creature
  = do
    let x = filter (\ ((_, _), _, (canAttack, _, attackPoint, _, _), _, logEvents) -> (canAttack == True) && (attackPoint > 0)) creature
    return x 

-- is this creature has property Taunt
isCreatureTaunt creature
  = do
    let x = filter (\ ((_, _), _, (_, _, _, isTaunt, _), _, logEvents) -> isTaunt == True) creature
    return x 
    
-- show data for player
showTable color creatures player@(cardsInHand, deck, crystals, turn)
  = do
    showLine
    putStrLn ("Turn: " ++ show turn ++ " You have crystals: " ++ show crystals) 
    -- show my hero health 

    myHero @((_, _), _, (_, hp1, _, _, _), _, _) <- getHeroByColor color creatures
    putStrLn ("My hero("++show color ++") HP is: " ++ show hp1)

    -- show enemy hero health     
    enemyHero @((_, _), _, (_, hp2, _, _, _), _, _) <- getHeroByColor (next color) creatures
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

-- show allowed actions for player
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

-- string to Int
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

-- string to Int with default value if can not convert to Int
strToIntDef x d
  = if (isInteger x) 
    then read x :: Int
    else d

-- read action from player by keyboard.    
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

-- Show cards in hand, with card ID, if ID = X then user can not use this card(isn't enough crystals)
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

-- player uses the card 
putCardToTable color creatures player1 player2 creatureMaxId
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
    (z, m) <- cardToGame x color creatureMaxId
    let newCreatures = creatures ++ z :: Creatures
    let newCreatureMaxId = if length z > 0 then creatureMaxId + length z else creatureMaxId
    let newPlayer = (newCardsInHand, deck, crystals - cost, turn) 

    --print (getOnPlayEventEffects m)
    (aCreatures, aPlayer1, aPlayer2, aListDamage) <- magicEffect (getOnPlayEventEffects m) (if length z > 0 then creatureMaxId else -1) 
            color newCreatures 
            (if (color == Red) then newPlayer else player1)
            (if (color /= Red) then newPlayer else player2) 
            [] 1
    (bCreatures, bPlayer1, bPlayer2) <- onDamageEvent aListDamage aCreatures aPlayer1 aPlayer2 

    --print (getUntilDeathEventEffects m)
    (cCreatures, cPlayer1, cPlayer2, cListDamage) <- magicEffect (getUntilDeathEventEffects m) (if length z > 0 then creatureMaxId else -1) 
            color bCreatures bPlayer1 bPlayer2 [] 2

    (dCreatures, dPlayer1, dPlayer2) <- onDamageEvent cListDamage cCreatures cPlayer1 cPlayer2
            
    nowTurn color dCreatures dPlayer1 dPlayer2 newCreatureMaxId

forDamageList _ [] = []
forDamageList [] _ = []

forDamageList (x : xs) (id : [])
  = case x of
     Health Relative points -> (if points < 0 then [id] else []) ++ forDamageList xs (id : [])
     otherwise -> forDamageList xs (id : [])

forDamageList effects (id : ys@(_:_))
  = (forDamageList effects (id : [])) ++ forDamageList effects ys

-- get list IDs of creatures
getCreaturesId [] = []
getCreaturesId (c@((creatureId, _), _, _, _, _) : cs)
  = creatureId : getCreaturesId cs

-- get random creature id
getRandomCreatureId :: [Creature] -> IO [Int]
getRandomCreatureId creatures
  = do
    let ids = getCreaturesId creatures
    --print (ids) 
    if (length ids == 0) 
    then do
      return []
    else do
      n <- random 0 (length ids -1)
      let id = ids !! n
      return [id]

-- show creatures for select by player
showCreaturesAsChoose (c@((creatureId, _), _, _, _, _) : cs)
  = do
    putStrLn ( (show creatureId) ++ " - " ++ (show c))
    a <- showCreaturesAsChoose cs
    let b = a ++ [creatureId]
    return b
       
showCreaturesAsChoose _ = return []

--getChooseCreatureId :: [Creature] -> IO [Int]
getChooseCreatureId creatures creatureEffect
  = do
    --print (ids) 
    if (length creatures == 0) 
    then do
      return []
    else do
      putStrLn (show creatureEffect)
      putStrLn "Please select creature number:"
      ids <- showCreaturesAsChoose creatures
      --print actions1
      chooseId <- readAction ids
      return [chooseId]

{-
getCreaturesByFilter [] f = []
getCreaturesByFilter (c:cs) f
  | (filterApplies f c) == True = c : getCreaturesByFilter cs f
  | otherwise let x =  creatures
    return x
-}

-- get creatures by filter
getCreaturesByFilter creatures f creatureSelfId color
  = do 
    let r = filter (\x@((_, _), _, (_, healthPoint, _, _, _), _, logEvents) -> (&&) (healthPoint > 0) (filterApplies f x creatureSelfId color True) ) creatures
    return r 

-- applies creature effect
appliesCreatureEffect creature [] fromId isUntil = creature
appliesCreatureEffect creature@((creatureId, name), creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype, logEvents) (Health changeType points: es) fromId isUntil  
  | (changeType == Relative) = appliesCreatureEffect ((creatureId, name), creatureColor, (canAttack, healthPoint + points, attackPoint, isTaunt, isHero), ctype, logEvents ++ [(fromId, isUntil, Health changeType points)]) es fromId isUntil
  | otherwise = appliesCreatureEffect ((creatureId, name), creatureColor, (canAttack, points, attackPoint, isTaunt, isHero), ctype, logEvents ++ [(fromId, isUntil, Health changeType points)]) es fromId isUntil
  
appliesCreatureEffect creature@((creatureId, name), creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype, logEvents) (Attack changeType points: es) fromId isUntil
  | (changeType == Relative) = appliesCreatureEffect ((creatureId, name), creatureColor, (canAttack, healthPoint, attackPoint + points, isTaunt, isHero), ctype, logEvents ++ [(fromId, isUntil, Attack changeType points)]) es fromId isUntil
  | otherwise = appliesCreatureEffect ((creatureId, name), creatureColor, (canAttack, healthPoint, points, isTaunt, isHero), ctype, logEvents ++ [(fromId, isUntil, Attack changeType points)]) es fromId isUntil

appliesCreatureEffect creature@((creatureId, name), creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype, logEvents) (Taunt setTaunt: es) fromId isUntil 
  = appliesCreatureEffect ((creatureId, name), creatureColor, (canAttack, healthPoint, attackPoint, setTaunt, isHero), ctype, logEvents++ [(fromId, isUntil, Taunt setTaunt)]) es fromId isUntil
  
appliesCreatureEffectByIds [] _ _ fromId isUntil = []
appliesCreatureEffectByIds (x@((creatureId, _), _, _, _, _) : xs) creatureEffects ids fromId isUntil
  | (elem creatureId ids) = appliesCreatureEffect x creatureEffects fromId isUntil : appliesCreatureEffectByIds xs creatureEffects ids fromId isUntil 
  | otherwise = x : appliesCreatureEffectByIds xs creatureEffects ids fromId isUntil 

-- filter applies    
filterApplies :: [Filter] -> Creature -> Int -> Color -> Bool -> Bool
filterApplies [] _ _ _ _ = True

filterApplies (AnyCreature : fs) c@((_, _), _, (_, _, _, _, isHero), _, _) creatureSelfId color isConjunction
  = (if isConjunction then (&&) else (||)) (not isHero) (filterApplies fs c creatureSelfId color isConjunction)

filterApplies (AnyHero : fs) c@((_, _), _, (_, _, _, _, isHero), _, _) creatureSelfId color isConjunction
  = (if isConjunction then (&&) else (||)) isHero (filterApplies fs c creatureSelfId color isConjunction)

filterApplies (AnyFriendly : fs) c@((_, _), creatureColor, _, _, _) creatureSelfId color isConjunction
  = (if isConjunction then (&&) else (||)) (creatureColor == color) (filterApplies fs c creatureSelfId color isConjunction)

filterApplies (Type minionType : fs) c@((_, _), _, _, MinionCard _ _ _ _ creatureType, _) creatureSelfId color isConjunction
  = (if isConjunction then (&&) else (||)) (Just minionType == creatureType) (filterApplies fs c creatureSelfId color isConjunction)

filterApplies (Self : fs) c@((creatureId, _), _, _, _, _) creatureSelfId color isConjunction
  = (if isConjunction then (&&) else (||)) (creatureId == creatureSelfId) (filterApplies fs c creatureSelfId color isConjunction)

filterApplies (Not f : fs) c creatureSelfId color isConjunction
  = (if isConjunction then (&&) else (||)) (not(filterApplies f c creatureSelfId color True)) (filterApplies fs c creatureSelfId color isConjunction)

filterApplies (Any f : fs) c creatureSelfId color isConjunction
  = (if isConjunction then (&&) else (||)) (filterApplies f c creatureSelfId color False) (filterApplies fs c creatureSelfId color isConjunction)

-- set magic effect 
magicEffect [] creatureSelfId color creatures player1 player2 listDamage eventInt
  = do
    return (creatures, player1, player2, listDamage)

magicEffect (m@(x : []) : ms) creatureSelfId color creatures player1 player2 listDamage eventInt
  = do
    --showLine
    --print m
    --print ms
    --print x
    --print xs
    --showLine
    case x of
      All x y -> do
                    qt <- getCreaturesByFilter creatures x creatureSelfId color
                    let ids = getCreaturesId qt
                    putStrLn ("Effect: " ++ show y ++ " from creature nr. "++ show creatureSelfId ++ " to creature nr. " ++ show ids)
                    let newCreatures = appliesCreatureEffectByIds creatures y ids creatureSelfId (eventInt == 2)                    
                    magicEffect ms creatureSelfId color newCreatures player1 player2 (listDamage ++ forDamageList y ids) eventInt
      Choose x y ->do
                    qt <- getCreaturesByFilter creatures x creatureSelfId color
                    creatureId <- getChooseCreatureId qt y
                    --print creatureId
                    putStrLn ("Effect: " ++ show y ++ " from creature nr. "++ show creatureSelfId ++ " to creature nr. " ++ show creatureId)
                    let newCreatures = appliesCreatureEffectByIds creatures y creatureId creatureSelfId (eventInt == 2)
                    magicEffect ms creatureSelfId color newCreatures player1 player2 (listDamage ++ forDamageList y creatureId) eventInt
      Random x y ->do
                   qt <- getCreaturesByFilter creatures x creatureSelfId color
                   creatureId <- getRandomCreatureId qt
                   --print creatureId
                   putStrLn ("Effect: " ++ show y ++ " from creature nr. "++ show creatureSelfId ++ " to creature nr. " ++ show creatureId)
                   let newCreatures = appliesCreatureEffectByIds creatures y creatureId creatureSelfId (eventInt == 2)
                   magicEffect ms creatureSelfId color newCreatures player1 player2 (listDamage ++ forDamageList y creatureId) eventInt
      DrawCard -> do
                  (player, newCreatures) <- takeCardFromDeck color creatures (if (color == Red) then player1 else player2)
                  magicEffect ms creatureSelfId color newCreatures 
                       (if (color == Red) then player else player1)
                       (if (color /= Red) then player else player2)
                       listDamage eventInt

-- if present multy definitions of event types, for example [OnPlay, OnPlay, OnPlay]
magicEffect (m@(x : xs) : ms) creatureSelfId color creatures player1 player2 listDamage eventInt
  = do
    --showLine
    --putStrLn "m@(x : xs) : ms" 
    --print x
    --print xs
    --print creatures
    magicEffect ([x] : xs : ms) creatureSelfId color creatures player1 player2 listDamage eventInt

-- get events "on play"
getOnPlayEventEffects [] = []
getOnPlayEventEffects (a : as)
  = case a of
      OnPlay b -> b : getOnPlayEventEffects as
      _        -> getOnPlayEventEffects as

-- get events "until death"
getUntilDeathEventEffects [] = []
getUntilDeathEventEffects (a : as)
  = case a of
      UntilDeath b -> b : getUntilDeathEventEffects as
      _        -> getUntilDeathEventEffects as

-- get events "on damage"
getOnDamageEventEffects [] = []
getOnDamageEventEffects (a : as)
  = case a of
      OnDamage b -> b : getOnDamageEventEffects as
      _          -> getOnDamageEventEffects as

-- get events "on death"
getOnDeathEventEffects [] = []
getOnDeathEventEffects (a : as)
  = case a of
      OnDeath b -> b : getOnDeathEventEffects as
      _         -> getOnDeathEventEffects as

{-
isSpellCard a
  = case a of
     SpellCard _ -> True
     _ -> False     
-}
-- player uses SpellCard
cardToGame card @(a, b, c @(SpellCard effects)) color creatureMaxId
  = do
    let z = [] :: Creatures
    let m = effectsByEventNameId effects [1]
    return (z, m)
{-
  | isSpellCard c = do
--  | otherwise = do
    --otherwise -> True
-}

-- player uses MinionCard
cardToGame card @(a, b, c @(MinionCard effects hp ap isTaunt _)) color creatureMaxId
  = do
    --putStrLn ((show) isTaunt)
    --putStrLn ((show) c)
    let z = [((creatureMaxId :: CreatureId, a), color :: Color, (False :: CanAttack, hp :: HealthPoint, ap :: AttackPoint, isTaunt :: IsTaunt, False :: IsHero), c :: CardType, [] :: LogEvents)] :: Creatures
    let m = effectsByEventNameId effects [1,2]
    return (z, m) 

-- show list of creatures wich can attack
showCreaturesAsAttacker (c@((creatureId, name), creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype, logEvents) : cs)
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


-- show list of creatures wich is possible to attack
showTargetAsDefender (c@((creatureId, name), creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype, logEvents) : cs) onlyTaunt
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

-- attack by creatrue
attackWithCreature color creatures player1 player2 creatureMaxId
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
    
    x@((_, name), creatureColor, (_, _, attackPoint1, _, _), _, _) <- getCreatureById creatures attckingId 
    y@((_, name), creatureColor, (_, _, attackPoint2, _, _), _, _) <- getCreatureById creatures targetId

    let newMyCreatures = changeCreatureHealthById (disableCreatureAttackById myCreatures attckingId) (-attackPoint2) attckingId targetId
    let newEnemyCreatures = changeCreatureHealthById enemyCreatures (-attackPoint1) targetId attckingId
        
    let listDamage = targetId : if (attackPoint2 > 0) then [attckingId] else []
    --print listDamage
    (rCreatures, rPlayer1, rPlayer2) <- onDamageEvent listDamage (newMyCreatures ++ newEnemyCreatures) player1 player2

    --print newMyCreatures
    --print attackPoint2 
    nowTurn color rCreatures rPlayer1 rPlayer2 creatureMaxId

-- get random Int in this range
random a b 
  = do
    r <- getStdRandom ( randomR (a, b))
    let i =  r :: Int
    return i

-- run on event damage
onDamageEvent [] creatures player1 player2
  = do
    return (creatures, player1, player2)

onDamageEvent (id : listDamage) creatures player1 player2
  = do
    ((_, name), creatureColor, (_, _, _, _, _), (MinionCard effects _ _ _ _), _) <- getCreatureById creatures id
    --print (getOnDamageEventEffects effects)
    (rCreatures, rPlayer1, rPlayer2, rListDamage) <- magicEffect (getOnDamageEventEffects effects) id creatureColor creatures player1 player2 listDamage 3
    onDamageEvent rListDamage rCreatures rPlayer1 rPlayer2

-- run on event death
onDeathEvent [] creatures player1 player2
  = do
    return (creatures, player1, player2)

onDeathEvent (id : listDeath) creatures player1 player2
  = do
    ((_, name), creatureColor, (_, _, _, _, _), (MinionCard effects _ _ _ _), _) <- getCreatureById creatures id
    --print (getOnDeathEventEffects effects)
    (rCreatures, rPlayer1, rPlayer2, rListDamage) <- magicEffect 
                           (getOnDeathEventEffects effects) id creatureColor 
                           (removeCreatureById creatures id) player1 player2 [] 4
    (tCreatures, tPlayer1, tPlayer2) <- onDamageEvent rListDamage rCreatures rPlayer1 rPlayer2 

    let xCreatures = updateDataFromLog tCreatures id

    onDeathEvent (listDeath ++ getIsDeathCreatureIds xCreatures listDeath) xCreatures tPlayer1 tPlayer2

-- recalcultation (damage/bonus log, health, attack, taunt) if ends untilDeath
updateDataFromLog [] fromId = []
updateDataFromLog ( x@((creatureId, name), creatureColor, (canAttack, healthPoint, attackPoint, isTaunt, isHero), ctype@(MinionCard effects hp ap isT mtype), logEvents) : xs) fromId 
  = do
   let newLog = updateLog logEvents fromId
   let newHealth = updateHealth newLog hp
   let newAttack = updateAttack newLog ap
   let newIsTaunt = updateTaunt newLog isT
   ((creatureId, name), creatureColor, (canAttack, newHealth, newAttack, isTaunt, isHero), (MinionCard effects hp ap isT mtype), newLog) : updateDataFromLog xs fromId 

-- remove old records from log
updateLog [] _ = []
updateLog (x@(id, isUntil, effect) : xs) fromId
  | (id == fromId) && (isUntil == True) = updateLog xs fromId
  | otherwise = x : updateLog xs fromId

-- recalculation of health points
updateHealth [] hp = hp
updateHealth (l@(_,_,effect) : ls) hp 
  = case effect of
     Health Relative x -> updateHealth ls (hp + x) 
     Health Absolute x -> updateHealth ls x 
     otherwise -> updateHealth ls hp

-- recalculation of attack points
updateAttack [] ap = ap
updateAttack (l@(_,_,effect) : ls) ap 
  = case effect of
     Attack Relative x -> updateAttack ls (ap + x) 
     Attack Absolute x -> updateAttack ls x 
     otherwise -> updateHealth ls ap

-- recalculation status taunt
updateTaunt [] isTaunt = isTaunt
updateTaunt (l@(_,_,effect) : ls) isTaunt
  = case effect of
     Taunt x -> updateTaunt ls x 
     otherwise -> updateTaunt ls isTaunt

-- try autostart for game, do not work with WinHugs :(
main = do  
   args <- getArgs
   --mapM putStrLn args  
   if (length args >= 2)
   then game (args !! 0) (args !! 1)
   else do
     if (length args == 1) 
     then game (args !! 0) ""
     else game "" ""
 