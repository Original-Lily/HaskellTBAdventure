import Base
import Data.Text.Array (equal)
import Data.List (deleteBy)
import Data.Function (on)

opposite :: Base.Direction -> Base.Direction
opposite Base.North = Base.South
opposite Base.South = Base.North
opposite Base.East  = Base.West
opposite Base.West  = Base.East

data Item = Key | Staff deriving (Show, Eq)

showItem :: Item -> String
showItem Key   = "Key"
showItem Staff = "Staff"

noActions :: Item -> GameState Item -> Next (GameState Item)
noActions item _ = Same $ "There is nothing that can be done with the " ++ showItem item ++ " here"

eqItem :: Item -> Item -> Bool
eqItem Key Key     = True
eqItem Staff Staff = True
eqItem Key Staff   = False
eqItem Staff Key   = False

winRoom :: Room Item
winRoom = Room
    {  name        = "Castle Tower"
     , description = "You won! Well done"
     , isWinRoom   = True
     , requires    = Just Key
     , items       = []
     , monsters    = []
     , doors       = [(West, dragonRoom)]
     , actions     = noActions
    }

dragonRoom :: Room Item
dragonRoom = Room
    {  name        = "Castle Foyer"
     , description = "You come face to face with an ancient, mossy creature"
     , isWinRoom   = False
     , requires    = Nothing
     , items       = [(Key, "in the troll's hand")]
     , monsters    = [WoodTroll {health = 10, holding = Key}]
     , doors       = [(South, startRoom), (East, winRoom)]
     , actions     = dragonRoomAction
    }


startRoom :: Room Item
startRoom = Room
    {  name        = "Castle Courtyard"
     , description = "This is where you journey starts"
     , isWinRoom   = False
     , requires    = Nothing
     , items       = [(Staff, "on the ground")]
     , monsters    = []
     , doors       = [(North, dragonRoom)]
     , actions     = noActions
    }

updateGameState :: GameState Item -> (Room Item -> Room Item) -> GameState Item
updateGameState gs f = gs { room = f (room gs) }

game0 :: GameState Item
game0 = GS (Player {playerName = "", inventory = []}) startRoom

dragonRoomAction :: Item -> GameState Item -> Next (GameState Item)
dragonRoomAction _ gameState =
  let monster = head $ monsters $ room gameState
      updatedGameState = updateGameState gameState $ \room ->
        let updatedMonsters = case monster of
              WoodTroll health heldItem ->
                ([WoodTroll (health - 5) heldItem | health > 5])
        in room { monsters = updatedMonsters }
  in case monster of
       WoodTroll _ _ ->
         if null $ monsters $ room gameState
           then Same "There is no monster"
           else Progress "You cast a spell at the creature with your staff" updatedGameState

consumePrefix :: String -> String -> Maybe String
consumePrefix [] b = Just b
consumePrefix a [] = Nothing
consumePrefix (x:xs) (y:ys)
  |x == y = consumePrefix xs ys
  | otherwise = Nothing

parseItem :: String -> Maybe Item
parseItem "key"    = Just Key
parseItem "staff" = Just Staff
parseItem _        = Nothing

parseDirection :: String -> Maybe Direction
parseDirection "north" = Just North
parseDirection "south" = Just South
parseDirection "east"  = Just East
parseDirection "west"  = Just West
parseDirection _       = Nothing

trim :: String -> String
trim [] = []
trim (x:xs) = if x == ' ' then trim xs else x:xs

parseCommand :: String -> Maybe (Command Item)
parseCommand input =
  case words (trim input) of
    ["go", dir]    -> case parseDirection dir of
      Just dir'    -> Just (Move dir')
      _            -> Nothing
    ["grab", item] -> case parseItem item of
      Just item'   -> Just (Grab item')
      _            -> Nothing
    ["use", item]  -> case parseItem item of
      Just item'   -> Just (Use item')
      _            -> Nothing
    ["end"]        -> Just End
    _              -> Nothing

deleteFrom :: (a -> Bool) -> [(a, b)] -> [(a, b)]
deleteFrom _ [] = []
deleteFrom pred ((key, val):xs)
  | pred key  = deleteFrom pred xs
  | otherwise = (key, val) : deleteFrom pred xs

updateDoors :: [(Direction, Room a)] -> Direction -> Room a -> [(Direction, Room a)]
updateDoors [] _ _ = []
updateDoors ((d, r):doors) dir newRoom
  | d == dir  = (d, newRoom) : doors
  | otherwise = (d, r) : updateDoors doors dir newRoom

leaveRoom :: Room a -> Direction -> Room a -> Room a 
leaveRoom fromRoom dir toRoom =
  let newDoors = updateDoors (doors fromRoom) dir toRoom
  in Room 
      { name         = name fromRoom
      , description = description fromRoom
      , isWinRoom   = isWinRoom fromRoom
      , requires    = requires fromRoom
      , items       = items fromRoom
      , monsters    = monsters fromRoom
      , doors       = newDoors
      , actions     = actions fromRoom
      }

checkInventory :: Item -> [Item] -> Bool
checkInventory _ [] = False
checkInventory item (x:xs)
  | eqItem item x = True
  | otherwise = checkInventory item xs


lookupItem :: Item -> [(Item, String)] -> Maybe String
lookupItem _ [] = Nothing
lookupItem item ((key, desc):xs)
  | eqItem item key = Just desc
  | otherwise = lookupItem item xs

step :: Command Item -> GameState Item -> Next (GameState Item)
step (Move dir) gameState =
  case lookupDoor dir (doors (room gameState)) of
    Just nextRoom ->
      case requires nextRoom of
        Just requiredItem ->
          if show requiredItem `elem` map show (inventory (player gameState))
          then Progress ("You move to another room.") (GS (player gameState) nextRoom)
          else Same "You need a required item to enter this room."
        Nothing -> Progress ("You move to another room.") (GS (player gameState) nextRoom)
    Nothing -> Same "There is no door in that direction."

step (Grab item) gameState =
  case lookupItem item (items (room gameState)) of
    Just desc ->
      let updatedItems = deleteFrom (\x -> show x == show item) (items (room gameState))
          updatedPlayer = (player gameState) { inventory = item : inventory (player gameState) }
      in Progress ("You grab the " ++ desc ++ ".") (GS updatedPlayer (room gameState) { items = updatedItems })
    Nothing -> Same "That item is not in this room."


step (Use item) gameState =
  if item `elem` inventory (player gameState)
  then actions (room gameState) item gameState
  else Same "You don't have that item in your inventory."

step End gameState = Progress "You leave the game. Goodbye" gameState




