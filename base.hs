-- v1.0
module Base where

import Data.List ( intercalate )
import System.IO ( hFlush, stdout )

-- # Core types

-- ## Game State
-- The game state has a player and a room (the current room)
-- The type parameter here will be the type of items in the game
-- which you will define later
data GameState a = GS { player :: Player a, room :: Room a }
 deriving Show

-- The `Next` type is used to represent what happens next in a game
-- either things stay the `Same` or `Progress` is made to a new game state
data Next s =
      Same String       -- Message about why things are staying the same
    | Progress String s -- Success message and next thing `s`
 deriving Show

-- ## Player

-- Has a name a list of items given by the type parameter `a`
-- which you will define later (this is the inventory of the player)
data Player a =
  Player
    {   playerName :: String
      , inventory  :: [a]
    }
  deriving Show

-- ## Room (where all the action happens)
-- where `a` is the type of items in the game which you will define
data Room a =
  Room
    {  name        :: String
     , description :: String
     , isWinRoom   :: Bool
     -- ^ whether this room is a winning room
     , requires    :: Maybe a
       -- ^ whether an item is required to enter this room or not
     , items       :: [(a, String)]
       -- ^ Association list of items and some description about where they are
     , monsters    :: [Monster a]
       -- ^ Some monsters in this room
     , doors       :: [(Direction, Room a)]
       -- ^ Association list between directions and a room
     , actions     :: a -> GameState a -> Next (GameState a)
       -- ^ Function taking an item and a game state and returning what to do next
    }

-- ## Directions

data Direction = North | South | East | West
  deriving Eq

-- ## Monsters

data Monster a = WoodTroll { health :: Float, holding :: a }
  deriving Show

-- ## Helper functions for showing things

instance Show (Room a) where
  show r = name r ++ " ... "

instance Show Direction where
  show North  = "north"
  show South  = "south"
  show East   = "east"
  show West   = "west"

showDirection :: Direction -> String
showDirection = show

-- Lookup a door in a list of door-room pairs
lookupDoor :: Direction -> [(Direction, Room a)] -> Maybe (Room a)
lookupDoor dir [] = Nothing
lookupDoor dir ((x, y):xs) = if dir == x then Just y else lookupDoor dir xs

showMonster :: (a -> String) -> Monster a -> String
showMonster showItem (WoodTroll health item) = 
  "wood troll holding a " ++ showItem item

-- ## Command interface

data Command a =
    Move Direction | Use a | Grab a | End
  deriving Show

-- ## Helpers for reading input and giving output to the player

tellContextLine :: String -> IO ()
tellContextLine s = putStrLn $ "   " ++ s ++ "."

tellDoors :: [(Direction, Room a)] -> IO ()
tellDoors [] = tellContextLine $ "There are no doors."
tellDoors [(dir, _)] = tellContextLine $ "There is a door to the " ++ show dir
tellDoors doors =
  tellContextLine $ "There are doors to the " ++ (intercalate " and " (map (show . fst) doors))

tellItem :: (a -> String) -> (a, String) -> IO ()
tellItem showItem (item, pos) = tellContextLine $ pos ++ " there is a " ++ (showItem item)

tellMonster :: (a -> String) -> Monster a -> IO ()
tellMonster showItem monster = tellContextLine $ "There is a " ++ showMonster showItem monster

-- Main help for taking a game state and outputting information about it
tellContext :: (a -> String) -> GameState a -> IO ()
tellContext showItem (GS p r) = do
  putStrLn ""
  tellContextLine $ "You are in a " ++ name r ++ ". It is " ++ description r
  tellDoors (doors r)
  mapM (tellItem showItem) (items r)
  mapM (tellMonster showItem) (monsters r)
  putStrLn ""
  return ()

tellResponse :: String -> IO ()
tellResponse s = putStrLn $ "< " ++ s ++ "."

readCommand :: (String -> Maybe (Command a)) -> IO (Maybe (Command a))
readCommand parseCommand = do
  putStr "> "
  hFlush stdout
  input <- getLine
  return $ parseCommand input

-- ## Main game loop

play :: (Command a -> (GameState a) -> Next (GameState a))  -- step function
      -> (a -> String)                                      -- showItem function
      -> (String -> Maybe (Command a))                      -- parseCommand function
      -> GameState a                                        -- initial game state
      -> IO ()
play step showItem parseCommand s = do
  tellContext showItem s
  playLoop step showItem parseCommand s

playLoop :: (Command a -> GameState a -> Next (GameState a)) 
             -> (a -> String)
             -> (String -> Maybe (Command a))
             -> GameState a -> IO ()
playLoop step showItem parseCommand s = do
  if isWinRoom (room s)
    then tellResponse $ "You won " ++ playerName (player s) ++ "! Well done"
    else do
      mcommand <- readCommand parseCommand

      case mcommand of
        Nothing -> do
          putStrLn "Unknown command"
          playLoop step showItem parseCommand s

        Just End -> tellResponse "You leave the game. Goodbye"

        Just c -> do
          case step c s of
            Same msg -> do
              tellResponse $ msg
              playLoop step showItem parseCommand s
            Progress msg s' -> do
              tellResponse $ msg
              tellContext showItem s'
              playLoop step showItem parseCommand s'