import System.IO
import Debug.Trace
import Data.Either (isRight)

data Room = Room String String [Door] deriving Show
data Location = Location [Room] deriving Show
data Direction = N | S | E | W | None deriving Show

-- Current game state
-- -- The current room
-- -- A list of items the player possesses
data Game = Game (Maybe Room) [Item] deriving Show

instance Eq Direction where
    (==) N N = True
    (==) S S = True
    (==) E E = True
    (==) W W = True
    (==) _ _ = False

-- Represents a door in a room.
-- Contents:
-- -- Position of door in room
-- -- Room that the door leads to
-- -- An optional Item that is required to use door
data Door = Door Direction Room (Maybe Item) deriving Show

type Item = String

data Grid = Grid [Location] deriving Show

startingRoom = Room "Entrance" "You're in the entrance lobby..." []

data Action = Start | Move | Take | Use | Quit deriving Show
data Command = Command Action String deriving Show

parseCommand :: String -> Either String Command
parseCommand command
    | take 4 command == "move" = Right (Command (Move) (drop 5 command))
    | take 4 command == "take" = Right (Command (Take) (drop 5 command))
    | take 3 command == "use" = Right (Command (Use) (drop 4 command))
    | take 4 command == "quit" = Right (Command (Quit) (drop 5 command))
    | otherwise = Left "Error: Invalid command"

main :: IO ()
main = do
    let newRoom = processCommand (Game Nothing []) (Right (Command Start ""))
    handleRoom newRoom
    requestAction (Game (Just newRoom) [])
    return ()
    
requestAction :: Game -> IO ()
requestAction (Game room items) = do
    putStr "action> "
    action <- getLine
    let newRoom = processCommand game (parseCommand action)
    handleRoom newRoom
    if isRight newRoom then
        requestAction (Game newRoom items)
    else
        requestAction (Game newRoom items)
    return ()
    
processCommand :: Game -> Either String Command -> Either String Room
processCommand _ (Right (Command Start _)) = Right startingRoom
processCommand (Game room) (Right (Command Move "n")) = moveInDirection room N
processCommand (Game room) (Right (Command Move "s")) = moveInDirection room S
processCommand (Game room) (Right (Command Move "e")) = moveInDirection room E
processCommand (Game room) (Right (Command Move "w")) = moveInDirection room W
processCommand (Game room) (Right (Command Move _)) = Left "Error: Invalid direction"
processCommand _ (Left err) = Left err

moveInDirection :: Room -> Direction -> Either String Room
moveInDirection room direction = matchDoor (roomDoors room) direction

roomDoors (Room _ _ doors) = doors

itemPossessed :: [Item] -> Item -> Bool
itemPossessed (item:items) wanted = if item == wanted then True else itemPossessed items
itemPossessed [] _ = False

matchDoor :: [Door] -> Direction -> Either String Room
matchDoor ((Door dir room Prelude.Nothing):doors) direction = if dir == direction then Right room else matchDoor doors direction
matchDoor ((Door dir room item):doors) direction = if dir == direction then if userHasItem item then Right room else Left ("You need the " + (show item)) else matchDoor doors direction 
matchDoor [] _ = Left "No door there..."

handleRoom :: Either String Room -> IO ()
handleRoom (Left err) = do
    putStrLn err
    return ()
handleRoom (Right room) = do
    putStrLn $ roomDescription room
    return ()
    
roomDescription (Room _ desc _) = desc
    
    
    