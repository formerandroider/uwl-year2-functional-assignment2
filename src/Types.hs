module Types where

-- Current game state
-- -- The next room, if moving room
-- -- A list of items the player possesses
data Game = Game Room [Item] deriving Show
playerItems (Game _ items) = items

-- A room in the game
-- -- The room name
-- -- The room description
-- -- A list of items the room contains
-- -- A list of doors in the room
data Room = Room String String [Item] [Door] deriving Show
roomOnly :: Either String Room -> Room
roomOnly (Right room) = room
roomOnly _ = error "Invalid operation"

roomDescriptionOrMessage :: Either String Room -> String
roomDescriptionOrMessage (Right room) = roomDescription room
roomDescriptionOrMessage (Left err) = err

roomDoors (Room _ _ _ doors) = doors
roomDescription (Room _ desc _ _) = desc

-- Represents a door in a room.
-- Contents:
-- -- Position of door in room
-- -- Room that the door leads to
-- -- An optional Item that is required to use door
data Door = Door Direction Room (Maybe Item) deriving Show

-- A location (a tree of rooms)
-- -- The list of rooms used to get to this location
data Location = Location [Room] deriving Show

-- An ordinal direction
data Direction = N | S | E | W deriving Show

-- Allow the equality function to be used on Directions
instance Eq Direction where
    (==) N N = True
    (==) S S = True
    (==) E E = True
    (==) W W = True
    (==) _ _ = False

-- A string representing an item that can be picked up by the player
type Item = String
itemPossessed :: [Item] -> Item -> Bool
itemPossessed (item:items) wanted = if item == wanted then True else itemPossessed items wanted
itemPossessed [] _ = False

processItems :: Game -> Room -> [Item]
processItems (Game room items) (Room _ _ roomItems _) = roomItems ++ items

describeFoundItems :: Room -> String
describeFoundItems (Room _ _ items _) = "You've found the following items: " ++

-- A grid of rooms
data Grid = Grid [Location] deriving Show

-- An action taken by the player
data Action = Start | Move | Take | Use | Quit deriving Show

-- A command issued by the player
-- -- The action
-- -- A parameter for the action
data Command = Command Action String deriving Show
parseCommand :: String -> Maybe Command
parseCommand command
    | take 4 command == "move" = Just (Command (Move) (drop 5 command))
    | take 4 command == "take" = Just (Command (Take) (drop 5 command))
    | take 3 command == "use" = Just (Command (Use) (drop 4 command))
    | take 4 command == "quit" = Just (Command (Quit) "")
    | otherwise = Nothing