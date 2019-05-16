-- Module holding the types
-- This allows game logic to be extracted to an alternative module
module Types where

-- Current game state
-- -- The next room, if moving rooms
-- -- A list of items the player possesses
data Game = Game Room [Item] deriving Show

-- Retrieves the item list from a game
playerItems (Game _ items) = items

-- A room in the game
-- -- The room name
-- -- The room description
-- -- A list of items the room contains
-- -- A list of doors in the room
data Room = Room String String [Item] [Door] deriving Show
-- Unpacks an Either instance into a room, or causes a fatal error
roomOnly :: Either String Room -> Room
roomOnly (Right room) = room
roomOnly _ = error "Invalid operation"

-- Checks if one room is equal to another
-- Internally, rooms are equal if they share the same name.
roomEqual :: Room -> Room -> Bool
roomEqual (Room name1 _ _ _) (Room name2 _ _ _)
  | name1 == name2 = True
  | otherwise = False

-- Returns either the room description, or a user notice if there is no room
roomDescriptionOrMessage :: Either String Room -> String
roomDescriptionOrMessage (Right room) = roomDescription room
roomDescriptionOrMessage (Left err) = err

-- Get the doors from a Room instance
roomDoors (Room _ _ _ doors) = doors
-- Get the description from a Room instance
roomDescription (Room _ desc _ _) = desc

-- Represents a door in a room.
-- Contents:
-- -- Position of door in room
-- -- Room that the door leads to
-- -- An optional Item that is required to use door
data Door = Door Direction Room (Maybe Item) deriving Show

-- An ordinal direction
data Direction = N | S | E | W deriving Show

-- Allow the equality function to be used on Directions
-- Directions are equal if they're the same
instance Eq Direction where
    (==) N N = True
    (==) S S = True
    (==) E E = True
    (==) W W = True
    (==) _ _ = False

-- A string representing an item that can be picked up by the player
-- The string should describe/name the item
type Item = String

-- Check if an item appears in the provided list
itemPossessed :: [Item] -> Item -> Bool
itemPossessed (item:items) wanted = if item == wanted then True else itemPossessed items wanted
itemPossessed [] _ = False

-- Merge items currently owned with items in a room
processItems :: Game -> Room -> [Item]
processItems (Game room items) (Room _ _ roomItems _) = roomItems ++ items

-- Describe any items that are in a room
describeFoundItems :: Room -> String
describeFoundItems (Room _ _ [] _) = ""
describeFoundItems (Room _ _ items _) = "You've found the following items: " ++ (expandItems "" items) ++ "\n"

-- Concatenate a list of items into a single comma separated string
expandItems :: String -> [Item] -> String
-- Matches a list of a single item
expandItems curString (item:[]) = item
-- Matches a list of more than one item
expandItems curString (item:items) = expandItems (curString ++ ", " ++ item) items
-- Return an empty string for an empty list
expandItems curString [] = ""

-- An action taken by the player
data Action = Move | Quit deriving Show

-- A command issued by the player
-- -- The action
-- -- A parameter for the action
data Command = Command Action String deriving Show

-- Parse a raw command string into a Command instance
parseCommand :: String -> Maybe Command
-- Check the first characters of the string represent a known command, and use the rest as the command parameter.
parseCommand command
    | take 4 command == "move" = Just (Command (Move) (drop 5 command))
    | take 4 command == "quit" = Just (Command (Quit) "")
    | otherwise = Nothing