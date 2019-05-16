module Main where

-- System module imports
import System.IO
import Data.Either (isRight)

-- Custom module imports
import Game
import Types

-- Main function (called when running as a binary)
main :: IO ()
main = do
    putStrLn "Welcome. The following commands are available: move n|s|e|w, quit"
    putStrLn "" -- Print empty line
    -- Set the first room to the starting room from the Game module
    let newRoom = Game.startingRoom
    -- Describe the starting room
    putStrLn $ roomDescription newRoom
    -- Request the first action, with a game with no items held
    requestAction (Game newRoom [])
    return ()

-- Request an action from the user.
requestAction :: Game -> IO ()
requestAction game = do
    putStr "action> " -- Print the command prompt
    hFlush stdout -- Flush the output stream (required to prevent artifacts)
    action <- getLine -- Retrieve a line of input from the console
    -- Process the command and print the description of the returned room
    let newRoom = processCommand game (parseCommand action)
    putStrLn $ roomDescriptionOrMessage newRoom
    case newRoom of
      -- If the new room is valid (a Right value)
      Right room -> do
        -- Check to see if it is the exit or quit room
        if roomEqual room exitRoom || roomEqual room quitRoom then
          -- If it is, end this function to end the game
          return ()
        else do
          -- Otherwise, describe the items in the room
          putStr $ describeFoundItems room
          hFlush stdout
          -- And request another action
          requestAction (Game room (processItems game room))
      -- If it isn't valid, request another action
      Left err -> requestAction game
    return ()

-- Process a command to a (Maybe room)
processCommand :: Game -> Maybe Command -> Either String Room
-- If the command is Nothing (invalid command), state it.
processCommand _ Nothing = Left "Invalid command"
-- For directional commands, move in that direction
processCommand game (Just (Command Move "n")) = moveInDirection game N
processCommand game (Just (Command Move "s")) = moveInDirection game S
processCommand game (Just (Command Move "e")) = moveInDirection game E
processCommand game (Just (Command Move "w")) = moveInDirection game W
processCommand _ (Just (Command Quit _)) = Right quitRoom -- For quit commands, return the quitRoom
-- If we get here, we have an invalid direction. Error out.
processCommand _ (Just (Command Move _)) = Left "Invalid direction"

-- Move the player in the direction specified, returning either a room or an prompt message.
moveInDirection :: Game -> Direction -> Either String Room
moveInDirection (Game curRoom items) direction = matchDoor (roomDoors curRoom) direction items

-- Take a list of doors, a direction, and a list of items
-- and attempt to move to the door in that direction.
matchDoor :: [Door] -> Direction -> [Item] -> Either String Room
-- Recursion. If the door has no items, check if the door is for the given direction, and return the room, otherwise
-- rerun the command with the other doors.
matchDoor ((Door dir room Nothing):doors) direction items = if dir == direction then Right room else matchDoor doors direction items
-- If the door has item requirements, check
matchDoor ((Door dir room (Just item)):doors) direction items =
  -- Nested if statement.
  -- If the current doors direction is the requested direction then...
  if dir == direction then
    -- Check if the user possess the current item
    if itemPossessed items item then
      -- and if they do, return the room associated with this dore
      Right room
    else
      -- and if they don't, return an message saying they need the relevant item
      Left ("You can't go that way... you need the " ++ (show item))
  -- If the direction doesn't match, recurse with the other doors in the passed list.
  else matchDoor doors direction items
-- If a list of empty doors is passed, there is no door, so return a message stating this.
matchDoor [] _ _ = Left "No door there..."