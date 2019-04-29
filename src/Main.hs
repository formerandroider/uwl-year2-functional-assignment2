module Main where

import System.IO
import Data.Either (isRight)
import Game
import Types

main :: IO ()
main = do
    putStrLn "Welcome. The following commands are available: move n|s|e|w, quit"
    putStrLn ""
    let newRoom = Game.startingRoom
    putStrLn $ roomDescription newRoom
    requestAction (Game newRoom [])
    return ()

requestAction :: Game -> IO ()
requestAction game = do
    putStr "action> "
    hFlush stdout
    action <- getLine
    let newRoom = processCommand game (parseCommand action)
    putStrLn $ roomDescriptionOrMessage newRoom
    case newRoom of
      Right room -> do
        if roomEqual room exitRoom || roomEqual room quitRoom then
          return ()
        else do
          putStrLn $ describeFoundItems room
          requestAction (Game room (processItems game room))
      Left err -> requestAction game
    return ()

processCommand :: Game -> Maybe Command -> Either String Room
processCommand _ Nothing = Left "Invalid command"
processCommand game (Just (Command Move "n")) = moveInDirection game N
processCommand game (Just (Command Move "s")) = moveInDirection game S
processCommand game (Just (Command Move "e")) = moveInDirection game E
processCommand game (Just (Command Move "w")) = moveInDirection game W
processCommand _ (Just (Command Quit _)) = Right quitRoom
processCommand _ (Just (Command Move _)) = Left "Invalid direction"

moveInDirection :: Game -> Direction -> Either String Room
moveInDirection (Game curRoom items) direction = matchDoor (roomDoors curRoom) direction items

matchDoor :: [Door] -> Direction -> [Item] -> Either String Room
matchDoor ((Door dir room Nothing):doors) direction items = if dir == direction then Right room else matchDoor doors direction items
matchDoor ((Door dir room (Just item)):doors) direction items = if dir == direction then if itemPossessed items item then Right room else Left ("You can't go that way... you need the " ++ (show item)) else matchDoor doors direction items
matchDoor [] _ _ = Left "No door there..."