-- Module that holds the game logic, exporting only the starting, exit and quitting rooms
module Game (startingRoom, exitRoom, quitRoom) where

-- Import our types module
import Types

-- The quit room, which is used to quit the game early
quitRoom = Room "Quit" "You've abandoned your quest. Coward..." [] []
-- The exit room, which is the room returned to end/finish the game.
exitRoom = Room "Exit" "You've found the exit. All is well. Well done!" [] []

kitchen = Room "Kitchen" "You're in an ancient kitchen. It's very dim. There's an old stove to the north..." [] []

-- The starting room contains the door to the exit and the door to the parlour
startingRoom = Room "Entrance" "You're in the entrance lobby..." [] [exitDoor,parlourDoor]
-- The parlour contains the exit key, and the reverse of the parlour door
parlour = Room "Parlor" "You're in the parlor. It's eerily desolate..." [exitKey] [parlourDoorReverse]

-- The door to the parlour
-- Located in the East and requires no items
parlourDoor = Door E parlour Nothing

-- The reverse of the parlour door. Leads from the parlour back to the starting room.
parlourDoorReverse = Door W startingRoom Nothing

-- The exit door requires the exitKey
exitDoor = Door S exitRoom (Just exitKey)

-- Items are just a string describing themselves
exitKey = "Key of leaving"