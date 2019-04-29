module Game (startingRoom, exitRoom, quitRoom) where

import Types

quitRoom = Room "Quit" "You've abandoned your quest. Coward..." [] []

exitRoom = Room "Exit" "You've found the exit. All is well. Well done!" [] []
kitchen = Room "Kitchen" "You're in an ancient kitchen. It's very dim. There's an old stove to the north..." [] []

startingRoom = Room "Entrance" "You're in the entrance lobby..." [] [exitDoor,parlourDoor]
parlour = Room "Parlor" "You're in the parlor. It's eerily desolate..." [exitKey] [parlourDoorReverse]

parlourDoor = Door E parlour Nothing
parlourDoorReverse = Door W startingRoom Nothing

exitDoor = Door S exitRoom (Just exitKey)
exitKey = "Key of leaving"