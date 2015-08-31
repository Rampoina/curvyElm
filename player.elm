module Player where

import Color exposing (..)
import Graphics.Collage exposing (..)

type alias Player =
  { name : String
  , position : Position
  , vel : Float
  , angle : Float
  , width : Float
  , color: Color
  , lastPositions : List Position
  , state : PlayerState
  }

type PlayerState = Alive | KilledBy Player | Waiting
type alias Vector2D = { x: Float, y: Float}
type alias Position = Vector2D
type alias Keys = { x: Int, y: Int}

distance : Position -> Position -> Float
distance p1 p2 = sqrt <| ((p2.x - p1.x) ^ 2) + ((p2.y - p1.y) ^2)

updatePlayer : Float -> List Player -> Keys -> Player -> Player
updatePlayer dt players keys player =
    case player.state of
        Alive ->
            player
            |> rotatePlayer dt keys
            |> movePlayer dt
            |> addPosition
            |> handleCollision players
        Waiting ->
            player
            |> rotatePlayer dt keys
            |> movePlayer dt
        _ -> player


rotatePlayer : Float -> Keys -> Player -> Player
rotatePlayer dt keys player =
  { player |
      angle <- player.angle  + dt * degrees ((toFloat keys.x) * player.vel)
  }

movePlayer : Float -> Player -> Player
movePlayer dt player =
    { player |
        position <- {
            x = player.position.x + dt * (sin player.angle) * player.vel
            ,y = player.position.y + dt * (cos player.angle) * player.vel
        }
    }

addPosition : Player -> Player
addPosition player =
  { player |
        lastPositions <- player.lastPositions ++ [player.position]
  }

handleCollision : List Player -> Player -> Player
handleCollision players player =
    List.foldl collide player players

collide : Player -> Player -> Player
collide player2 player1 =
    let
        positions = if player1.name == player2.name then
                       List.take ((List.length player1.lastPositions) - 20) player1.lastPositions
                    else
                        player2.lastPositions

        playerWidths = (player1.width / 2) + (player2.width /2)
        distanceToPlayer1 = (\position -> (distance player1.position position) - playerWidths)
        distancesToPlayer1 = List.map distanceToPlayer1 positions
        treshold = 0.001
        collision = List.any (\distance -> distance < treshold) distancesToPlayer1
    in
        { player1 |
            state <- if collision then KilledBy player2 else player1.state
        }

readyPlayer : Player -> Player
readyPlayer player =
    { player |
        state <- Alive
    }

resetPlayer : Player -> Player
resetPlayer player =
    { player |
        position <- {x = 100
                    ,y = 100
                    }
        , lastPositions <- []
        , state <- Waiting
    }

initPlayer : Player

initPlayer = 
  { name = "defaultPlayer"
  , position = { x = 0
               , y = 0
               }
  , vel = 0
  , width = 0
  , angle = 0
  , color = Color.black
  , lastPositions = []
  , state = Waiting
  }


drawPlayer : Player -> Form
drawPlayer player =
      let
        positionToTuple = (\position -> (position.x, position.y))
        lineStyle = { defaultLine |
                    width <- player.width
                    , color <- player.color
                    , cap <- Round
                }
      in
         if List.isEmpty player.lastPositions then 
            circle (player.width / 2)
            |> filled player.color
            |> move (positionToTuple player.position)
         else
            List.map positionToTuple player.lastPositions
            |> path
            |> traced lineStyle
