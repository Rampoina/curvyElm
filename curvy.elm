import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug


-- MODEL

type alias Game =
  { players : List Player
    , state : GameState
    , rounds : Int
    , currentRound : Int
  }

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

type Outcome = Playing | Draw | WonBy (List Player)
type PlayerState = Alive | KilledBy Player | Waiting
type GameState = WaitingRound Float | PlayingRound | RoundOver Float Outcome | GameOver
type alias Vector2D = { x: Float, y: Float}
type alias Position = Vector2D
type alias Keys = { x: Int, y: Int}


player1 : Player
player1 =
  { name = "Player1"
  , position = { x = 0
               , y = 0
               }
  , vel = 4
  , width = 10
  , angle = 0
  , color = Color.blue
  , lastPositions = []
  , state = Waiting
  }

player2 : Player
player2 =
  { name = "Player2"
  , position = { x = 100
               , y = 100
               }
  , vel = 4
  , width = 10
  , angle = 0
  , color = Color.red
  , lastPositions = []
  , state = Waiting
  }


game : Game
game =
  { players = [player1, player2]
    , state = WaitingRound 0.0
    , rounds = 10
    , currentRound = 0
  }

-- UPDATE

update : (Float, Keys, Keys) -> Game -> Game
update (dt, keys1, keys2) game =
    case game.state of
        PlayingRound ->
            case outcome game.players of
                Playing ->
                    { game |
                        players <- List.map (updatePlayer dt game.players keys1) game.players
                    }
                WonBy player ->
                    { game |
                        players <- List.map resetPlayer game.players
                        , state <- RoundOver dt (WonBy player)
                        , currentRound <- game.currentRound + 1
                    }
                Draw ->
                    { game |
                        players <- List.map resetPlayer game.players
                        , state <- RoundOver dt Draw
                        , currentRound <- game.currentRound + 1
                    }
        WaitingRound t ->
            if (t < 100) then
                { game |
                    players <- List.map (updatePlayer dt game.players keys1) game.players
                    , state <- WaitingRound (t+dt)
                }
            else
                { game |
                    players <- List.map readyPlayer game.players
                    , state <- PlayingRound
                }
        RoundOver t s ->
            if (t < 100) then
                { game |
                    players <- List.map (updatePlayer dt game.players keys1) game.players
                    , state <- RoundOver (t+dt) s
                }
            else
                { game |
                    players <- List.map readyPlayer game.players
                    , state <- WaitingRound dt
                }
        GameOver ->
            game

outcome : List Player -> Outcome
outcome players =
    let
        alivePlayers = List.filter (\player -> player.state == Alive) players
        numberAlivePlayers = List.length alivePlayers
    in
       case numberAlivePlayers of
           0 -> Draw
           1 -> WonBy alivePlayers
           _ -> Playing

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

distance : Position -> Position -> Float
distance p1 p2 = sqrt <| ((p2.x - p1.x) ^ 2) + ((p2.y - p1.y) ^2)

movePlayer : Float -> Player -> Player
movePlayer dt player =
    {
        player |
        position <- {
            x = player.position.x + dt * (sin player.angle) * player.vel
            ,y = player.position.y + dt * (cos player.angle) * player.vel
        }
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

addPosition : Player -> Player
addPosition player =
  { player |
        lastPositions <- player.lastPositions ++ [player.position]
  }
  
rotatePlayer : Float -> Keys -> Player -> Player
rotatePlayer dt keys player =
  { player |
      angle <- player.angle  + dt * degrees ((toFloat keys.x) * player.vel)
  }


-- VIEW

view : (Int, Int) -> Game -> Element
view (w',h') game =
    collage w' h' <| List.map drawPlayer game.players

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

-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update game input)

input : Signal (Float, Keys, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 60)
  in
    Signal.sampleOn delta (Signal.map3 (,,) delta Keyboard.arrows Keyboard.wasd)
