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
type PlayerState = Alive | KilledBy Player
type GameState = Elapsing | BetweenRounds Float | GameOver
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
  , state = Alive
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
  , state = Alive
  }


game : Game
game =
  { players = [player1, player2]
    , state = Elapsing
    , rounds = 10
    , currentRound = 0
  }

-- UPDATE

update : (Float, Keys, Keys) -> Game -> Game
update (dt, keys1, keys2) game =
    case game.state of
        Elapsing ->
            case outcome game.players of
                Playing ->
                    { game |
                        players <- List.map (updatePlayer dt game.players keys1) game.players
                    }
                WonBy player ->
                    { game |
                        players <- List.map resetPlayer game.players
                        , state <- BetweenRounds dt
                        , currentRound <- game.currentRound + 1
                    }
                Draw ->
                    { game |
                        players <- List.map resetPlayer game.players
                        , state <- BetweenRounds dt
                        , currentRound <- game.currentRound + 1
                    }
        BetweenRounds t ->
            if (t < 100) then
                { game |
                    state <- BetweenRounds (t+dt)
                }
            else
                { game |
                    state <- Elapsing
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

resetPlayer : Player -> Player
resetPlayer player =
    { player |
        position <- {x = 100
                    ,y = 100
                    }
        , lastPositions <- []
        , state <- Alive
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
        style = { defaultLine | width <- player.width
                              , cap <- Round
                              , color <- player.color
                }
      in
        traced style (path (List.map (\position -> (position.x, position.y)) player.lastPositions))

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
