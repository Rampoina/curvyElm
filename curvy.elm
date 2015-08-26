import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug


-- MODEL

type alias Game = 
  { players: List Player
  }

type alias Player =
  { name : String
  , x : Float
  , y : Float
  , vel : Float
  , angle : Float
  , width : Float
  , color: Color
  , lastPositions : List (Float, Float)
  , alive : Bool
  }

type alias Keys = { x:Int, y:Int }


player1 : Player
player1 =
  { name = "Player1" 
  , x = 0
  , y = 0
  , vel = 4
  , width = 10
  , angle = 0
  , color = Color.blue
  , lastPositions = []
  , alive = True
  }

player2 : Player
player2 =
  { name = "Player2" 
  , x = 100
  , y = 100
  , vel = 4
  , width = 10
  , angle = 0
  , color = Color.red
  , lastPositions = []
  , alive = True
  }


game : Game
game =
  { players = [player1, player2]
  }

-- UPDATE

update : (Float, Keys, Keys) -> Game -> Game
update (dt, keys1, keys2) game =
     { game |
         players <- List.map (updatePlayer dt game.players keys1) game.players
    }

updatePlayer : Float -> List Player -> Keys -> Player -> Player
updatePlayer dt players keys player =
    let
        otherPlayers = (List.filter (\p-> p /= player) players)
    in
    (player |> movePlayer dt keys |> physics dt) otherPlayers |> addPosition

distance : (Float, Float) -> (Float, Float) -> Float
distance p1 p2 = sqrt ((((fst p2 - fst p1) ^ 2) + ((snd p2 - snd p1) ^2)) ^ 2)
  
physics : Float -> Player -> List Player -> Player
physics dt player otherPlayers =
  let 
    collisionSelf = List.any (\p -> p < 80) (List.map (distance (player.x, player.y)) (List.take ((List.length player.lastPositions) - 20) player.lastPositions))
    collisionOthers = List.any (\p -> p < 80) (List.map (distance (player.x, player.y)) (List.foldl (++) [] (List.map (.lastPositions) otherPlayers)))
  in
  
  if (player.alive && ((not collisionSelf) && (not collisionOthers))) then
      {
        player |
          x <- player.x + dt * (sin player.angle) * player.vel
          ,y <- player.y + dt * (cos player.angle) * player.vel
      }
  else
    player
  
  
addPosition : Player -> Player
addPosition player =
  { player |
        lastPositions <- player.lastPositions ++ [(player.x, player.y)]
  }
  
movePlayer : Float -> Keys -> Player -> Player
movePlayer dt keys player =
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
        traced style (path player.lastPositions)

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
