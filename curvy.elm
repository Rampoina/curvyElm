import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug


-- MODEL

type alias Player =
  { x : Float
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
  { x = 0
  , y = 0
  , vel = 4
  , width = 4
  , angle = 0
  , color = Color.blue
  , lastPositions = []
  , alive = True
  }

player2 : Player
player2 =
  { x = 100
  , y = 100
  , vel = 4
  , width = 4
  , angle = 0
  , color = Color.red
  , lastPositions = []
  , alive = True
  }

-- UPDATE

update : (Float, Keys, Keys) -> (Player, Player) -> (Player, Player)
update (dt, keys1, keys2) (player1, player2) =
     ((player1 |> movePlayer dt keys1 |> physics dt) player2 |> addPosition,
      (player2 |> movePlayer dt keys2 |> physics dt) player1 |> addPosition
     )

distance : (Float, Float) -> (Float, Float) -> Float
distance p1 p2 = sqrt ((((fst p2 - fst p1) ^ 2) + ((snd p2 - snd p1) ^2)) ^ 2)
  
physics : Float -> Player -> Player -> Player
physics dt player player2 =
  let 
    collisionSelf = List.any (\p -> p < 80) (List.map (distance (player.x, player.y)) (List.take ((List.length player.lastPositions) - 20) player.lastPositions))
    collisionOthers = List.any (\p -> p < 80) (List.map (distance (player.x, player.y)) player2.lastPositions)
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

view : (Int, Int) -> (Player, Player) -> Element
view (w',h') (player1, player2) =
      let
        style1 = { defaultLine | width <- 10                          
                              , cap <- Round
                              , color <- player1.color
                }

        style2 = { defaultLine | width <- 10                          
                              , cap <- Round
                              , color <- player2.color
                }
      in 
        collage w' h' [ traced style1 (path player1.lastPositions),
                        traced style2 (path player2.lastPositions)
                    ]

-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update (player1, player2) input)

input : Signal (Float, Keys, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 60)
  in
    Signal.sampleOn delta (Signal.map3 (,,) delta Keyboard.arrows Keyboard.wasd)
