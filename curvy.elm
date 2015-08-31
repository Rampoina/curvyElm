import Player exposing (..)
import Text exposing (..)
import Outcome exposing (..)
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


type GameState = WaitingRound Float | PlayingRound | RoundOver Float Outcome | GameOver
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
    , rounds = 3
    , currentRound = 0
  }

-- UPDATE

update : (Float, Keys, Keys) -> Game -> Game
update (dt, keys1, keys2) game =
    let
        a = Debug.watch "State: " game.state
    in
    case game.state of
        PlayingRound ->
            case outcome game.players of
                Playing ->
                    { game |
                        players <- List.map (updatePlayer dt game.players keys1) game.players
                    }
                WonBy player ->
                    { game |
                        state <- RoundOver dt (WonBy player)
                        , currentRound <- game.currentRound + 1
                    }
                Draw ->
                    { game |
                         state <- RoundOver dt Draw
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
            if (game.currentRound > game.rounds) then
               { game | 
                   state <- GameOver
               }
           else
                if (t < 100) then
                    { game |
                        players <- List.map (updatePlayer dt game.players keys1) game.players
                        , state <- RoundOver (t+dt) s
                    }
                else
                    { game |
                        players <- List.map resetPlayer game.players
                        , state <- WaitingRound dt
                    }
        GameOver ->
            game

-- VIEW

view : (Int, Int) -> Game -> Element
view (w',h') game =
    collage w' h' <|
        case game.state of
            PlayingRound ->  List.map drawPlayer game.players
            WaitingRound t ->  
                        [ rect (toFloat w') (toFloat h')
                            |> filled black
                            |> alpha (0.4 - (t/250))
                            ,
                            drawCurrentRound game
                            |> move (0, toFloat (h')/2.1)
                            ,
                            toString ( floor ((100 - t)/30))
                            |> fromString
                            |> text
                            |> move (0, toFloat (h')/2.3)
                        ] ++ List.map drawPlayer game.players
            RoundOver t s ->  
                        [ rect (toFloat w') (toFloat h')
                            |> filled black
                            |> alpha (0.0 + (t/250))
                            ,
                            outcomeToString s
                            |> fromString
                            |> text
                            |> move (0, toFloat (h')/2.1)
                        ] ++ List.map drawPlayer game.players
            GameOver ->  
                ["GameOver"
                |> fromString 
                |> text
                |> move (0, toFloat (h')/2.1)
                ] ++ List.map drawPlayer game.players

drawCurrentRound : Game -> Form
drawCurrentRound game =
    "Round " ++ (toString game.currentRound) ++ " starts in"
    |> fromString
    |> text
    

outcomeToString : Outcome -> String
outcomeToString outcome =
    case outcome of
        Draw -> "Draw"
        Playing -> "Playing"
        WonBy player -> player.name ++ " wins"

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
