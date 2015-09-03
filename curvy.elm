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
    case game.state of
        PlayingRound ->
            case outcome game.players of
                Playing ->
                    game
                    |> updatePlayers dt keys1
                WonBy player ->
                    game
                    |> nextRound
                    |> transition (RoundOver dt (WonBy player))
                Draw ->
                    game
                    |> nextRound
                    |> transition (RoundOver dt Draw)
        WaitingRound t ->
            if t < 100 then
                game
                |> updatePlayers dt keys1
                |> transition (WaitingRound (t+dt))
            else
                game
                |> preparePlayers
                |> transition PlayingRound
        RoundOver t s ->
            if game.currentRound > game.rounds then
               game
               |> transition GameOver
           else
                if t < 100 then
                    game
                    |> updatePlayers dt keys1
                    |> transition (RoundOver (t+dt) s)
                else
                    game
                    |> resetPlayers
                    |> transition (WaitingRound dt)
        GameOver ->
            game

updatePlayers :  Float -> Keys -> Game -> Game
updatePlayers dt keys game =
    { game |
        players <- List.map (updatePlayer dt game.players keys) game.players
    }

resetPlayers : Game -> Game
resetPlayers game =
    { game |
        players <- List.map resetPlayer game.players
    }

preparePlayers : Game -> Game 
preparePlayers game =
    { game |
        players <- List.map readyPlayer game.players
    }

transition : GameState -> Game -> Game
transition state game =
    { game |
        state <- state
    }

nextRound : Game -> Game
nextRound game =
    { game |
        currentRound <- game.currentRound + 1
    }

-- VIEW

view : (Int, Int) -> Game -> Element
view (w',h') game =
    collage w' h' <|
        case game.state of
            PlayingRound -> drawPlayingRound game w' h'
            WaitingRound t -> drawWaitingRound game t w' h'
            RoundOver t s -> drawRoundOver game t s w' h'
            GameOver -> drawGameOver game w' h'

drawPlayingRound : Game -> Int -> Int -> List Form
drawPlayingRound game w' h' =
    List.map drawPlayer game.players

drawWaitingRound : Game -> Float -> Int -> Int -> List Form
drawWaitingRound game t w' h' =
    [ rect (toFloat w') (toFloat h')
        |> filled black
        |> alpha (0.4 - (t/250))
        ,
        "Round " ++ (toString game.currentRound) ++ " starts in"
        |> fromString
        |> text
        |> move (0, toFloat (h')/2.1)
        ,
        toString ( floor ((100 - t)/30))
        |> fromString
        |> text
        |> move (0, toFloat (h')/2.3)
    ] ++ List.map drawPlayer game.players
    
drawRoundOver : Game -> Float -> Outcome -> Int -> Int -> List Form
drawRoundOver game t s w' h' =
    [ rect (toFloat w') (toFloat h')
        |> filled black
        |> alpha (0.0 + (t/250))
        ,
        outcomeToString s
        |> fromString
        |> text
        |> move (0, toFloat (h')/2.1)
    ] ++ List.map drawPlayer game.players

drawGameOver : Game -> Int -> Int -> List Form
drawGameOver game w' h' =
    ["GameOver"
    |> fromString 
    |> text
    |> move (0, toFloat (h')/2.1)
    ] ++ List.map drawPlayer game.players


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
