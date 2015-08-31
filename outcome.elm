module Outcome where

import Player exposing (..)
import Maybe exposing (..)

type Outcome = Playing | Draw | WonBy Player

outcome : List Player -> Outcome
outcome players =
    let
        alivePlayers = List.filter (\player -> player.state == Alive) players
        numberAlivePlayers = List.length alivePlayers
        defaultPlayer = initPlayer
    in
       case numberAlivePlayers of
           0 -> Draw
           1 -> WonBy (withDefault defaultPlayer (List.head alivePlayers))
           _ -> Playing
