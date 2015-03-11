module Example where

import Piece
import Piece.Infix(..)
import Time(second)
import Time
import Graphics.Collage(..)
import Signal
import Color
import Debug

pos =  Piece.cycle
    <| Piece.for (3 * second) (\t -> t * (40 / second))
    +> \x -> Piece.for (1 * second) (\t -> x - t * (x / (1 * second)))

main = Piece.run (Time.every 30) (Signal.constant pos)
  |> Signal.map (\x -> filled Color.red (circle 20) |> moveX x)
  |> Signal.map (\form -> collage 300 300 [form])

