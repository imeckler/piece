import Mouse
import Graphics.Collage(..)
import Time
import Time (second)
import Piece
import Color
import Signal

circlePiece = Piece.for (1 * second) (\t ->
  circle 10 |> filled Color.blue |> moveX (t * (400 / second)))

circles = Signal.map (\_ -> circlePiece) Mouse.clicks

main = Piece.layer (Time.every 30) circles |> Signal.map (collage 400 400)

