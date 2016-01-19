module Goban.UI
  where

import Goban.Position as GP

import Color
import Graphics.Collage as GC

type alias BoardFormSpec = { edgeSize : Int, margin : Float, stoneDiameter : Float }
testBoardFormSpec = { edgeSize = 9, margin = 1, stoneDiameter = 20 }

defaultBoardForm : BoardFormSpec -> GC.Form
defaultBoardForm {edgeSize, margin, stoneDiameter} =
  let edge = toFloat edgeSize
      scaled x = x * stoneDiameter
      scaledCoord (x, y) = (scaled x, scaled y)
      surface = let square = GC.square <| scaled <| edge + margin
                    center = GC.filled Color.yellow square
                    border = GC.outlined (GC.solid Color.black) square
                in GC.group [center, border]
      line c0 c1 =
        GC.traced (GC.solid Color.black) <|
        GC.segment (scaledCoord c0) (scaledCoord c1)
      hline y = line (1, y) (edge, y)
      vline x = line (x, 1) (x, edge)
      ixs = [1..edge]
      ints = GC.group <| List.map hline ixs ++ List.map vline ixs
      -- TODO: how should star locations be derived?
      star = GC.filled (Color.black) <| GC.circle (stoneDiameter / 6)
      starAt coord = GC.move (scaledCoord coord) star
      stars = GC.group <| List.map starAt [(3,3), (7,3), (7,7), (3,7), (5,5)]
      halflen = scaled <| -(edge + margin) / 2
      all = GC.group [surface, GC.move (halflen, halflen) <| GC.group [ints, stars]]
  in all

defaultStonesForm : BoardFormSpec -> GP.Board -> GC.Form
defaultStonesForm {edgeSize, margin, stoneDiameter} =
  let stoneRad = stoneDiameter / 2
      scaled x = x * stoneDiameter
      scaledCoord (x, y) = (scaled <| toFloat x, scaled <| toFloat y)
      form color = let circle = GC.circle stoneRad
                       center = GC.filled color circle
                       border = GC.outlined (GC.solid Color.black) circle
                   in GC.group [center, border]
      black = form Color.black
      white = form Color.white
      stoneForm st = case st of
        GP.Black -> black
        GP.White -> white
      stoneFormMoved coord = GC.move (scaledCoord coord) << stoneForm
      ixs = [1 .. edgeSize]
      halflen = scaled <| -(toFloat edgeSize + margin) / 2
      stonesForm board =
        let cell coord = Maybe.map (stoneFormMoved coord) <| GP.get board coord
            row ypos = List.filterMap (\x -> cell (x, ypos)) ixs
            stones = GC.group <| List.concat <| List.map row ixs
        in GC.move (halflen, halflen) stones
  in stonesForm


-- TODO: parametric scaling

boardElement board =
  let bf = defaultBoardForm testBoardFormSpec
      stones = defaultStonesForm testBoardFormSpec board
      all = GC.scale 1.5 <| GC.group [bf, stones]
  in GC.collage 400 400 [all]

posToCoord (xa, ya) =
  let trans p = round <| toFloat p / 30
      x = trans (xa - 80) + 1
      y = trans (322 - ya) + 1
      legal p = p >= 1 && p <= 9
  in if legal x && legal y then Just (x, y) else Nothing
