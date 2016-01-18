module Goban.UI
  where

import Goban.Position as GP

import Color
import Graphics.Collage as GC

-- TODO: parametric scaling
stoneRad = 10
scaled x = x * stoneRad * 2
star = GC.filled (Color.black) <| GC.circle (stoneRad / 3)

stoneColor st = case st of
  GP.Black -> Color.black
  GP.White -> Color.white

stoneForm st = let color = stoneColor st
                   circle = GC.circle stoneRad
                   center = GC.filled color circle
                   border = GC.outlined (GC.solid Color.black) circle
               in GC.group [center, border]

sizedCoord (x, y) = (scaled <| toFloat x, scaled <| toFloat y)
scaledSeg c0 c1 = GC.segment (sizedCoord c0) (sizedCoord c1)
starAt coord = GC.move (sizedCoord coord) star

stoneFormMoved coord = GC.move (sizedCoord coord) << stoneForm

lineTracer = GC.traced (GC.solid Color.black)

scaledSquare sz = let square = GC.square <| toFloat sz
                      center = GC.filled Color.yellow square
                      border = GC.outlined (GC.solid Color.black) square
                  in GC.group [center, border]

boardElement board =
  let ixs = [1 .. board.size]
      cell coord = Maybe.map (stoneFormMoved coord) <| GP.get board coord
      row ypos = List.filterMap identity <| List.map (\x -> cell (x, ypos)) ixs
      stones = GC.group << List.concat <| List.map row ixs
      offset = stoneRad
      dim = board.size
      hline y = lineTracer <| scaledSeg (1, y) (dim, y)
      vline x = lineTracer <| scaledSeg (x, 1) (x, dim)
      hints = List.map hline <| ixs
      vints = List.map vline <| ixs
      --ints = GC.move (offset, offset) << GC.group <| hints ++ vints
      -- TODO: how should star locations be derived?
      backdrop = scaledSquare <| scaled <| board.size + 1
      stars = GC.group <| List.map starAt [(3,3), (7,3), (7,7), (3,7), (5,5)]
      ints = GC.group <| hints ++ vints
      movedim = scaled <| toFloat (-board.size - 1) / 2
      all = GC.scale 1.5 <| GC.group [backdrop, GC.move (movedim, movedim) <| GC.group [ints, stars, stones]]
  in GC.collage 400 400 [all]

-- TODO: coord-clicks signal

posToCoord (xa, ya) =
  let trans p = round <| toFloat p / scaled 1.5
      x = trans (xa - 80) + 1
      y = trans (322 - ya) + 1
      legal p = p >= 1 && p <= 9
  in if legal x && legal y then Just (x, y) else Nothing
