module Goban.UI
  where

import Goban.Position as GP

import Color
import Graphics.Collage as GC

type alias PositionFormSpec =
  { edgeSize : Int
  , margin : Float
  , stoneDiameter : Float
  }

starPoints : number -> List (Float, Float)
starPoints edgeSize =
  let mid = (edgeSize + 1) / 2
      h0 = if edgeSize >= 12 then 4 else 3
      h1 = edgeSize - h0 + 1
      corners = if edgeSize >= 3*h0
                then [(h0, h0), (h0, h1), (h1, h0), (h1, h1)]
                else []
      sides = if edgeSize % 2 == 1 && edgeSize >= 4*h0 - 1
              then [(h0, mid), (h1, mid), (mid, h0), (mid, h1)]
              else []
      center = if edgeSize % 2 == 1 then [(mid, mid)] else []
  in corners ++ sides ++ center

defaultBoardForm : PositionFormSpec -> GC.Form
defaultBoardForm {edgeSize, margin, stoneDiameter} =
  let edge = toFloat edgeSize
      scaled x = x * stoneDiameter
      scaledPoint (x, y) = (scaled x, scaled y)
      surface = let square = GC.square <| scaled <| edge + margin
                    center = GC.filled Color.yellow square
                    border = GC.outlined (GC.solid Color.black) square
                in GC.group [center, border]
      line c0 c1 =
        GC.traced (GC.solid Color.black) <|
        GC.segment (scaledPoint c0) (scaledPoint c1)
      hline y = line (1, y) (edge, y)
      vline x = line (x, 1) (x, edge)
      ixs = [1..edge]
      ints = GC.group <| List.map hline ixs ++ List.map vline ixs
      star = GC.filled (Color.black) <| GC.circle (stoneDiameter / 6)
      starAt point = GC.move (scaledPoint point) star
      stars = GC.group <| List.map starAt <| starPoints edge
      halflen = scaled <| -(edge + 1) / 2
      markings = GC.move (halflen, halflen) <| GC.group [ints, stars]
      all = GC.group [surface, markings]
  in all

defaultStonesForm : PositionFormSpec -> GP.Position -> GC.Form
defaultStonesForm {edgeSize, stoneDiameter} =
  let scaled x = x * stoneDiameter
      scaledPoint (x, y) = (scaled <| toFloat x, scaled <| toFloat y)
      form color = let circle = GC.circle <| stoneDiameter / 2
                       center = GC.filled color circle
                       border = GC.outlined (GC.solid Color.black) circle
                   in GC.group [center, border]
      black = form Color.black
      white = form Color.white
      stoneForm st = case st of
        GP.Black -> black
        GP.White -> white
      stoneFormMoved point = GC.move (scaledPoint point) << stoneForm
      ixs = [1 .. edgeSize]
      halflen = scaled <| -(toFloat edgeSize + 1) / 2
      stonesForm pos =
        let cell point = Maybe.map (stoneFormMoved point) <| GP.get pos point
            row ypos = List.filterMap (\x -> cell (x, ypos)) ixs
            stones = GC.group <| List.concat <| List.map row ixs
        in GC.move (halflen, halflen) stones
  in stonesForm

defaultPositionForm : PositionFormSpec -> GP.Position -> GC.Form
defaultPositionForm pfspec =
  let bf = defaultBoardForm pfspec
      stonesWith = defaultStonesForm pfspec
      form pos = GC.group [bf, stonesWith pos]
  in form

type alias Point = (Int, Int)

absoluteToPoint : Point -> Float -> PositionFormSpec -> Point -> Maybe GP.Point
absoluteToPoint (xc, yc) scale {edgeSize, margin, stoneDiameter} =
  let csd = scale * stoneDiameter
      trans delta = 1 + (round <| delta / csd)
      xo = toFloat xc + csd*(margin + 1)/2
      yo = toFloat yc + csd*((margin + 1)/2 + toFloat edgeSize - 1)
      toPoint (xa, ya) =
        let x = trans (toFloat xa - xo)
            y = trans (yo - toFloat ya)
            legal p = p >= 1 && p <= edgeSize
        in if legal x && legal y then Just (x, y) else Nothing
  in toPoint
