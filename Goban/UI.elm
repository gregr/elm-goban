module Goban.UI
  where

import Goban.Position as GP

import Color
import Graphics.Collage as GC

type alias BoardFormSpec =
  { edgeSize : Int
  , margin : Float
  , stoneDiameter : Float }

testBoardFormSpec : BoardFormSpec
testBoardFormSpec = { edgeSize = 9, margin = 1, stoneDiameter = 20 }

starCoords : number -> List (Float, Float)
starCoords edgeSize =
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
      star = GC.filled (Color.black) <| GC.circle (stoneDiameter / 6)
      starAt coord = GC.move (scaledCoord coord) star
      stars = GC.group <| List.map starAt <| starCoords edge
      halflen = scaled <| -(edge + 1) / 2
      markings = GC.move (halflen, halflen) <| GC.group [ints, stars]
      all = GC.group [surface, markings]
  in all

defaultStonesForm : BoardFormSpec -> GP.Board -> GC.Form
defaultStonesForm {edgeSize, stoneDiameter} =
  let scaled x = x * stoneDiameter
      scaledCoord (x, y) = (scaled <| toFloat x, scaled <| toFloat y)
      form color = let circle = GC.circle <| stoneDiameter / 2
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
      halflen = scaled <| -(toFloat edgeSize + 1) / 2
      stonesForm board =
        let cell coord = Maybe.map (stoneFormMoved coord) <| GP.get board coord
            row ypos = List.filterMap (\x -> cell (x, ypos)) ixs
            stones = GC.group <| List.concat <| List.map row ixs
        in GC.move (halflen, halflen) stones
  in stonesForm

defaultPositionForm : BoardFormSpec -> GP.Board -> GC.Form
defaultPositionForm bfspec =
  let bf = defaultBoardForm bfspec
      stonesWith = defaultStonesForm bfspec
      form board = GC.group [bf, stonesWith board]
  in form

cedge = 19
cdiam = 20
cbfs = { testBoardFormSpec | edgeSize = cedge, stoneDiameter = cdiam }
cscale = 1.5
cdim = 20 + round (cscale * cdiam * (cedge + cbfs.margin))

boardElement = let dpf = defaultPositionForm cbfs
                   form board = GC.collage cdim cdim [GC.scale 1.5 <| dpf board]
               in form

p2cSpec {edgeSize, margin, stoneDiameter} =
  let edge = toFloat edgeSize
      csd = cscale * stoneDiameter
      trans p = round <| p / csd
      offset = (toFloat cdim - csd*(edge - 1)) / 2
      p2c (xa, ya) =
        let x = trans (toFloat xa - offset) + 1
            y = trans (toFloat cdim - offset - toFloat ya) + 1
            legal p = p >= 1 && p <= edge
        in if legal x && legal y then Just (x, y) else Nothing
  in p2c

posToCoord = p2cSpec cbfs
