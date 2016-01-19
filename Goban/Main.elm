import Goban.Position as GP
import Goban.UI as GUI
import Graphics.Collage as GC
import Graphics.Element as GE
import Mouse

cedge = 19
cdiam = 20
cpfs = { edgeSize = cedge, margin = 1, stoneDiameter = cdiam }
cscale = 1.5
coffset = 10
cdim = coffset*2 + round (cscale * cdiam * (cedge + cpfs.margin))

positionElement =
  let dpf = GUI.defaultPositionForm cpfs
      form pos = GC.collage cdim cdim [GC.scale cscale <| dpf pos]
  in form

posToCoord = GUI.absoluteToCoord (coffset, coffset) cscale cpfs

update mc { stone, position } =
  let mcoord = posToCoord mc
      (stone', position') = case mcoord `Maybe.andThen` \coord -> GP.add stone coord position of
                           Nothing -> (stone, position)
                           Just (b, _) -> (GP.invertStone stone, b)
  in { stone = stone', position = position'}

view pos clickPos = GE.show (posToCoord pos) `GE.above` GE.show (posToCoord clickPos) `GE.above` GE.show pos `GE.above` GE.show clickPos

positionState = Signal.foldp update { stone = GP.Black, position = GP.empty cedge } <| Signal.sampleOn Mouse.clicks Mouse.position
positionView = Signal.map (\ps -> positionElement ps.position) positionState
mouseView = Signal.map2 view Mouse.position <| Signal.sampleOn Mouse.clicks Mouse.position

main = Signal.map2 GE.above positionView mouseView

