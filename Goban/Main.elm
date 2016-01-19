import Goban.Position as GP
import Goban.UI as GUI
import Goban.Variation as GV
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

view pos clickPos = GE.show (posToCoord pos) `GE.above` GE.show (posToCoord clickPos) `GE.above` GE.show pos `GE.above` GE.show clickPos
mouseView = Signal.map2 view Mouse.position <| Signal.sampleOn Mouse.clicks Mouse.position

update mc { stone, vcur } =
  let (GV.VTree vt) = vcur.focus
      mcoord = posToCoord mc
      (stone', vcur') = case mcoord `Maybe.andThen` \coord -> GP.add stone coord vt.position of
                              Nothing -> (stone, vcur)
                              Just (pos, _) -> (GP.invertStone stone, GV.add pos vcur)
  in { stone = stone', vcur = vcur' }

variationState = Signal.foldp update { stone = GP.Black, vcur = GV.empty cedge } <| Signal.sampleOn Mouse.clicks Mouse.position
variationView = Signal.map (\vs -> positionElement <| GV.get vs.vcur) variationState

main = Signal.map2 GE.above variationView mouseView
