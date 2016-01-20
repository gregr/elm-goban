import Goban.Position as GP
import Goban.UI as GUI
import Goban.Variation as GV
import Graphics.Collage as GC
import Graphics.Element as GE
import Keyboard
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

placeStone mc { stone, vcur } =
  let (GV.VTree vt) = vcur.focus
      mcoord = posToCoord mc
      (stone', vcur') = case mcoord `Maybe.andThen` \coord -> GP.add stone coord vt.position of
                              Nothing -> (stone, vcur)
                              Just (pos, _) -> (GP.invertStone stone, GV.add pos vcur)
  in { stone = stone', vcur = vcur' }

-- TODO: currently assumes stone alternates; add variation metadata instead?
navigateVariations {x, y} state =
  let stone' = GP.invertStone state.stone
      update st =
        Maybe.withDefault state <<
        Maybe.map (\vcur' -> { stone = st, vcur = vcur' })
      update0 = update stone'
      update1 = update state.stone
  in if x == 1 && y == 0 then update0 <| GV.nextPos state.vcur
     else if x == -1 && y == 0 then update0 <| GV.prevPos state.vcur
     else if x == 0 && y == 1 then update1 <| GV.prevAlt state.vcur
     else if x == 0 && y == -1 then update1 <| GV.nextAlt state.vcur
     else state

update gi = case gi of
  IClick mc -> placeStone mc
  IArrow arr -> navigateVariations arr

type GameInput = IClick GUI.Coord | IArrow { x : Int, y : Int }
iclicks = Signal.map IClick <| Signal.sampleOn Mouse.clicks Mouse.position
iarrows = Signal.map IArrow Keyboard.arrows
input = Signal.merge iclicks iarrows

variationState = Signal.foldp update { stone = GP.Black, vcur = GV.empty cedge } input
variationView = Signal.map (\vs -> positionElement <| GV.get vs.vcur) variationState

main = Signal.map2 GE.above variationView mouseView
