import Goban.Position as GP
import Goban.SGF as GS
import Goban.UI as GUI
import Goban.Variation as GV
import Graphics.Collage as GC
import Graphics.Element as GE
import Keyboard
import Mouse
import Text

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
arrowView = Signal.map GE.show <| Keyboard.arrows

placeStone mc vcur =
  let (GV.VTree vt) = vcur.focus
      {stoneToPlay} = vt.metadata
      mcoord = posToCoord mc
  in case mcoord `Maybe.andThen` \coord -> GP.add stoneToPlay coord vt.position of
       Nothing -> vcur
       Just (pos, _) -> GV.add pos (GS.playMetadata stoneToPlay GS.emptyMetadata) vcur

navigateVariations {x, y} vcur =
  Maybe.withDefault vcur <|
  if x == 1 && y == 0 then GV.nextPos vcur
  else if x == -1 && y == 0 then GV.prevPos vcur
  else if x == 0 && y == 1 then GV.prevAlt vcur
  else if x == 0 && y == -1 then GV.nextAlt vcur
  else Nothing

update gi = case gi of
  IClick mc -> placeStone mc
  IArrow arr -> navigateVariations arr

forwardMoveCount (GV.VTree {children}) =
  Maybe.withDefault 0 <|
  Maybe.map ((\n -> n + 1) << forwardMoveCount << .current) children

altCounts (GV.VTree {children}) = case children of
  Nothing -> (0, 0)
  Just {prev, next} -> (List.length prev, List.length next)

variationInfo vcur =
  let currentCount = List.length vcur.ancestors
      totalCount = currentCount + forwardMoveCount vcur.focus
      (prevCount, nextCount) = altCounts vcur.focus
      moveInfo = GE.leftAligned <| Text.fromString <| "Move " ++ toString currentCount ++ " of " ++ toString totalCount
      altInfo = GE.leftAligned <| Text.fromString <| "Variation " ++ toString (prevCount + 1) ++ " of " ++ toString (prevCount + 1 + nextCount)
  in moveInfo `GE.above` altInfo

type GameInput = IClick GUI.Coord | IArrow { x : Int, y : Int }
iclicks = Signal.map IClick <| Signal.sampleOn Mouse.clicks Mouse.position
iarrows = Signal.map IArrow Keyboard.arrows
input = Signal.merge iclicks iarrows

initVar = GV.empty cedge GS.emptyMetadata
variationState = Signal.foldp update initVar input
variationView = Signal.map (positionElement << GV.get) variationState
variationInfoView = Signal.map variationInfo variationState
sgfView = Signal.map (GE.show << GS.fromVariation) variationState

main = Signal.map2 GE.above variationView <| Signal.map2 GE.above variationInfoView <| Signal.map2 GE.above sgfView <| Signal.map2 GE.above arrowView mouseView
