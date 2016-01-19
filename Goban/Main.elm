import Goban.Position as GP
import Goban.UI as GUI
import Graphics.Element as GE
import Mouse

update mc { stone, position } =
  let --hoverCoord' = GUI.posToCoord mp
      mcoord = GUI.posToCoord mc
      (stone', position') = case mcoord `Maybe.andThen` \coord -> GP.add stone coord position of
                           Nothing -> (stone, position)
                           Just (b, _) -> (GP.invertStone stone, b)
  in { stone = stone', position = position'}

view pos clickPos = GE.show (GUI.posToCoord pos) `GE.above` GE.show (GUI.posToCoord clickPos) `GE.above` GE.show pos `GE.above` GE.show clickPos

positionState = Signal.foldp update { stone = GP.Black, position = GP.empty GUI.cedge } <| Signal.sampleOn Mouse.clicks Mouse.position
positionView = Signal.map (\bs -> GUI.positionElement bs.position) positionState
mouseView = Signal.map2 view Mouse.position <| Signal.sampleOn Mouse.clicks Mouse.position

main = Signal.map2 GE.above positionView mouseView

