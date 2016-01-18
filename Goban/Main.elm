import Goban.Position as GP
import Goban.UI as GUI
import Graphics.Element as GE
import Mouse

update mc { stone, board } =
  let --hoverCoord' = GUI.posToCoord mp
      mcoord = GUI.posToCoord mc
      (stone', board') = case mcoord `Maybe.andThen` \coord -> GP.add stone coord board of
                           Nothing -> (stone, board)
                           Just (b, _) -> (GP.invertStone stone, b)
  in { stone = stone', board = board'}

view pos clickPos = GE.show (GUI.posToCoord pos) `GE.above` GE.show (GUI.posToCoord clickPos) `GE.above` GE.show pos `GE.above` GE.show clickPos

boardState = Signal.foldp update { stone = GP.Black, board = GP.empty 9 } <| Signal.sampleOn Mouse.clicks Mouse.position
boardView = Signal.map (\bs -> GUI.boardElement bs.board) boardState
mouseView = Signal.map2 view Mouse.position <| Signal.sampleOn Mouse.clicks Mouse.position

main = Signal.map2 GE.above boardView mouseView

