import Goban.Position as GP
import Goban.UI as GUI
import Graphics.Element as GE

main = case GP.testb of
  Nothing -> GE.show "nothing"
  Just (board, _) -> GUI.boardElement board
