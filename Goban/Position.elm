module Goban.Position
  where

import Dict as Dict exposing (Dict)
import Maybe
import Set as Set exposing (Set)

type Stone = Black | White

invertStone : Stone -> Stone
invertStone stone = case stone of
  Black -> White
  White -> Black

type alias Coord = (Int, Int)
type alias Board = { size : Int, grid : Dict Coord Stone }

empty : Int -> Board
empty size = { size = size, grid = Dict.empty }

remove : Coord -> Board -> Board
remove coord board = { board | grid = Dict.remove coord board.grid }

get : Board -> Coord -> Maybe Stone
get board coord = Dict.get coord board.grid

captured board stone coords =
  let friendly (_, s) = stone == s
      captured1 coord ((seen, safe), found) =
        let adjs = adjacent board.size coord
            neighbors = List.map2 (,) adjs <| List.map (get board) adjs
            (libs, nonlibs) = partitionKV neighbors
            friends = List.map fst <| List.filter friendly nonlibs
            unseen = Set.diff (Set.fromList friends) seen
            seen' = Set.union unseen seen
            found' = Set.union unseen found
        in if List.length libs > 0 || List.any (flip Set.member safe) friends
           then ((seen', Set.union found' safe), Set.empty)
           else Set.foldl captured1 ((seen', safe), found') unseen
      captured0 coord (seen, safe) =
        Maybe.withDefault (seen, safe) <|
        get board coord `Maybe.andThen`
        \stone' -> if stone /= stone' then Nothing
                   else Just << fst <|
                     let acc' = (Set.insert coord seen, safe)
                     in captured1 coord (acc', Set.singleton coord)
      (seen, alive) = List.foldl captured0 (Set.empty, Set.empty) coords
  in Set.diff seen alive

add : Stone -> Coord -> Board -> Maybe (Board, Maybe (Stone, Set Coord))
add stone coord board = case get board coord of
  Just _ -> Nothing
  Nothing -> Just <|
    let adjs = adjacent board.size coord
        board' = { board | grid = Dict.insert coord stone board.grid }
        stone' = invertStone stone
        removeCaptured stone coords =
          let caps = captured board' stone coords
          in if Set.size caps == 0 then Nothing
             else Just (Set.foldl remove board' caps, Just (stone, caps))
    in case removeCaptured stone' adjs of
      Just result -> result
      Nothing -> case removeCaptured stone [coord] of
        Just result -> result
        Nothing -> (board', Nothing)

testb =
  add Black (5,5) (empty 9) `Maybe.andThen`
  (fst >> add White (3,5)) `Maybe.andThen`
  --(fst >> add White (4,4)) `Maybe.andThen`
  (fst >> add White (4,6)) `Maybe.andThen`
  (fst >> add White (6,5)) `Maybe.andThen`
  (fst >> add Black (4,5)) `Maybe.andThen`
  (fst >> add White (5,4)) `Maybe.andThen`
  (fst >> add White (5,6))

partitionKV =
  let partition (k, mv) (ns, js) = case mv of
    Nothing -> (k :: ns, js)
    Just v -> (ns, (k, v) :: js)
  in List.foldr partition ([], [])

adjacent size (xpos, ypos) =
  let adjacent1 pos =
        (if pos <= 1 then [] else [pos - 1]) ++
        (if pos >= size then [] else [pos + 1])
      hs = List.map (\xp -> (xp, ypos)) <| adjacent1 xpos
      vs = List.map (\yp -> (xpos, yp)) <| adjacent1 ypos
  in hs ++ vs
