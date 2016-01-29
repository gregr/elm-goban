module Goban.Position
  where

import Dict exposing (Dict)
import Set exposing (Set)

type Stone = Black | White

invertStone : Stone -> Stone
invertStone stone = case stone of
  Black -> White
  White -> Black

type alias Point = (Int, Int)
type alias Position = { size : Int, grid : Dict Point Stone }

empty : Int -> Position
empty size = { size = size, grid = Dict.empty }

within : Position -> Point -> Bool
within {size} (x, y) =
  let posOn p = p >= 1 && p <= size
  in posOn x && posOn y

get : Position -> Point -> Maybe Stone
get position point = Dict.get point position.grid

remove : Point -> Position -> Position
remove point position = { position | grid = Dict.remove point position.grid }

add : Stone -> Point -> Position -> Maybe (Position, Maybe (Stone, Set Point))
add stone point position =
  if not <| within position point then Nothing
  else case get position point of
    Just _ -> Nothing
    Nothing -> Just <|
    let adjs = adjacent position.size point
        position' = { position | grid = Dict.insert point stone position.grid }
        stone' = invertStone stone
        removeCaptured stone points =
          let caps = captured position' stone points
          in if Set.size caps == 0 then Nothing
                else Just (Set.foldl remove position' caps, Just (stone, caps))
          in case removeCaptured stone' adjs of
            Just result -> result
            Nothing -> case removeCaptured stone [point] of
              Just result -> result
              Nothing -> (position', Nothing)

diff : Position -> Position -> { added : List (Point, Stone)
                               , removed : List Point }
diff pos0 pos1 =
  let g0 = pos0.grid
      g1 = pos1.grid
      added = Dict.toList <| Dict.diff g1 g0
      removed = Dict.keys <| Dict.diff g0 g1
  in { added = added, removed = removed }

captured position stone points =
  let friendly (_, s) = stone == s
      captured1 point ((seen, safe), found) =
        let adjs = adjacent position.size point
            neighbors = List.map2 (,) adjs <| List.map (get position) adjs
            (libs, nonlibs) = partitionKV neighbors
            friends = List.map fst <| List.filter friendly nonlibs
            unseen = Set.diff (Set.fromList friends) seen
            seen' = Set.union unseen seen
            found' = Set.union unseen found
        in if List.length libs > 0 || List.any (flip Set.member safe) friends
           then ((seen', Set.union found' safe), Set.empty)
           else Set.foldl captured1 ((seen', safe), found') unseen
      captured0 point (seen, safe) =
        Maybe.withDefault (seen, safe) <|
        get position point `Maybe.andThen`
        \stone' -> if stone /= stone' then Nothing
                   else Just << fst <|
                     let acc' = (Set.insert point seen, safe)
                     in captured1 point (acc', Set.singleton point)
      (seen, alive) = List.foldl captured0 (Set.empty, Set.empty) points
  in Set.diff seen alive

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
