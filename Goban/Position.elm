module Goban.Position
  where

import Dict exposing (Dict)
import Set exposing (Set)

type Stone = Black | White

invertStone : Stone -> Stone
invertStone stone = case stone of
  Black -> White
  White -> Black

type alias Coord = (Int, Int)
type alias Position = { size : Int, grid : Dict Coord Stone }

empty : Int -> Position
empty size = { size = size, grid = Dict.empty }

within : Position -> Coord -> Bool
within {size} (x, y) =
  let posOn p = p >= 1 && p <= size
  in posOn x && posOn y

get : Position -> Coord -> Maybe Stone
get position coord = Dict.get coord position.grid

remove : Coord -> Position -> Position
remove coord position = { position | grid = Dict.remove coord position.grid }

add : Stone -> Coord -> Position -> Maybe (Position, Maybe (Stone, Set Coord))
add stone coord position =
  if not <| within position coord then Nothing
  else case get position coord of
    Just _ -> Nothing
    Nothing -> Just <|
    let adjs = adjacent position.size coord
        position' = { position | grid = Dict.insert coord stone position.grid }
        stone' = invertStone stone
        removeCaptured stone coords =
          let caps = captured position' stone coords
          in if Set.size caps == 0 then Nothing
                else Just (Set.foldl remove position' caps, Just (stone, caps))
          in case removeCaptured stone' adjs of
            Just result -> result
            Nothing -> case removeCaptured stone [coord] of
              Just result -> result
              Nothing -> (position', Nothing)

captured position stone coords =
  let friendly (_, s) = stone == s
      captured1 coord ((seen, safe), found) =
        let adjs = adjacent position.size coord
            neighbors = List.map2 (,) adjs <| List.map (get position) adjs
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
        get position coord `Maybe.andThen`
        \stone' -> if stone /= stone' then Nothing
                   else Just << fst <|
                     let acc' = (Set.insert coord seen, safe)
                     in captured1 coord (acc', Set.singleton coord)
      (seen, alive) = List.foldl captured0 (Set.empty, Set.empty) coords
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
