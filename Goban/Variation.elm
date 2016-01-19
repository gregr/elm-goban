module Goban.Variation
  where

import Goban.Position as GP

type alias Alts =
  { current : VTree
  , prev : List VTree
  , next : List VTree
  }

type VTree = VTree
  { position : GP.Position
  , children : Maybe Alts
  }

type alias Context =
  { position : GP.Position
  , prev : List VTree
  , next : List VTree
  }

type alias VCursor = { focus : VTree, ancestors : List Context }

empty : Int -> VCursor
empty size =
  { focus = VTree { position = GP.empty size, children = Nothing }
  , ancestors = []
  }

withFocus : (VTree -> Maybe VTree) -> VCursor -> Maybe VCursor
withFocus op cursor =
  op cursor.focus `Maybe.andThen`
  \focus -> Just { cursor | focus = focus }

get : VCursor -> GP.Position
get {focus} = let (VTree {position}) = focus
              in position

edit : (GP.Position -> GP.Position) -> VCursor -> VCursor
edit op cursor =
  let trans (VTree vt) = VTree { vt | position = op vt.position }
  in { cursor | focus = trans cursor.focus }

put : GP.Position -> VCursor -> VCursor
put pos = edit (\_ -> pos)

-- TODO: search for existing alt with same position
add : GP.Position -> VCursor -> VCursor
add pos cursor =
  let cursor' = newAlt (VTree { position = pos, children = Nothing }) cursor
  in Maybe.withDefault cursor' <| nextPos cursor'

newAlt : VTree -> VCursor -> VCursor
newAlt alt cursor =
  let op (VTree focus) =
        let alts' = case focus.children of
              Nothing -> { current = alt, prev = [], next = [] }
              Just alts ->
                { alts | current = alt, prev = alts.current::alts.prev }
        in VTree { focus | children = Just alts' }
  in { cursor | focus = op cursor.focus }

nextPos : VCursor -> Maybe VCursor
nextPos {focus, ancestors} =
  let nextPosVT ancestors (VTree {position, children}) =
        children `Maybe.andThen`
        \{current, prev, next} ->
          let parent = { position = position, prev = prev, next = next }
          in Just { focus = current, ancestors = parent::ancestors }
  in nextPosVT ancestors focus

prevPos : VCursor -> Maybe VCursor
prevPos {focus, ancestors} = case ancestors of
  [] -> Nothing
  {position, prev, next}::ancestors ->
    let children = Just { current = focus, prev = prev, next = next }
        vt = VTree { position = position, children = children }
    in Just { focus = vt, ancestors = ancestors}

nextAlt : VCursor -> Maybe VCursor
nextAlt =
  let nextAltVT (VTree vt) =
        vt.children `Maybe.andThen`
        \{current, prev, next} -> case next of
          [] -> Nothing
          new::next ->
            let alts = Just { current = new, prev = new::prev, next = next }
            in Just <| VTree { vt | children = alts }
  in withFocus nextAltVT

prevAlt : VCursor -> Maybe VCursor
prevAlt =
  let prevAltVT (VTree vt) =
        vt.children `Maybe.andThen`
        \{current, prev, next} -> case prev of
          [] -> Nothing
          new::prev ->
            let alts = Just { current = new, prev = prev, next = new::next }
            in Just <| VTree { vt | children = alts }
  in withFocus prevAltVT
