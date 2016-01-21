module Goban.Variation
  where

import Goban.Position as GP

type alias Alts a =
  { current : VTree a
  , prev : List (VTree a)
  , next : List (VTree a)
  }

type VTree a = VTree
  { position : GP.Position
  , metadata : a
  , children : Maybe (Alts a)
  }

type alias Context a =
  { position : GP.Position
  , metadata : a
  , prev : List (VTree a)
  , next : List (VTree a)
  }

type alias VCursor a = { focus : VTree a, ancestors : List (Context a) }

empty : Int -> a -> VCursor a
empty size metadata =
  { focus = VTree { position = GP.empty size
                  , metadata = metadata
                  , children = Nothing }
  , ancestors = []
  }

withFocus : (VTree a -> Maybe (VTree a)) -> VCursor a -> Maybe (VCursor a)
withFocus op cursor =
  op cursor.focus `Maybe.andThen`
  \focus -> Just { cursor | focus = focus }

get : VCursor a -> GP.Position
get {focus} = let (VTree {position}) = focus
              in position

edit : (GP.Position -> GP.Position) -> VCursor a -> VCursor a
edit op cursor =
  let trans (VTree vt) = VTree { vt | position = op vt.position }
  in { cursor | focus = trans cursor.focus }

put : GP.Position -> VCursor a -> VCursor a
put pos = edit (\_ -> pos)

-- TODO: search for existing alt with same position
add : GP.Position -> a -> VCursor a -> VCursor a
add pos metadata cursor =
  let cursor' = newAlt (VTree { position = pos
                              , metadata = metadata
                              , children = Nothing }) cursor
  in Maybe.withDefault cursor' <| nextPos cursor'

newAlt : VTree a -> VCursor a -> VCursor a
newAlt alt cursor =
  let op (VTree focus) =
        let alts' = case focus.children of
              Nothing -> { current = alt, prev = [], next = [] }
              Just alts ->
                { alts | current = alt, prev = alts.current::alts.prev }
        in VTree { focus | children = Just alts' }
  in { cursor | focus = op cursor.focus }

nextPos : VCursor a -> Maybe (VCursor a)
nextPos {focus, ancestors} =
  let nextPosVT ancestors (VTree {position, metadata, children}) =
        children `Maybe.andThen`
        \{current, prev, next} ->
          let parent = { position = position
                       , metadata = metadata
                       , prev = prev
                       , next = next }
          in Just { focus = current, ancestors = parent::ancestors }
  in nextPosVT ancestors focus

prevPos : VCursor a -> Maybe (VCursor a)
prevPos {focus, ancestors} = case ancestors of
  [] -> Nothing
  {position, metadata, prev, next}::ancestors ->
    let children = Just { current = focus, prev = prev, next = next }
        vt = VTree { position = position
                   , metadata = metadata
                   , children = children }
    in Just { focus = vt, ancestors = ancestors}

nextAlt : VCursor a -> Maybe (VCursor a)
nextAlt =
  let nextAltVT (VTree vt) =
        vt.children `Maybe.andThen`
        \{current, prev, next} -> case next of
          [] -> Nothing
          new::next ->
            let alts = Just { current = new
                            , prev = current::prev
                            , next = next }
            in Just <| VTree { vt | children = alts }
  in withFocus nextAltVT

prevAlt : VCursor a -> Maybe (VCursor a)
prevAlt =
  let prevAltVT (VTree vt) =
        vt.children `Maybe.andThen`
        \{current, prev, next} -> case prev of
          [] -> Nothing
          new::prev ->
            let alts = Just { current = new
                            , prev = prev
                            , next = current::next }
            in Just <| VTree { vt | children = alts }
  in withFocus prevAltVT
