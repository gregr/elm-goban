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

newAlt : VTree a -> VTree a -> VTree a
newAlt alt (VTree vt) =
  let alts' = case vt.children of
        Nothing -> { current = alt, prev = [], next = [] }
        Just alts ->
          { alts | current = alt, prev = alts.current::alts.prev }
  in VTree { vt | children = Just alts' }

init : GP.Position -> a -> VCursor a
init pos metadata =
  { focus = VTree { position = pos
                  , metadata = metadata
                  , children = Nothing }
  , ancestors = []
  }

empty : Int -> a -> VCursor a
empty size = init <| GP.empty size

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
  let focus' = newAlt (VTree { position = pos
                              , metadata = metadata
                              , children = Nothing }) cursor.focus
      cursor' = { cursor | focus = focus' }
  in Maybe.withDefault cursor' <| nextPos cursor'

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

firstPos : VCursor a -> VCursor a
firstPos vcur = Maybe.withDefault vcur <| Maybe.map firstPos <| prevPos vcur

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
