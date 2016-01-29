module Goban.SGF
  where

import Goban.Position as GP
import Goban.Variation as GV

import Set
import String

type alias Metadata =
  { stoneToPlay : GP.Stone
  , properties : List BasicProperty
  }

-- TODO:
--fromVariation vcur =

toVariation : String -> Result (String, String) (GV.VCursor Metadata)
toVariation ss =
  collection ss `Result.andThen`
  \gts -> case gts of
    [gt] -> case gtToVariation gt of
      Nothing -> Err ("invalid game-tree", "")
      Just vcur -> Ok vcur
    _ -> Err ("expected one game-tree", "")

initMetadata = { stoneToPlay = GP.Black, properties = [] }
updateMetadata prop md = case prop of
  (Play color _) -> { md | stoneToPlay = GP.invertStone <| colorToStone color }
  _ -> md

gtToVariation gt =
  let (GameTree seq gts) = gt
      mvt = case seq of
        [] -> Nothing
        setup::_ -> case List.filterMap maybeSize setup of
          sz::_ -> Just <| applyGameTree (GP.empty sz, initMetadata) gt
          _ -> Nothing
  in Maybe.map (\vt -> { focus = vt, ancestors = [] }) mvt

applyGameTree pmd (GameTree seq gts) =
  let (pmd', trail) = applySeq pmd seq
      alts = List.map (applyGameTree pmd') gts
      children = case alts of
        [] -> Nothing
        alt0::alts' -> Just { current = alt0, prev = [], next = alts' }
      (pos, md) = pmd'
      last = GV.VTree { position = pos, metadata = md, children = children }
      newVT (pos, md) next =
        GV.VTree { position = pos
                 , metadata = md
                 , children = Just { current = next, prev = [], next = []} }
  in List.foldl newVT last trail

applySeq pmd seq =
  let (last, trail) = List.foldl applyNode (pmd, []) seq
  in (last, List.drop 1 trail)

applyNode node ((pos, md), trail) =
  let props = List.filterMap maybeBasic node
      md' = List.foldl updateMetadata md node
      pos' = applyProps pos node
      pmd = (pos', md')
  in (pmd, pmd::trail)

applyProps = List.foldl applyProp
applyProp prop pos = case prop of
  Play color pt ->
    Maybe.withDefault pos <| Maybe.map fst <|
    GP.add (colorToStone color) pt pos
  Add color ps -> let folder pt pos =
                        let add stone pos =
                              Maybe.withDefault pos <| Maybe.map fst <|
                              GP.add stone pt pos
                            update = case color of
                              AE -> identity
                              AB -> add GP.Black
                              AW -> add GP.White
                        in update <| GP.remove pt pos
                  in List.foldl folder pos ps
  _ -> pos

colorToStone color = case color of
  PB -> GP.Black
  PW -> GP.White

maybeSize prop = case prop of
  Size sz -> Just sz
  _ -> Nothing
maybeBasic prop = case prop of
  Basic bp -> Just bp
  _ -> Nothing

type alias Collection = List GameTree
type GameTree = GameTree Sequence (List GameTree)
type alias Sequence = List Node
type alias Node = List Property
type Property = Basic BasicProperty
              | Play PlayColor Point | Add AddColor (List Point) | Size Size
type AddColor = AE | AB | AW
type PlayColor = PB | PW
type alias BasicProperty = (PropIdent, List PropValue)
type alias PropIdent = String
type alias PropValue = String
type alias Size = Int
type alias Point = (Int, Int)

pure val ss = Ok (val, ss)
(>>=) p0 fp1 ss = p0 ss `Result.andThen` uncurry fp1
infixl 1 >>=
(*>) p0 p1 = p0 >>= \_ -> p1
infixl 4 *>
(<*) p0 p1 = p0 >>= \r0 -> p1 >>= \_ -> pure r0
infixl 4 <*
(<*>) p0 p1 = p0 >>= \f0 -> p1 >>= \r1 -> pure (f0 r1)
infixl 4 <*>
(<$>) f0 p1 = pure f0 <*> p1
infixl 4 <$>
(<|>) p0 p1 ss = case p0 ss of
  Ok answer -> Ok answer
  Err (e0, _) ->
    Result.formatError (\(e1, ss') -> (e0 ++ " OR " ++ e1, ss')) <| p1 ss
infixl 3 <|>
lift f val ss = let lifter = (flip (,) ss)
                in Result.formatError lifter <| Result.map lifter <| f val

eos result ss = case String.uncons ss of
  Nothing -> Ok result
  Just _ -> Err ("expected end of string", ss)
charPred errMsg pred ss = case String.uncons ss of
  Nothing -> Err (errMsg, ss)
  Just (ch, ss') -> if pred ch then Ok (ch, ss') else Err (errMsg, ss')
char ch = let errMsg = "expected character '" ++ String.fromChar ch ++ "'"
          in charPred errMsg (flip (==) ch)
charInStr str =
  charPred ("expected character from \"" ++ str ++ "\"") <|
  flip Set.member <| Set.fromList <| String.toList str
string str =
  let folder ch cs = (::) <$> char ch <*> cs
  in String.fromList <$> (List.foldr folder (pure []) <| String.toList str)
list0 parse ss =
  let loop ss =
    let do = (::) <$> parse <*> loop
    in Ok <| Result.withDefault ([], ss) <| do ss
  in loop ss
list1 parse = (::) <$> parse <*> list0 parse
wspace =
  list0 <| charPred "expected whitespace" <| flip Set.member <| Set.fromList <|
  String.toList " \t\v\n\r"
bracket lch rch p0 = wspace *> char lch *> wspace *> p0 <* wspace <* char rch

collection = list1 gameTree <* wspace >>= eos
gameTree ss = (bracket '(' ')' <| GameTree <$> sequence <*> list0 gameTree) ss
sequence = list1 node
node = wspace *> char ';' *> list0 property
property =
  wspace *> (playProperty <|> addProperty <|> sizeProperty <|> basicProperty)

playProperty =
  let prop color ident = string ident *> (Play color <$> pointValue)
  in prop PB "B" <|> prop PW "W"
addProperty =
  let prop color ident = string ident *> (Add color <$> list1 pointValue)
  in prop AE "AE" <|> prop AB "AB" <|> prop AW "AW"
pointValue = bracket '[' ']' pointValueText
pointValueText = (,) <$> pointLetter <*> ((\l -> 20 - l) <$> pointLetter)
pointLetter =
  String.fromChar <$> charInStr alphaLowerUpper >>=
  lift (\letter -> case String.indices letter alphaLowerUpper of
    [index] -> Ok <| index + 1
    _ -> Err <| "invalid letter '" ++ letter ++ "'")
sizeProperty = Size <$> (string "SZ" *> bracket '[' ']' nat)

basicProperty = Basic <$> ((,) <$> propIdent <*> list1 propValue)
propIdent = String.fromList <$> list1 ucLetter
ucLetter = charInStr alphaUpper

propValue = bracket '[' ']' valueText
valueText = String.fromList <$> list0 valueTextChar
valueTextChar =
  char '\\' *> charPred "expected any character" (\_ -> True) <|>
  charPred "expected non-']' character" (\ch -> ch /= ']')

nat = String.fromList <$> list1 digit >>= lift String.toInt
digit = charInStr digits
alphaUpper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
alphaLower = String.toLower alphaUpper
alphaLowerUpper = alphaLower ++ alphaUpper
digits = String.concat <| List.map toString [0..9]
