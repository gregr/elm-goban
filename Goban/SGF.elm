module Goban.SGF
  where

import Goban.Position as GP
import Goban.Variation as GV

import Set
import String

type alias Metadata =
  { stoneToPlay : GP.Stone
  , setup : Bool
  , properties : List BasicProperty
  }

fromVariation : GV.VCursor Metadata -> String
fromVariation vcur = printRootTree (GV.firstPos vcur).focus

printRootTree vt =
  let (GV.VTree {position}) = vt
      tree = printPosition (GP.empty position.size) vt
  in "(;" ++ printRoot position.size ++ tree ++ ")"
printPosition p0 (GV.VTree {position, metadata, children}) =
  let alts = case children of
        Nothing -> []
        Just {current, prev, next} -> List.reverse prev ++ current::next
  in printDiff p0 position metadata ++ printAlts position alts
printAlts parent alts = case alts of
  [] -> ""
  [single] -> printSeq parent single
  _ -> String.concat <| List.map (printTree parent) alts
printTree p0 vt = "(;" ++ printPosition p0 vt ++ ")"
printSeq p0 vt = ";" ++ printPosition p0 vt
printRoot size =
  "GM[1]FF[4]CA[UTF-8]AP[elm-goban:0]ST[2]SZ[" ++ toString size ++ "]"
charToStr char = case char of
  ']' -> "\\]"
  '\\' -> "\\"
  _ -> String.fromChar char
toValText str =
  "[" ++ (String.concat <| List.map charToStr <| String.toList str) ++ "]"
printBasic (ident, vals) = case ident of
  "GM" -> ""
  "FF" -> ""
  "CA" -> ""
  "AP" -> ""
  "ST" -> ""
  "SZ" -> ""
  _ -> ident ++ (String.concat <| List.map toValText vals)
printMetadata (GV.VTree {metadata}) =
  String.concat <| List.map printBasic metadata.properties
coordToLetter c =
  String.fromChar <| Maybe.withDefault '0' <| Maybe.map fst <| String.uncons <|
  String.slice (c - 1) c alphaLowerUpper
printPoint (x, y) = "[" ++ coordToLetter x ++ coordToLetter (20 - y) ++ "]"
printAdd ident ps = case List.map printPoint ps of
  [] -> ""
  pstrs -> ident ++ String.concat pstrs
printAdds added removed =
  let isBlack (_, stone) = case stone of
        GP.Black -> True
        GP.White -> False
      (bpss, wpss) = List.partition isBlack added
      bps = List.map fst bpss
      wps = List.map fst wpss
      es = printAdd "AE" removed
      bs = printAdd "AB" bps
      ws = printAdd "AW" wps
  in es ++ bs ++ ws
printPlay stone point = let ident = case stone of
                              GP.Black -> "B"
                              GP.White -> "W"
                        in ident ++ printPoint point
printDiff p0 p1 {setup} =
  let {added, removed} = GP.diff p0 p1
  in if setup then printAdds added removed
     else case added of
       [(pt, stone)] -> printPlay stone pt
       _ -> printAdds added removed

toVariations : String -> Result (String, String) (List (GV.VCursor Metadata))
toVariations ss =
  collection ss `Result.andThen` (Ok << List.filterMap gtToVariation)

toVariation : String -> Result (String, String) (GV.VCursor Metadata)
toVariation ss =
  collection ss `Result.andThen`
  \gts -> case gts of
    [gt] -> case gtToVariation gt of
      Nothing -> Err ("invalid game-tree", "")
      Just vcur -> Ok vcur
    _ -> Err ("expected one game-tree", "")

emptyMetadata = { stoneToPlay = GP.Black, setup = True, properties = [] }
playMetadata stone md =
  { md | stoneToPlay = (GP.invertStone stone), setup = False }
addMetadata md = { md | setup = True }
basicMetadata bp md = { md | properties = bp::md.properties }
stoneToPlayMetadata stone md = { md | stoneToPlay = stone }
updateMetadata prop md = case prop of
  (Play color _) -> playMetadata (colorToStone color) md
  (Add _ _) -> addMetadata md
  (Basic ("HA", _)) -> stoneToPlayMetadata GP.White md
  (Basic ("PL", ["B"])) -> stoneToPlayMetadata GP.Black md
  (Basic ("PL", ["W"])) -> stoneToPlayMetadata GP.White md
  (Basic bp) -> basicMetadata bp md
  _ -> md

gtToVariation gt =
  let (GameTree seq gts) = gt
      mvt = case seq of
        [] -> Nothing
        root::_ -> case List.filterMap maybeSize root of
          sz::_ -> Just <| applyGameTree (GP.empty sz, emptyMetadata) gt
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
  let md' = List.foldl updateMetadata { md | properties = [] } node
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
