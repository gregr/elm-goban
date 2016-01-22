module Goban.SGF
  where

import Set
import String

-- TODO: toVariation, fromVariation

type alias Collection = List GameTree
type GameTree = GameTree Sequence (List GameTree)
type alias Sequence = List Node
type alias Node = List Property
type alias Property = (PropIdent, List PropValue)
type alias PropIdent = String
type alias PropValue = String

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
property = wspace *> ((,) <$> propIdent <*> list1 propValue)

propIdent = String.fromList <$> list1 ucLetter
ucLetter = charInStr alphaUpper

propValue = bracket '[' ']' valueText
valueText = String.fromList <$> list0 valueTextChar
valueTextChar =
  char '\\' *> charPred "expected any character" (\_ -> True) <|>
  charPred "expected non-']' character" (\ch -> ch /= ']')

alphaUpper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
