{-# OPTIONS_GHC -Wall #-}
module AST.Pattern where

import qualified Data.Set as Set
import Text.PrettyPrint as P

import qualified AST.Helpers as Help
import qualified AST.Literal as L
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Region as R


type Pattern =
    A.Annotated R.Region Pattern'


data Pattern'
    = Data Var.Var [Pattern]
    | Record [String]
    | Alias String Pattern
    | Var String
    | Anything
    | Literal L.Literal
    deriving (Show)


list :: R.Position -> [Pattern] -> Pattern
list end patterns =
  case patterns of
    [] ->
        A.at end end (Data (Var.Var "[]") [])

    pattern@(A.A (R.Region start _) _) : rest ->
        A.at start end (Data (Var.Var "::") [pattern, list end rest])


consMany :: R.Position -> [Pattern] -> Pattern
consMany end patterns =
  let cons hd@(A.A (R.Region start _) _) tl =
          A.at start end (Data (Var.Var "::") [hd, tl])
  in
      foldr1 cons patterns


tuple :: [Pattern] -> Pattern'
tuple patterns =
  Data (Var.Var ("_Tuple" ++ show (length patterns))) patterns


-- FIND VARIABLES

boundVars :: Pattern -> [A.Annotated R.Region String]
boundVars (A.A ann pattern) =
  case pattern of
    Var x ->
        [A.A ann x]

    Alias name realPattern ->
        A.A ann name : boundVars realPattern

    Data _ patterns ->
        concatMap boundVars patterns

    Record fields ->
        map (A.A ann) fields

    Anything ->
        []

    Literal _ ->
        []


member :: String -> Pattern -> Bool
member name pattern =
  any (name==) (map A.drop (boundVars pattern))


boundVarSet :: Pattern -> Set.Set String
boundVarSet pattern =
  Set.fromList (map A.drop (boundVars pattern))


boundVarList :: Pattern -> [String]
boundVarList pattern =
  Set.toList (boundVarSet pattern)


-- PRETTY PRINTING

instance P.Pretty Pattern' where
  pretty dealiaser needsParens pattern =
    case pattern of
      Var name ->
          P.text name

      Literal literal ->
          P.pretty dealiaser needsParens literal

      Record fields ->
          P.braces (P.commaCat (map P.text fields))

      Alias x ptrn ->
          P.parensIf needsParens $
              P.pretty dealiaser True ptrn <+> P.text "as" <+> P.text x

      Anything ->
          P.text "_"

      Data name [A.A _ hd, A.A _ tl]
          | Var.toString name == "::" ->
              P.parensIf isCons (P.pretty dealiaser False hd)
              <+> P.text "::"
              <+> P.pretty dealiaser False tl
          where
            isCons =
              case hd of
                Data ctor _ -> Var.toString ctor == "::"
                _ -> False

      Data name patterns ->
          let name' = Var.toString name
          in
            if Help.isTuple name'
              then
                P.parens (P.commaCat (map (P.pretty dealiaser False) patterns))
              else
                P.parensIf needsParens $
                    P.hsep (P.text name' : map (P.pretty dealiaser True) patterns)