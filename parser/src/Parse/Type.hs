module Parse.Type where

import Text.Parsec ((<|>), (<?>), char, many1, string, try, optionMaybe)

import Parse.Helpers
import Reporting.Annotation (Located)
import qualified Reporting.Annotation as A
import AST.V0_16
import AST.Structure (FixAST(..))
import Data.Coapplicative
import ElmVersion
import Parse.IParser
import Parse.Common
import Data.Maybe (maybeToList)


tvar :: ElmVersion -> IParser (FixAST Typ Located typeRef ctorRef varRef)
tvar elmVersion =
  fmap FixAST $ addLocation
    (TypeVariable <$> lowVar elmVersion <?> "a type variable")


tuple :: ElmVersion -> IParser (FixAST Typ Located typeRef ([UppercaseIdentifier], UppercaseIdentifier) varRef)
tuple elmVersion =
  fmap FixAST $ addLocation $
  do  types <- parens'' (withEol $ expr elmVersion)
      return $
          case types of
              Left comments ->
                  UnitType comments
              Right [] ->
                  UnitType []
              Right [C ([], []) (C Nothing t)] ->
                  extract $ unFixAST t
              Right [C (pre, post) (C eol t)] ->
                  TypeParens $ C (pre, maybeToList (fmap LineComment eol) ++ post) t
              Right types' ->
                  TupleType $ fmap (\(C (pre, post) (C eol t)) -> C (pre, post, eol) t) types'


record :: ElmVersion -> IParser (FixAST Typ Located typeRef ([UppercaseIdentifier], UppercaseIdentifier) varRef)
record elmVersion =
    fmap FixAST $ addLocation $ brackets' $ checkMultiline $
        do
            base' <- optionMaybe $ try (commented (lowVar elmVersion) <* string "|")
            (fields', trailing) <- sectionedGroup (pair (lowVar elmVersion) lenientHasType (expr elmVersion))
            return $ RecordType base' fields' trailing


capTypeVar :: ElmVersion -> IParser [UppercaseIdentifier]
capTypeVar elmVersion =
    dotSep1 (capVar elmVersion)


constructor0 :: ElmVersion -> IParser (TypeConstructor ([UppercaseIdentifier], UppercaseIdentifier))
constructor0 elmVersion =
  do  name <- capTypeVar elmVersion
      case reverse name of
        [] -> error "Impossible empty TypeConstructor name"
        last':rest' ->
            return (NamedConstructor (reverse rest', last'))


constructor0' :: ElmVersion -> IParser (FixAST Typ Located typeRef ([UppercaseIdentifier], UppercaseIdentifier) varRef)
constructor0' elmVersion =
    fmap FixAST $ addLocation $
    do  ctor <- constructor0 elmVersion
        return (TypeConstruction ctor [])


term :: ElmVersion -> IParser (FixAST Typ Located typeRef ([UppercaseIdentifier], UppercaseIdentifier) varRef)
term elmVersion =
  tuple elmVersion <|> record elmVersion <|> tvar elmVersion <|> constructor0' elmVersion


tupleCtor :: IParser (TypeConstructor ns)
tupleCtor =
    do  ctor <- parens' (many1 (char ','))
        return (TupleConstructor (length ctor + 1))


app :: ElmVersion -> IParser (FixAST Typ Located typeRef ([UppercaseIdentifier], UppercaseIdentifier) varRef)
app elmVersion =
  fmap FixAST $ addLocation $
  do  f <- constructor0 elmVersion <|> try tupleCtor <?> "a type constructor"
      args <- spacePrefix (term elmVersion)
      return $ TypeConstruction f args


expr :: ElmVersion -> IParser (FixAST Typ Located typeRef ([UppercaseIdentifier], UppercaseIdentifier) varRef)
expr elmVersion =
  do
    result <- separated rightArrow (app elmVersion <|> term elmVersion)
    return $
      case result of
        Left t ->
          t
        Right (region, first', rest', multiline) ->
          FixAST $ A.A region $ FunctionType first' rest' (ForceMultiline multiline)


constructor :: ElmVersion -> IParser ([UppercaseIdentifier], [C1 before (FixAST Typ Located typeRef ([UppercaseIdentifier], UppercaseIdentifier) varRef)])
constructor elmVersion =
  (,) <$> (capTypeVar elmVersion<?> "another type constructor")
      <*> spacePrefix (term elmVersion)


tag :: ElmVersion -> IParser (UppercaseIdentifier, [C1 before (FixAST Typ Located typeRef ([UppercaseIdentifier], UppercaseIdentifier) varRef)])
tag elmVersion =
  (,) <$> (capVar elmVersion <?> "another type constructor")
      <*> spacePrefix (term elmVersion)
