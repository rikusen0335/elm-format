module Parse.Pattern (term, expr) where

import Text.Parsec ((<|>), (<?>), char, choice, optionMaybe, try)

import AST.V0_16
import AST.Pattern (Pattern)
import AST.Structure (FixAST(..))
import qualified AST.Pattern as P
import ElmVersion
import Parse.Helpers
import qualified Parse.Literal as Literal
import Reporting.Annotation (Located)
import qualified Reporting.Annotation as A
import Parse.IParser
import Parse.Whitespace


basic :: ElmVersion -> IParser (FixAST Pattern Located ([UppercaseIdentifier], UppercaseIdentifier) ctorRef varRef)
basic elmVersion =
  fmap FixAST $ addLocation $
    choice
      [ char '_' >> return P.Anything
      , P.VarPattern <$> lowVar elmVersion
      , chunksToPattern <$> dotSep1 (capVar elmVersion)
      , P.Literal <$> Literal.literal
      ]
  where
    chunksToPattern chunks =
        case reverse chunks of
          [UppercaseIdentifier "True"] ->
              P.Literal (Boolean True)

          [UppercaseIdentifier "False"] ->
              P.Literal (Boolean False)

          (last:rest) ->
              P.Data (reverse rest, last) []

          [] -> error "dotSep1 returned empty list"


asPattern :: ElmVersion -> IParser (FixAST Pattern Located typeRef ctorRef varRef) -> IParser (FixAST Pattern Located typeRef ctorRef varRef)
asPattern elmVersion patternParser =
  do  (start, pattern, _) <- located patternParser

      maybeAlias <- optionMaybe asAlias

      case maybeAlias of
        Just (postPattern, alias) ->
            do  end <- getMyPosition
                return $ FixAST $ A.at start end $ P.Alias (C postPattern pattern) alias

        Nothing ->
            return pattern
  where
    asAlias =
      do  preAs <- try (whitespace <* reserved elmVersion "as")
          postAs <- whitespace
          var <- lowVar elmVersion
          return (preAs, C postAs var)


record :: ElmVersion -> IParser (FixAST Pattern Located typeRef ctorRef varRef)
record elmVersion =
  fmap FixAST $ addLocation $
  do
      result <- surround'' '{' '}' (lowVar elmVersion)
      return $
          case result of
              Left comments ->
                  P.EmptyRecordPattern comments
              Right fields ->
                  P.Record fields


tuple :: ElmVersion -> IParser (FixAST Pattern Located ([UppercaseIdentifier], UppercaseIdentifier) ctorRef varRef)
tuple elmVersion =
  do  (start, patterns, end) <- located $ parens'' (expr elmVersion)

      return $
        case patterns of
          Left comments ->
            FixAST $ A.at start end $ P.UnitPattern comments

          Right [] ->
            FixAST $ A.at start end $ P.UnitPattern []

          Right [C ([], []) pattern] ->
            pattern

          Right [pattern] ->
            FixAST $ A.at start end $ P.PatternParens pattern

          Right patterns ->
            FixAST $ A.at start end $ P.Tuple patterns


list :: ElmVersion -> IParser (FixAST Pattern Located ([UppercaseIdentifier], UppercaseIdentifier) ctorRef varRef)
list elmVersion =
  fmap FixAST $ addLocation $
  do
    result <- braces'' (expr elmVersion)
    return $
      case result of
        Left comments ->
          P.EmptyListPattern comments
        Right patterns ->
          P.List patterns


term :: ElmVersion -> IParser (FixAST Pattern Located ([UppercaseIdentifier], UppercaseIdentifier) ctorRef varRef)
term elmVersion =
  choice [ record elmVersion, tuple elmVersion, list elmVersion, basic elmVersion ]
    <?> "a pattern"


patternConstructor :: ElmVersion -> IParser (FixAST Pattern Located ([UppercaseIdentifier], UppercaseIdentifier) ctorRef varRef)
patternConstructor elmVersion =
  fmap FixAST $ addLocation $
    do  v <- dotSep1 (capVar elmVersion)
        case reverse v of
          [UppercaseIdentifier "True"]  -> return $ P.Literal (Boolean True)
          [UppercaseIdentifier "False"] -> return $ P.Literal (Boolean False)
          (last:rest) -> P.Data (reverse rest, last) <$> spacePrefix (term elmVersion)
          [] -> error "dotSep1 returned empty list"


expr :: ElmVersion -> IParser (FixAST Pattern Located ([UppercaseIdentifier], UppercaseIdentifier) ctorRef varRef)
expr elmVersion =
    asPattern elmVersion subPattern <?> "a pattern"
  where
    subPattern =
      do
        result <- separated cons (patternConstructor elmVersion <|> term elmVersion)
        return $
          case result of
            Left pattern ->
              pattern
            Right (region, first, rest, _) ->
              FixAST $ A.A region $ P.ConsPattern first rest
