module Parse.Expression (term, typeAnnotation, definition, expr) where

import Data.Maybe (fromMaybe)
import Text.Parsec hiding (newline, spaces)
import Text.Parsec.Indent (block, withPos, checkIndent)

import qualified Parse.Binop as Binop
import Parse.Helpers
import Parse.Common
import qualified Parse.Helpers as Help
import qualified Parse.Literal as Literal
import qualified Parse.Pattern as Pattern
import qualified Parse.Type as Type
import Parse.IParser
import Parse.Whitespace

import AST.V0_16
import AST.Expression
import qualified AST.Expression as E
import AST.Pattern (Pattern)
import qualified AST.Pattern as P
import AST.Structure
import qualified AST.Variable as Var
import ElmVersion
import Reporting.Annotation (Located)
import qualified Reporting.Annotation as A


--------  Basic Terms  --------

varTerm :: ElmVersion -> IParser (Expression typeRef ctorRef (Var.Ref [UppercaseIdentifier]) pat typ expr)
varTerm elmVersion =
    let
        resolve v =
            case v of
                Var.TagRef [] (UppercaseIdentifier "True") -> E.Literal $ Boolean True
                Var.TagRef [] (UppercaseIdentifier "False") -> E.Literal $ Boolean False
                _ -> E.VarExpr v
    in
        resolve <$> var elmVersion


accessor :: ElmVersion -> IParser (Expression typeRef ctorRef varRef pat typ expr)
accessor elmVersion =
  do  lbl <- try (string "." >> rLabel elmVersion)
      return $ E.AccessFunction lbl


negative :: ElmVersion -> IParser (Expression typeRef ctorRef varRef pat typ (FixASTNS Expression Located [UppercaseIdentifier]))
negative elmVersion =
  do  nTerm <-
          try $
            do  _ <- char '-'
                notFollowedBy (char '.' <|> char '-')
                term elmVersion

      return $ E.Unary E.Negative nTerm


--------  Complex Terms  --------

listTerm :: ElmVersion -> IParser (FixASTNS Expression Located [UppercaseIdentifier])
listTerm elmVersion =
  fmap FixAST $ addLocation $
    shader' <|> try (braces range) <|> commaSeparated
  where
    range =
      do
          lo <- expr elmVersion
          (C (loPost, hiPre) _) <- padded (string "..")
          hi <- expr elmVersion
          return $ \loPre hiPost multiline ->
              E.Range
                  (C (loPre, loPost) lo)
                  (C (hiPre, hiPost) hi)
                  multiline

    shader' =
      do  rawSrc <- Help.shader
          return $ E.GLShader (filter (/='\r') rawSrc)

    commaSeparated =
        braces' $ checkMultiline $
        do
            (terms, trailing) <- sectionedGroup (expr elmVersion)
            return $ E.ExplicitList terms trailing


parensTerm :: ElmVersion -> IParser (FixASTNS Expression Located [UppercaseIdentifier])
parensTerm elmVersion =
  fmap FixAST $
  choice
    [ try (addLocation $ parens' opFn )
    , try (addLocation $ parens' tupleFn)
    , do
          (start, e, end) <- located $ parens (parened <|> unit)
          return $ A.at start end e
    ]
  where
    opFn =
      E.VarExpr <$> anyOp elmVersion

    tupleFn =
      do  commas <- many1 comma
          return $ E.TupleFunction (length commas + 1)

    parened =
      do  expressions <- commaSep1 ((\e a b -> C (a, b) e) <$> expr elmVersion)
          return $ \pre post multiline ->
            case expressions pre post of
              [single] ->
                  E.Parens single

              expressions' ->
                  E.Tuple expressions' multiline

    unit =
        return $ \pre post _ -> E.Unit (pre ++ post)


recordTerm :: ElmVersion -> IParser (FixASTNS Expression Located [UppercaseIdentifier])
recordTerm elmVersion =
    fmap FixAST $
    addLocation $ brackets' $ checkMultiline $
        do
            base <- optionMaybe $ try (commented (lowVar elmVersion) <* string "|")
            (fields, trailing) <- sectionedGroup (pair (lowVar elmVersion) lenientEquals (expr elmVersion))
            return $ E.Record base fields trailing


term :: ElmVersion -> IParser (FixASTNS Expression Located [UppercaseIdentifier])
term elmVersion =
  (choice [ (fmap FixAST . addLocation) (E.Literal <$> Literal.literal), listTerm elmVersion, (fmap FixAST . addLocation) (accessor elmVersion), (fmap FixAST . addLocation) (negative elmVersion) ])
    <|> accessible elmVersion ((fmap FixAST . addLocation) (varTerm elmVersion) <|> parensTerm elmVersion <|> recordTerm elmVersion)
    <?> "an expression"


--------  Applications  --------

head' :: [a] -> Maybe a
head' [] = Nothing
head' (a:_) = Just a


appExpr :: ElmVersion -> IParser (FixASTNS Expression Located [UppercaseIdentifier])
appExpr elmVersion =
  expecting "an expression" $
  do  start <- getMyPosition
      (t, initialTermMultiline) <- trackNewline (term elmVersion)
      ts <- constrainedSpacePrefix (term elmVersion)
      end <- getMyPosition
      return $
          case ts of
            [] ->
              t
            _  ->
                let
                    multiline =
                        case
                            ( initialTermMultiline
                            , fromMaybe (JoinAll) $ fmap snd $ head' ts
                            , any (isMultiline . snd) $ tail ts
                            )
                        of
                            (SplitAll, _, _ ) -> FASplitFirst
                            (JoinAll, JoinAll, True) -> FAJoinFirst SplitAll
                            (JoinAll, JoinAll, False) -> FAJoinFirst JoinAll
                            (JoinAll, SplitAll, _) -> FASplitFirst
                in
                    FixAST $ A.at start end $ E.App t (fmap fst ts) multiline


--------  Normal Expressions  --------

expr :: ElmVersion -> IParser (FixASTNS Expression Located [UppercaseIdentifier])
expr elmVersion =
  choice [ letExpr elmVersion, caseExpr elmVersion, ifExpr elmVersion ]
    <|> lambdaExpr elmVersion
    <|> binaryExpr elmVersion
    <?> "an expression"


binaryExpr :: ElmVersion -> IParser (FixASTNS Expression Located [UppercaseIdentifier])
binaryExpr elmVersion =
    Binop.binops (appExpr elmVersion) lastExpr (anyOp elmVersion)
  where
    lastExpr =
        choice [ letExpr elmVersion, caseExpr elmVersion, ifExpr elmVersion ]
        <|> lambdaExpr elmVersion
        <?> "an expression"


ifExpr :: ElmVersion -> IParser (FixASTNS Expression Located [UppercaseIdentifier])
ifExpr elmVersion =
  let
    elseKeyword =
      (reserved elmVersion "else" <?> "an 'else' branch")
        >> whitespace
  in
  fmap FixAST $ addLocation $
    do
      first <- ifClause elmVersion
      rest <- many (try $ C <$> elseKeyword <*> ifClause elmVersion)
      final <- C <$> elseKeyword <*> expr elmVersion

      return $ E.If first rest final


ifClause :: ElmVersion -> IParser (E.IfClause (FixASTNS Expression Located [UppercaseIdentifier]))
ifClause elmVersion =
  do
    try (reserved elmVersion "if")
    preCondition <- whitespace
    condition <- expr elmVersion
    (C (postCondition, bodyComments) _) <- padded (reserved elmVersion "then")
    thenBranch <- expr elmVersion
    preElse <- whitespace <?> "an 'else' branch"
    return $ E.IfClause
      (C (preCondition, postCondition) condition)
      (C (bodyComments, preElse) thenBranch)


lambdaExpr :: ElmVersion -> IParser (FixASTNS Expression Located [UppercaseIdentifier])
lambdaExpr elmVersion =
  let
    subparser = do
      _ <- char '\\' <|> char '\x03BB' <?> "an anonymous function"
      args <- spacePrefix (Pattern.term elmVersion)
      (C (preArrowComments, bodyComments) _) <- padded rightArrow
      body <- expr elmVersion
      return (args, preArrowComments, bodyComments, body)
  in
    fmap FixAST $ addLocation $
        do  ((args, preArrowComments, bodyComments, body), multiline) <- trackNewline subparser
            return $ E.Lambda args (preArrowComments ++ bodyComments) body $ multilineToBool multiline


caseExpr :: ElmVersion -> IParser (FixASTNS Expression Located [UppercaseIdentifier])
caseExpr elmVersion =
  fmap FixAST $ addLocation $
  do  try (reserved elmVersion "case")
      (e, multilineSubject) <- trackNewline $ padded (expr elmVersion)
      reserved elmVersion "of"
      firstPatternComments <- whitespace
      result <- cases firstPatternComments
      return $ E.Case (e, multilineToBool multilineSubject) result
  where
    case_ preComments =
      do
          (patternComments, p, C (preArrowComments, bodyComments) _) <-
              try ((,,)
                  <$> whitespace
                  <*> (checkIndent >> Pattern.expr elmVersion)
                  <*> padded rightArrow
                  )
          result <- expr elmVersion
          return $ CaseBranch
              { beforePattern = preComments ++ patternComments
              , beforeArrow = preArrowComments
              , afterArrow = bodyComments
              , pattern = p
              , body = result
              }

    cases preComments =
        withPos $
            do
                r1 <- case_ preComments
                r <- many $ case_ []
                return $ r1:r



-- LET


letExpr :: ElmVersion -> IParser (FixASTNS Expression Located [UppercaseIdentifier])
letExpr elmVersion =
  fmap FixAST $ addLocation $
  do  try (reserved elmVersion "let")
      commentsAfterLet <- map E.LetComment <$> whitespace
      defs <-
        block $
          do  def <- typeAnnotation elmVersion LetAnnotation <|> definition elmVersion LetDefinition
              commentsAfterDef <- whitespace
              return $ def : (map E.LetComment commentsAfterDef)
      _ <- reserved elmVersion "in"
      bodyComments <- whitespace
      E.Let (commentsAfterLet ++ concat defs) bodyComments <$> expr elmVersion



-- TYPE ANNOTATION

typeAnnotation :: ElmVersion -> (C1 after (Var.Ref ()) -> C1 before (FixAST Typ Located typeRef ([UppercaseIdentifier], UppercaseIdentifier) varRef) -> a) -> IParser a
typeAnnotation elmVersion fn =
    (\(v, pre, post) e -> fn (C pre v) (C post e)) <$> try start <*> Type.expr elmVersion
  where
    start =
      do  v <- (Var.VarRef () <$> lowVar elmVersion) <|> (Var.OpRef <$> symOpInParens)
          (C (preColon, postColon) _) <- padded hasType
          return (v, preColon, postColon)


-- DEFINITION

definition ::
    ElmVersion
    ->
        (FixASTNS Pattern Located [UppercaseIdentifier]
          -> [C1 before (FixASTNS Pattern Located [UppercaseIdentifier])]
          -> Comments
          -> (FixASTNS Expression Located [UppercaseIdentifier])
          -> a
        )
    -> IParser a
definition elmVersion fn =
  withPos $
    do
        (name, args) <- defStart elmVersion
        (C (preEqualsComments, postEqualsComments) _) <- padded equals
        body <- expr elmVersion
        return $ fn name args (preEqualsComments ++ postEqualsComments) body


defStart :: ElmVersion -> IParser (FixASTNS Pattern Located [UppercaseIdentifier], [C1 before (FixASTNS Pattern Located [UppercaseIdentifier])])
defStart elmVersion =
    choice
      [ do  pattern <- try $ Pattern.term elmVersion
            func $ pattern
      , do  opPattern <- fmap FixAST $ addLocation (P.OpPattern <$> parens' symOp)
            func opPattern
      ]
      <?> "the definition of a variable (x = ...)"
  where
    func pattern =
        case unFixAST pattern of
          A.A _ (P.VarPattern _) ->
              ((,) pattern) <$> spacePrefix (Pattern.term elmVersion)

          A.A _ (P.OpPattern _) ->
              ((,) pattern) <$> spacePrefix (Pattern.term elmVersion)

          _ ->
              return (pattern, [])
