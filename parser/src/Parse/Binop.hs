module Parse.Binop (binops) where

import Text.Parsec ((<|>), choice, try)

import AST.Expression (Expression)
import qualified AST.Expression as E
import AST.Structure (FixAST(..))
import Data.Coapplicative
import Parse.Helpers (commitIf, addLocation, multilineToBool)
import Parse.IParser
import Parse.Whitespace
import Reporting.Annotation (Located)


binops
    :: IParser (FixAST Expression Located typeRef ctorRef varRef)
    -> IParser (FixAST Expression Located typeRef ctorRef varRef)
    -> IParser varRef
    -> IParser (FixAST Expression Located typeRef ctorRef varRef)
binops term last anyOp =
  fmap FixAST $ addLocation $
  do  ((e, ops), multiline) <- trackNewline ((,) <$> term <*> nextOps)
      return $
        case ops of
          [] ->
            extract $ unFixAST e
          _ ->
            E.Binops e ops $ multilineToBool multiline
  where
    nextOps =
      choice
        [ commitIf (whitespace >> anyOp) $
            do  preOpComments <- whitespace
                op <- anyOp
                preExpressionComments <- whitespace
                expr <- Left <$> try term <|> Right <$> last
                case expr of
                  Left t -> (:) (E.BinopsClause preOpComments op preExpressionComments t) <$> nextOps
                  Right e -> return [E.BinopsClause preOpComments op preExpressionComments e]
        , return []
        ]
