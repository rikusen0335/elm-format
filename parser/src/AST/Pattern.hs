{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module AST.Pattern where

import AST.V0_16
import ElmFormat.Mapping


data Pattern typeRef ctorRef varRef pat typ expr
    = Anything
    | UnitPattern Comments
    | Literal Literal
    | VarPattern LowercaseIdentifier
    | OpPattern SymbolIdentifier
    | Data typeRef [C1 BeforeTerm pat]
    | PatternParens (C2 Before After pat)
    | Tuple [C2 BeforeTerm AfterTerm pat]
    | EmptyListPattern Comments
    | List [C2 BeforeTerm AfterTerm pat]
    | ConsPattern
        { first :: C0Eol pat
        , rest :: Sequence pat
        }
    | EmptyRecordPattern Comments
    | Record [C2 BeforeTerm AfterTerm LowercaseIdentifier]
    | Alias (C1 After pat) (C1 Before LowercaseIdentifier)
    deriving (Eq, Show)


instance MapAST Pattern where
    mapAll ftref _ _ fpat _ _ = \case
        Anything -> Anything
        UnitPattern c -> UnitPattern c
        Literal l -> Literal l
        VarPattern l -> VarPattern l
        OpPattern s -> OpPattern s
        Data typ pats -> Data (ftref typ) (fmap (fmap fpat) pats)
        PatternParens pat -> PatternParens (fmap fpat pat)
        Tuple pats -> Tuple (fmap (fmap fpat) pats)
        EmptyListPattern c -> EmptyListPattern c
        List pats -> List (fmap (fmap fpat) pats)
        ConsPattern first rest -> ConsPattern (fmap fpat first) (fmap (fmap fpat) rest)
        EmptyRecordPattern c -> EmptyRecordPattern c
        Record fields -> Record fields
        Alias pat name -> Alias (fmap fpat pat) name
