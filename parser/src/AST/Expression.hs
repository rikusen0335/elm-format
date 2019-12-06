{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module AST.Expression where

import AST.V0_16
import Data.Bifunctor
import ElmFormat.Mapping

import qualified AST.Variable as Var


---- GENERAL AST ----

data UnaryOperator =
    Negative
    deriving (Eq, Show)


data LetDeclaration typeRef ctorRef varRef pat typ expr
  = LetDefinition pat [C1 Before pat] Comments expr
  | LetAnnotation (C1 After (Var.Ref ())) (C1 Before typ)
  | LetComment Comment
  deriving (Eq, Show)

instance MapAST LetDeclaration where
    mapAll _ _ _ fp ft fe = \case
        LetDefinition first rest c e -> LetDefinition (fp first) (fmap (fmap fp) rest) c (fe e)
        LetAnnotation name typ -> LetAnnotation name (fmap ft typ)
        LetComment c -> LetComment c


data BeforePredicate; data AfterPredicate; data BeforeBody; data AfterBody
data IfClause e =
    IfClause (C2 BeforePredicate AfterPredicate e) (C2 BeforeBody AfterBody e)
    deriving (Eq, Show, Functor)


data BinopsClause varRef expr =
    BinopsClause Comments varRef Comments expr
    deriving (Eq, Show, Functor)

instance Bifunctor BinopsClause where
    bimap fvr fe = \case
        BinopsClause c1 vr c2 e -> BinopsClause c1 (fvr vr) c2 (fe e)


data CaseBranch typeRef ctorRef varRef pat typ expr =
    CaseBranch
        { beforePattern :: Comments
        , beforeArrow :: Comments
        , afterArrow :: Comments
        , pattern :: pat
        , body :: expr
        }
    deriving (Eq, Show)

instance MapAST CaseBranch where
    mapAll _ _ _ fp _ fe = \case
        CaseBranch c1 c2 c3 p e -> CaseBranch c1 c2 c3 (fp p) (fe e)


data BeforePattern; data BeforeArrow; data AfterArrow
data Expression typeRef ctorRef varRef pat typ expr
    = Unit Comments
    | Literal Literal
    | VarExpr varRef

    | App expr [C1 Before expr] FunctionApplicationMultiline
    | Unary UnaryOperator expr
    | Binops expr [BinopsClause varRef expr] Bool
    | Parens (C2 Before After expr)

    | ExplicitList
        { terms :: Sequence expr
        , trailingComments :: Comments
        , forceMultiline :: ForceMultiline
        }
    | Range (C2 Before After expr) (C2 Before After expr) Bool

    | Tuple [C2 Before After expr] Bool
    | TupleFunction Int -- will be 2 or greater, indicating the number of elements in the tuple

    | Record
        { base :: Maybe (C2 Before After LowercaseIdentifier)
        , fields :: Sequence (Pair LowercaseIdentifier expr)
        , trailingComments :: Comments
        , forceMultiline :: ForceMultiline
        }
    | Access expr LowercaseIdentifier
    | AccessFunction LowercaseIdentifier

    | Lambda [C1 Before pat] Comments expr Bool
    | If (IfClause expr) [C1 Before (IfClause expr)] (C1 Before expr)
    | Let [LetDeclaration typeRef ctorRef varRef pat typ expr] Comments expr
    | Case (C2 Before After expr, Bool) [CaseBranch typeRef ctorRef varRef pat typ expr]

    -- for type checking and code gen only
    | GLShader String
    deriving (Eq, Show)


instance MapAST Expression where
    mapAll ftr fcr fvr fp ft fe = \case
        Unit c -> Unit c
        Literal l -> Literal l
        VarExpr var -> VarExpr (fvr var)
        App first rest ml -> App (fe first) (fmap (fmap fe) rest) ml
        Unary op e -> Unary op (fe e)
        Binops first ops ml -> Binops (fe first) (fmap (bimap fvr fe) ops) ml
        Parens e -> Parens (fmap fe e)
        ExplicitList terms c ml -> ExplicitList (fmap (fmap fe) terms) c ml
        Range left right ml -> Range (fmap fe left) (fmap fe right) ml
        Tuple terms ml -> Tuple (fmap (fmap fe) terms) ml
        TupleFunction n -> TupleFunction n
        Record base fields c ml -> Record base (fmap (fmap $ fmap fe) fields) c ml
        Access e field -> Access (fe e) field
        AccessFunction field -> AccessFunction field
        Lambda args c e ml -> Lambda (fmap (fmap fp) args) c (fe e) ml
        If cond elsifs els -> If (fmap fe cond) (fmap (fmap $ fmap fe) elsifs) (fmap fe els)
        Let decls c e -> Let (fmap (mapAll ftr fcr fvr fp ft fe) decls) c (fe e)
        Case (cond, ml) branches -> Case (fmap fe cond, ml) (fmap (mapAll ftr fcr fvr fp ft fe) branches)
        GLShader s -> GLShader s
