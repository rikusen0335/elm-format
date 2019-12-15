{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module AST.Structure
    ( FixAST(..), FixASTNS, ASTNS
    , ChangeAnnotation, SetAnnotation, convertFix
    , FixedAST(..)
    , cataReferences
    , bottomUpReferences
    , mapNs
    ) where


import AST.Expression (Expression(..), BinopsClause(..), IfClause(..), LetDeclaration(..), CaseBranch(..))
import AST.Pattern (Pattern(..))
import qualified AST.Variable as Var
import Data.Coapplicative
import ElmFormat.Mapping
import AST.V0_16


newtype FixAST t annf typeRef ctorRef varRef =
    FixAST
        { unFixAST ::
            annf
                (t typeRef ctorRef varRef
                    (FixAST Pattern annf typeRef ctorRef varRef)
                    (FixAST Typ annf typeRef ctorRef varRef)
                    (FixAST Expression annf typeRef ctorRef varRef)
                )
        }
deriving instance Show (annf (t typeRef ctorRef varRef (FixAST Pattern annf typeRef ctorRef varRef) (FixAST Typ annf typeRef ctorRef varRef) (FixAST Expression annf typeRef ctorRef varRef))) => Show (FixAST t annf typeRef ctorRef varRef)
deriving instance Eq (annf (t typeRef ctorRef varRef (FixAST Pattern annf typeRef ctorRef varRef) (FixAST Typ annf typeRef ctorRef varRef) (FixAST Expression annf typeRef ctorRef varRef))) => Eq (FixAST t annf typeRef ctorRef varRef)


class FixedAST fix where
    type Seed fix pat typ expr :: *

    cataAll ::
        Functor annf =>
        (annf (Pattern typeRef ctorRef varRef pat typ expr) -> pat)
        -> (annf (Typ typeRef ctorRef varRef pat typ expr) -> typ)
        -> (annf (Expression typeRef ctorRef varRef pat typ expr) -> expr)
        -> fix annf typeRef ctorRef varRef
        -> Seed fix pat typ expr

instance FixedAST (FixAST Pattern) where
    type Seed (FixAST Pattern) pat typ expr = pat
    cataAll fp ft fe =
        fp . fmap (mapAll id id id (cataAll fp ft fe) (cataAll fp ft fe) (cataAll fp ft fe)) . unFixAST

instance FixedAST (FixAST Typ) where
    type Seed (FixAST Typ) pat typ expr = typ
    cataAll fp ft fe =
        ft . fmap (mapAll id id id (cataAll fp ft fe) (cataAll fp ft fe) (cataAll fp ft fe)) . unFixAST

instance FixedAST (FixAST Expression) where
    type Seed (FixAST Expression) pat typ expr = expr
    cataAll fp ft fe =
        fe . fmap (mapAll id id id (cataAll fp ft fe) (cataAll fp ft fe) (cataAll fp ft fe)) . unFixAST


class ChangeAnnotation t ann | t -> ann where
    type SetAnnotation (ann' :: * -> *) t
    convertFix :: (forall x. ann x -> ann' x) -> t -> SetAnnotation ann' t

instance (MapAST t, Functor ann) => ChangeAnnotation (FixAST t ann typeRef ctorRef varRef) ann where
    type SetAnnotation ann' (FixAST t ann typeRef ctorRef varRef) = FixAST t ann' typeRef ctorRef varRef
    convertFix f = FixAST . f . fmap (mapAll id id id (convertFix f) (convertFix f) (convertFix f)) . unFixAST


type FixASTNS t annf ns =
    FixAST t annf (ns, UppercaseIdentifier) (ns, UppercaseIdentifier) (Var.Ref ns)

type ASTNS t annf ns =
    t (ns, UppercaseIdentifier) (ns, UppercaseIdentifier) (Var.Ref ns)
        (FixASTNS Pattern annf ns)
        (FixASTNS Typ annf ns)
        (FixASTNS Expression annf ns)

bottomUpReferences ::
    (Functor annf, FixedAST fix) =>
    (typeRef1 -> typeRef2)
    -> (ctorRef1 -> ctorRef2)
    -> (varRef1 -> varRef2)
    -> fix annf typeRef1 ctorRef1 varRef1
    -> Seed fix
        (FixAST Pattern annf typeRef2 ctorRef2 varRef2)
        (FixAST Typ annf typeRef2 ctorRef2 varRef2)
        (FixAST Expression annf typeRef2 ctorRef2 varRef2)
bottomUpReferences ftr fcr fvr =
    cataAll
        (FixAST . fmap (mapAll ftr fcr fvr id id id))
        (FixAST . fmap (mapAll ftr fcr fvr id id id))
        (FixAST . fmap (mapAll ftr fcr fvr id id id))

cataReferences ::
    (Monoid a, FixedAST fix, Coapplicative annf) =>
    (typeRef -> a) -> (ctorRef -> a) -> (varRef -> a)
    -> fix annf typeRef ctorRef varRef
    -> Seed fix a a a -- NOTE: this should always result in `a`
cataReferences ftype fctor fvar =
    cataAll (foldPattern . extract) (foldType . extract) (foldExpression . extract)
    where
        -- TODO: is there some way to move all these functions into MapAST without unnecessary performance overhead?
        foldPattern = \case
            Anything -> mempty
            UnitPattern _ -> mempty
            AST.Pattern.Literal _ -> mempty
            VarPattern _ -> mempty
            OpPattern _ -> mempty
            Data ctor args -> ftype ctor <> mconcat (fmap extract args)
            PatternParens p -> extract p
            AST.Pattern.Tuple terms -> mconcat (fmap extract terms)
            EmptyListPattern _ -> mempty
            List terms -> mconcat (fmap extract terms)
            ConsPattern first rest -> extract first <> mconcat (fmap extract rest)
            EmptyRecordPattern _ -> mempty
            AST.Pattern.Record _ -> mempty
            Alias p _ -> extract p

        foldTypeConstructor = \case
            NamedConstructor ctor -> fctor ctor
            TupleConstructor _ -> mempty

        foldType = \case
            UnitType _ -> mempty
            TypeVariable _ -> mempty
            TypeConstruction ctor args -> foldTypeConstructor ctor <> mconcat (fmap extract args)
            TypeParens typ -> extract typ
            TupleType terms -> mconcat (fmap extract terms)
            RecordType _ fields _ _ -> mconcat $ fmap (extract . _value . extract) fields
            FunctionType first rest _ -> extract first <> mconcat (fmap extract rest)

        foldBinopsClause = \case
            BinopsClause _ op _ e -> fvar op <> e

        foldIfClause = \case
            IfClause cond els -> extract cond <> extract els

        foldLetDeclaration = \case
            LetDefinition name args _ body -> name <> mconcat (fmap extract args) <> body
            LetAnnotation _ typ -> extract typ
            LetComment _ -> mempty

        foldCaseBranch = \case
            CaseBranch _ _ _ p e -> p <> e

        foldExpression = \case
            Unit _ -> mempty
            AST.Expression.Literal _ -> mempty
            VarExpr var -> fvar var
            App first rest _ -> first <> mconcat (fmap extract rest)
            Unary _ e -> e
            Binops first ops _ -> first <> mconcat (fmap foldBinopsClause ops)
            Parens e -> extract e
            ExplicitList terms _ _ -> mconcat $ fmap extract terms
            Range left right _ -> extract left <> extract right
            AST.Expression.Tuple terms _ -> mconcat $ fmap extract terms
            TupleFunction _ -> mempty
            AST.Expression.Record _ fields _ _ -> mconcat $ fmap (extract . _value . extract) fields
            Access e _ -> e
            AccessFunction _ -> mempty
            Lambda args _ e _ -> mconcat (fmap extract args) <> e
            If cond elsifs els -> foldIfClause cond <> mconcat (fmap (foldIfClause . extract) elsifs) <> extract els
            Let defs _ e -> mconcat (fmap foldLetDeclaration defs) <> e
            Case (cond, _) branches -> extract cond <> mconcat (fmap foldCaseBranch branches)
            GLShader _ -> mempty


mapNs ::
    (MapAST t, Functor annf) =>
    (ns1 -> ns2)
    -> ASTNS t annf ns1
    -> ASTNS t annf ns2
mapNs f =
    let
        mapTypeRef (ns, u) = (f ns, u)
        mapCtorRef (ns, u) = (f ns, u)
        mapVarRef (Var.VarRef ns l) = Var.VarRef (f ns) l
        mapVarRef (Var.TagRef ns u) = Var.TagRef (f ns) u
        mapVarRef (Var.OpRef op) = Var.OpRef op
    in
    mapAll mapTypeRef mapCtorRef mapVarRef
        (bottomUpReferences mapTypeRef mapCtorRef mapVarRef)
        (bottomUpReferences mapTypeRef mapCtorRef mapVarRef)
        (bottomUpReferences mapTypeRef mapCtorRef mapVarRef)
