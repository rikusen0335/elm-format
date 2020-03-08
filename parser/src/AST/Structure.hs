{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AST.Structure
    ( FixAST, ASTNS
    , foldReferences
    , bottomUpReferences
    , mapNs
    ) where


import Data.Coapplicative
import Data.Foldable (fold)
import AST.V0_16
import qualified Data.Indexed as I


type FixAST annf typeRef ctorRef varRef (kind :: NodeKind) =
    I.Fix annf (AST typeRef ctorRef varRef) kind


type ASTNS annf ns (kind :: NodeKind) =
    FixAST annf (ns, UppercaseIdentifier) (ns, UppercaseIdentifier) (Ref ns) kind


bottomUpReferences ::
    (Functor annf) =>
    (typeRef1 -> typeRef2)
    -> (ctorRef1 -> ctorRef2)
    -> (varRef1 -> varRef2)
    -> (forall kind.
        FixAST annf typeRef1 ctorRef1 varRef1 kind
        -> FixAST annf typeRef2 ctorRef2 varRef2 kind
       )
bottomUpReferences ftr fcr fvr =
    I.cata (I.Fix . fmap (mapAll ftr fcr fvr id))


-- TODO: move this Data.Indexed?
newtype Const x y = Const { unConst :: x }
-- TODO: probably best to remove this monoid instance so unwrapping can be more explicit
instance Semigroup x => Semigroup (Const x y) where
    (Const a) <> (Const b) = Const (a <> b)
instance Monoid x => Monoid (Const x y) where
    mempty = Const mempty


foldReferences ::
    forall a annf typeRef ctorRef varRef kind.
    (Monoid a, Coapplicative annf) =>
    (typeRef -> a) -> (ctorRef -> a) -> (varRef -> a)
    -> FixAST annf typeRef ctorRef varRef kind -> a
foldReferences ftype fctor fvar =
    unConst . I.cata (foldNode  . extract)
    where
        -- This is kinda confusing, but we use the Const type constructor to merge all the different NodeKinds into a single type `a`
        -- See http://www.timphilipwilliams.com/posts/2013-01-16-fixing-gadts.html for relevant details.
        foldNode :: forall kind'. AST typeRef ctorRef varRef (Const a) kind' -> Const a kind'
        foldNode = \case
            -- Declarations
            Definition name args _ e -> Const (unConst name <> foldMap (unConst . extract) args <> unConst e)
            TypeAnnotation _ t -> Const (unConst $ extract t)
            Datatype _ ctors -> Const (foldMap (unConst . fold) ctors)
            TypeAlias _ _ t -> Const (unConst $ extract t)
            PortAnnotation _ _ t -> Const (unConst t)
            PortDefinition _ _ e -> Const (unConst e)
            Fixity _ _ _ _ name -> Const (fvar name)
            Fixity_0_19 _ _ _ _ -> mempty

            -- Expressions
            Unit _ -> mempty
            Literal _ -> mempty
            VarExpr var -> Const $ fvar var
            App first rest _ -> first <> mconcat (fmap extract rest)
            Unary _ e -> e
            Binops first ops _ -> Const (unConst first <> foldMap foldBinopsClause ops)
            Parens e -> extract e
            ExplicitList terms _ _ -> fold terms
            Range left right _ -> extract left <> extract right
            Tuple terms _ -> mconcat $ fmap extract terms
            TupleFunction _ -> mempty
            Record _ fields _ _ -> foldMap (extract . _value) fields
            Access e _ -> e
            AccessFunction _ -> mempty
            Lambda args _ e _ -> Const (foldMap (unConst . extract) args <> unConst e)
            If cond elsifs els -> Const (foldIfClause cond <> foldMap (foldIfClause . extract) elsifs <> unConst (extract els))
            Let defs _ e -> Const (foldMap unConst defs <> unConst e)
            LetDefinition name args _ body -> Const (unConst name <> foldMap (unConst . extract) args <> unConst body)
            LetAnnotation _ typ -> Const (unConst $ extract typ)
            LetComment _ -> mempty
            Case (cond, _) branches -> Const (unConst (extract cond) <> foldMap unConst branches)
            CaseBranch _ _ _ p e -> Const (unConst p <> unConst e)
            GLShader _ -> mempty

            -- Patterns
            Anything -> mempty
            UnitPattern _ -> mempty
            LiteralPattern _ -> mempty
            VarPattern _ -> mempty
            OpPattern _ -> mempty
            DataPattern ctor args -> Const (ftype ctor <> foldMap (unConst . extract) args)
            PatternParens p -> extract p
            TuplePattern terms -> foldMap extract terms
            EmptyListPattern _ -> mempty
            ListPattern terms -> foldMap extract terms
            ConsPattern first rest -> extract first <> fold rest
            EmptyRecordPattern _ -> mempty
            RecordPattern _ -> mempty
            Alias p _ -> extract p

            -- Types
            UnitType _ -> mempty
            TypeVariable _ -> mempty
            TypeConstruction ctor args -> Const (foldTypeConstructor ctor <> foldMap (unConst . extract) args)
            TypeParens typ -> extract typ
            TupleType terms -> foldMap extract terms
            RecordType _ fields _ _ -> foldMap (extract . _value) fields
            FunctionType first rest _ -> extract first <> fold rest

        foldTypeConstructor :: TypeConstructor ctorRef -> a
        foldTypeConstructor = \case
            NamedConstructor ctor -> fctor ctor
            TupleConstructor _ -> mempty

        foldBinopsClause :: BinopsClause varRef (Const a 'ExpressionNK) -> a
        foldBinopsClause = \case
            BinopsClause _ op _ e -> fvar op <> unConst e

        foldIfClause :: IfClause (Const a 'ExpressionNK) -> a
        foldIfClause = \case
            IfClause cond els -> unConst (extract cond) <> unConst (extract els)


mapNs ::
    Functor annf =>
    (ns1 -> ns2)
    -> (forall kind.
        ASTNS annf ns1 kind
        -> ASTNS annf ns2 kind
       )
mapNs f =
    let
        mapTypeRef (ns, u) = (f ns, u)
        mapCtorRef (ns, u) = (f ns, u)
        mapVarRef (VarRef ns l) = VarRef (f ns) l
        mapVarRef (TagRef ns u) = TagRef (f ns) u
        mapVarRef (OpRef op) = OpRef op
    in
    bottomUpReferences mapTypeRef mapCtorRef mapVarRef
