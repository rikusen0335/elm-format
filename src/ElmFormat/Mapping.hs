{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module ElmFormat.Mapping where

import AST.V0_16


class MapAST t where
    mapAll ::
        (typeRef1 -> typeRef2) -> (ctorRef1 -> ctorRef2) -> (varRef1 -> varRef2)
        -> (pat1 -> pat2) -> (typ1 -> typ2) -> (expr1 -> expr2)
        -> t typeRef1 ctorRef1 varRef1 pat1 typ1 expr1
        -> t typeRef2 ctorRef2 varRef2 pat2 typ2 expr2

    mapTyp :: (a -> b) -> t typeRef ctorRef varRef pat a expr -> t typeRef ctorRef varRef pat b expr
    mapTyp f = mapAll id id id id f id


instance MapAST Typ where
    mapAll _ fctor _ _ fTyp _ = \case
        UnitType c -> UnitType c
        TypeVariable name -> TypeVariable name
        TypeConstruction ctor args -> TypeConstruction (fmap fctor ctor) (fmap (fmap fTyp) args)
        TypeParens typ -> TypeParens (fmap fTyp typ)
        TupleType typs -> TupleType (fmap (fmap fTyp) typs)
        RecordType base fields c ml -> RecordType base (fmap (fmap $ fmap fTyp) fields) c ml
        FunctionType first rest ml -> FunctionType (fmap fTyp first) (fmap (fmap fTyp) rest) ml
