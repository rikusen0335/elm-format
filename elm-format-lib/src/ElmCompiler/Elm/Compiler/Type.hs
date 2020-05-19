{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module ElmCompiler.Elm.Compiler.Type
  ( Type(..)
  , RT.Context(..)
  , toDoc
  , DebugMetadata(..)
  , Alias(..)
  , Union(..)
  , encode
  , decoder
  , encodeMetadata
  )
  where


import qualified ElmCompiler.Data.Name as Name

import qualified ElmCompiler.AST.Source as Src
import qualified ElmCompiler.Json.Decode as D
import qualified ElmCompiler.Json.Encode as E
import ElmCompiler.Json.Encode ((==>))
import qualified ElmCompiler.Json.String as Json
import qualified ElmCompiler.Parse.Primitives as P
import qualified ElmCompiler.Parse.Type as Type
import qualified ElmCompiler.Reporting.Annotation as A
import qualified ElmCompiler.Reporting.Doc as D
import qualified ElmCompiler.Reporting.Render.Type as RT
import qualified ElmCompiler.Reporting.Render.Type.Localizer as L



-- TYPES


data Type
  = Lambda Type Type
  | Var Name.Name
  | Type Name.Name [Type]
  | Record [(Name.Name, Type)] (Maybe Name.Name)
  | Unit
  | Tuple Type Type [Type]


data DebugMetadata =
  DebugMetadata
    { _message :: Type
    , _aliases :: [Alias]
    , _unions :: [Union]
    }


data Alias = Alias Name.Name [Name.Name] Type
data Union = Union Name.Name [Name.Name] [(Name.Name, [Type])]



-- TO DOC


toDoc :: L.Localizer -> RT.Context -> Type -> D.Doc
toDoc localizer context tipe =
  case tipe of
    Lambda _ _ ->
      let
        a:b:cs =
          map (toDoc localizer RT.Func) (collectLambdas tipe)
      in
      RT.lambda context a b cs

    Var name ->
      D.fromName name

    Unit ->
      "()"

    Tuple a b cs ->
      RT.tuple
        (toDoc localizer RT.None a)
        (toDoc localizer RT.None b)
        (map (toDoc localizer RT.None) cs)

    Type name args ->
      RT.apply
        context
        (D.fromName name)
        (map (toDoc localizer RT.App) args)

    Record fields ext ->
      RT.record
        (map (entryToDoc localizer) fields)
        (fmap D.fromName ext)


entryToDoc :: L.Localizer -> (Name.Name, Type) -> (D.Doc, D.Doc)
entryToDoc localizer (field, fieldType) =
  ( D.fromName field, toDoc localizer RT.None fieldType )


collectLambdas :: Type -> [Type]
collectLambdas tipe =
  case tipe of
    Lambda arg body ->
      arg : collectLambdas body

    _ ->
      [tipe]



-- JSON for TYPE


encode :: Type -> E.Value
encode tipe =
  E.chars $ D.toLine (toDoc L.empty RT.None tipe)


decoder :: D.Decoder () Type
decoder =
  let
    parser =
      P.specialize (\_ _ _ -> ()) (fromRawType . fst <$> Type.expression)
  in
  D.customString parser (\_ _ -> ())


fromRawType :: Src.Type -> Type
fromRawType (A.At _ astType) =
  case astType of
    Src.TLambda t1 t2 ->
        Lambda (fromRawType t1) (fromRawType t2)

    Src.TVar x ->
        Var x

    Src.TUnit ->
        Unit

    Src.TTuple a b cs ->
        Tuple
          (fromRawType a)
          (fromRawType b)
          (map fromRawType cs)

    Src.TType _ name args ->
        Type name (map fromRawType args)

    Src.TTypeQual _ _ name args ->
        Type name (map fromRawType args)

    Src.TRecord fields ext ->
        let fromField (A.At _ field, tipe) = (field, fromRawType tipe) in
        Record
          (map fromField fields)
          (fmap A.toValue ext)



-- JSON for PROGRAM


encodeMetadata :: DebugMetadata -> E.Value
encodeMetadata (DebugMetadata msg aliases unions) =
  E.object
    [ "message" ==> encode msg
    , "aliases" ==> E.object (map toTypeAliasField aliases)
    , "unions" ==> E.object (map toCustomTypeField unions)
    ]


toTypeAliasField :: Alias -> ( Json.String, E.Value )
toTypeAliasField (Alias name args tipe) =
  ( Json.fromName name
  , E.object
      [ "args" ==> E.list E.name args
      , "type" ==> encode tipe
      ]
  )


toCustomTypeField :: Union -> ( Json.String, E.Value )
toCustomTypeField (Union name args constructors) =
  ( Json.fromName name
  , E.object
      [ "args" ==> E.list E.name args
      , "tags" ==> E.object (map toVariantObject constructors)
      ]
  )


toVariantObject :: (Name.Name, [Type]) -> ( Json.String, E.Value )
toVariantObject (name, args) =
  ( Json.fromName name, E.list encode args )
