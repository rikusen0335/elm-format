module AST.MatchReferences (MatchedNamespace(..), fromMatched, matchReferences, applyReferences) where

import AST.V0_16
import AST.Structure
import Control.Applicative ((<|>))
import Data.Coapplicative
import ElmFormat.ImportInfo (ImportInfo)

import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Dict
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified ElmFormat.ImportInfo as ImportInfo


data MatchedNamespace t
    = NoNamespace
    | MatchedImport t
    | Unmatched t
    deriving (Eq, Ord, Show)


fromMatched :: t -> MatchedNamespace t -> t
fromMatched empty NoNamespace = empty
fromMatched _ (MatchedImport t) = t
fromMatched _ (Unmatched t) = t


data Name
    = TypeName UppercaseIdentifier
    | CtorName UppercaseIdentifier
    | VarName LowercaseIdentifier
    deriving (Eq, Ord)

matchReferences ::
    (Functor annf, Coapplicative annf, Ord u) =>
    ImportInfo [u]
    -> ASTNS annf [u] kind
    -> ASTNS annf (MatchedNamespace [u]) kind
matchReferences importInfo =
    let
        aliases = Bimap.toMap $ ImportInfo._aliases importInfo
        imports = ImportInfo._directImports importInfo
        exposed =
            Dict.union
                (Dict.mapKeys Left $ ImportInfo._exposed importInfo)
                (Dict.mapKeys Right $ ImportInfo._exposedTypes importInfo)

        toExposedKey (TypeName u) = Right u
        toExposedKey (CtorName u) = Right u
        toExposedKey (VarName l) = Left l

        f locals ns identifier =
            case ns of
                [] ->
                    case Dict.lookup identifier locals of
                        Just () -> NoNamespace
                        Nothing ->
                            case Dict.lookup (toExposedKey identifier) exposed of
                                Nothing -> NoNamespace
                                Just exposedFrom -> MatchedImport exposedFrom

                _ ->
                    let
                        self =
                            if Set.member ns imports then
                                Just ns
                            else
                                Nothing

                        fromAlias =
                            Dict.lookup ns aliases

                        resolved =
                            fromAlias <|> self
                    in
                    case resolved of
                        Nothing -> Unmatched ns
                        Just single -> MatchedImport single

        defineLocalType u = Dict.insert (TypeName u) ()
        defineLocalCtor u = Dict.insert (CtorName u) ()
        defineLocalVar l = Dict.insert (VarName l) ()

        mapTypeRef locals (ns, u) = (f locals ns (TypeName u), u)
        mapCtorRef locals (ns, u) = (f locals ns (CtorName u), u)
        mapVarRef locals (VarRef ns l) = VarRef (f locals ns (VarName l)) l
        mapVarRef locals (TagRef ns u) = TagRef (f locals ns (CtorName u)) u
        mapVarRef _ (OpRef op) = OpRef op
    in
    topDownReferencesWithContext
        defineLocalType defineLocalCtor defineLocalVar
        mapTypeRef mapCtorRef mapVarRef
        mempty


applyReferences ::
    (Functor annf, Ord u) =>
    ImportInfo [u]
    -> ASTNS annf (MatchedNamespace [u]) kind
    -> ASTNS annf [u] kind
applyReferences importInfo =
    let
        aliases = Bimap.toMapR $ ImportInfo._aliases importInfo
        exposed =
            Dict.union
                (Dict.mapKeys Left $ ImportInfo._exposed importInfo)
                (Dict.mapKeys Right $ ImportInfo._exposedTypes importInfo)

        f ns' identifier =
            case ns' of
                NoNamespace -> []
                MatchedImport ns ->
                    case Dict.lookup identifier exposed of
                        Just exposedFrom | exposedFrom == ns -> []
                        _ -> Maybe.fromMaybe ns $ Dict.lookup ns aliases
                Unmatched name -> name
        mapTypeRef (ns, u) = (f ns (Right u), u)
        mapCtorRef (ns, u) = (f ns (Right u), u)
        mapVarRef (VarRef ns l) = VarRef (f ns (Left l)) l
        mapVarRef (TagRef ns u) = TagRef (f ns (Right u)) u
        mapVarRef (OpRef op) = OpRef op
    in
    bottomUpReferences mapTypeRef mapCtorRef mapVarRef
