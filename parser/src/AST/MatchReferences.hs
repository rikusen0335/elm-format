module AST.MatchReferences (MatchedNamespace(..), fromMatched, matchReferences, applyReferences) where

import AST.V0_16
import AST.Structure
import Control.Applicative ((<|>))
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


matchReferences ::
    (Functor annf, Ord u) =>
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

        f ns identifier =
            case ns of
                [] ->
                    case Dict.lookup identifier exposed of
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

        mapTypeRef (ns, u) = (f ns (Right u), u)
        mapCtorRef (ns, u) = (f ns (Right u), u)
        mapVarRef (VarRef ns l) = VarRef (f ns (Left l)) l
        mapVarRef (TagRef ns u) = TagRef (f ns (Right u)) u
        mapVarRef (OpRef op) = OpRef op
    in
    bottomUpReferences mapTypeRef mapCtorRef mapVarRef


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
