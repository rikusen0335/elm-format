{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module AST.Declaration where

import AST.Structure
import AST.V0_16
import ElmFormat.Mapping

import qualified AST.Variable as Var
import qualified Cheapskate.Types as Markdown


-- DECLARATIONS

type NameWithArgs name arg =
    (name, [C1 BeforeTerm arg])

data AfterName; data BeforeType; data AfterEquals
data Declaration typeRef ctorRef varRef pat typ expr
    = Definition pat [C1 BeforeTerm pat] Comments expr
    | TypeAnnotation (C1 AfterName (Var.Ref ())) (C1 BeforeType typ)
    | Datatype
        { nameWithArgs :: C2 Before After (NameWithArgs UppercaseIdentifier LowercaseIdentifier)
        , tags :: OpenCommentedList (NameWithArgs UppercaseIdentifier typ)
        }
    | TypeAlias Comments
        (C2 Before After (NameWithArgs UppercaseIdentifier LowercaseIdentifier))
        (C1 AfterEquals typ)
    | PortAnnotation (C2 Before After LowercaseIdentifier) Comments typ
    | PortDefinition (C2 Before After LowercaseIdentifier) Comments expr
    | Fixity Assoc Comments Int Comments varRef
    | Fixity_0_19 (C1 Before Assoc) (C1 Before Int) (C2 Before After SymbolIdentifier) (C1 Before LowercaseIdentifier)
    deriving (Eq, Show, Functor)

instance MapAST Declaration where
    mapAll _ _ fvr fp ft fe = \case
        Definition name args c e -> Definition (fp name) (fmap (fmap fp) args) c (fe e)
        TypeAnnotation name t -> TypeAnnotation name (fmap ft t)
        Datatype nameWithArgs ctors -> Datatype nameWithArgs (fmap (fmap $ fmap $ fmap ft) ctors)
        TypeAlias c nameWithArgs t -> TypeAlias c nameWithArgs (fmap ft t)
        PortAnnotation name c t -> PortAnnotation name c (ft t)
        PortDefinition name c e -> PortDefinition name c (fe e)
        Fixity a c n c' name -> Fixity a c n c' (fvr name)
        Fixity_0_19 a n op name -> Fixity_0_19 a n op name

instance Functor ann => ChangeAnnotation (ASTNS Declaration ann ns) ann where
    type SetAnnotation ann' (ASTNS Declaration ann ns) = ASTNS Declaration ann' ns
    convertFix f = mapAll id id id (convertFix f) (convertFix f) (convertFix f)


-- INFIX STUFF

data Assoc = L | N | R
    deriving (Eq, Show)


assocToString :: Assoc -> String
assocToString assoc =
    case assoc of
      L -> "left"
      N -> "non"
      R -> "right"


-- DECLARATION PHASES


data TopLevelStructure a
    = DocComment Markdown.Blocks
    | BodyComment Comment
    | Entry a
    deriving (Eq, Show, Functor)


