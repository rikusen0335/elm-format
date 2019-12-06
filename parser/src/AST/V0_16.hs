{-# LANGUAGE DuplicateRecordFields #-}
module AST.V0_16 where

import Data.Coapplicative
import Data.Int (Int64)


type List a = [a]


newtype ForceMultiline =
    ForceMultiline Bool
    deriving (Eq, Show)


data LowercaseIdentifier =
    LowercaseIdentifier String
    deriving (Eq, Ord, Show)


data UppercaseIdentifier =
    UppercaseIdentifier String
    deriving (Eq, Ord, Show)


data SymbolIdentifier =
    SymbolIdentifier String
    deriving (Eq, Ord, Show)


data Comment
    = BlockComment (List String)
    | LineComment String
    | CommentTrickOpener
    | CommentTrickCloser
    | CommentTrickBlock String
    deriving (Eq, Ord, Show)
type Comments = List Comment


data Commented c a =
    C c a
    deriving (Eq, Ord, Functor, Show) -- TODO: is Ord needed?

instance Coapplicative (Commented c) where
    extract (C _ a) = a
    {-# INLINE extract #-}

type C1 l1 a = Commented Comments a
type C2 l1 l2 a = Commented (Comments, Comments) a
type C3 l1 l2 l3 a = Commented (Comments, Comments, Comments) a

type C0Eol a = Commented (Maybe String) a
type C1Eol l1 a = Commented (Comments, Maybe String) a
type C2Eol l1 l2 a = Commented (Comments, Comments, Maybe String) a


{-| This represents a list of things separated by comments.

Currently, the first item will never have leading comments.
However, if Elm ever changes to allow optional leading delimiters, then
comments before the first delimiter will go there.
-}
data BeforeSeparator; data AfterSeparator
type Sequence a =
    List (C2Eol BeforeSeparator AfterSeparator a)


{-| This represents a list of things between clear start and end delimiters.
Comments can appear before and after any item, or alone if there are no items.

For example:
  ( {- nothing -} )
  ( a, b )

TODO: this should be replaced with (Sequence a, Comments)
-}
data Inside
data ContainedCommentedList a
    = Empty (C1 Inside ())
    | Items [C2 Before After a]


{-| This represents a list of things that have no clear start and end
delimiters.

If there is more than one item in the list, then comments can appear.
Comments can appear after the first item, before the last item, and
around any other item.
An end-of-line comment can appear after the last item.

If there is only one item in the list, an end-of-line comment can appear after the item.

TODO: this should be replaced with (Sequence a)
-}
data AfterFirstTerm; data BeforeLastTerm; data BeforeTerm; data AfterTerm
data ExposedCommentedList a
    = Single (C0Eol a)
    | Multiple (C1Eol AfterFirstTerm a) [C2Eol BeforeTerm AfterTerm a] (C1Eol BeforeLastTerm a)


{-| This represents a list of things that have a clear start delimiter but no
clear end delimiter.
There must be at least one item.
Comments can appear before the last item, or around any other item.
An end-of-line comment can also appear after the last item.

For example:
  = a
  = a, b, c

TODO: this should be replaced with (Sequence a)
-}
data OpenCommentedList a
    = OpenCommentedList [C2Eol BeforeTerm AfterTerm a] (C1Eol BeforeLastTerm a)
    deriving (Eq, Show, Functor)


exposedToOpen :: Comments -> ExposedCommentedList a -> OpenCommentedList a
exposedToOpen pre exposed =
    case exposed of
        Single (C eol item) ->
            OpenCommentedList [] (C (pre, eol) item)

        Multiple (C (postFirst, eol) first') rest' lst ->
            OpenCommentedList (C (pre, postFirst, eol) first' : rest') lst


{-| Represents a delimiter-separated pair.

Comments can appear after the key or before the value.

For example:

  key = value
  key : value
-}
data AfterKey; data BeforeValue
data Pair key value =
    Pair
        { _key :: C1 AfterKey key
        , _value :: C1 BeforeValue value
        , forceMultiline :: ForceMultiline
        }
    deriving (Show, Eq, Functor)


data Multiline
    = JoinAll
    | SplitAll
    deriving (Eq, Show)


isMultiline :: Multiline -> Bool
isMultiline JoinAll = False
isMultiline SplitAll = True


data FunctionApplicationMultiline
    = FASplitFirst
    | FAJoinFirst Multiline
    deriving (Eq, Show)


data IntRepresentation
  = DecimalInt
  | HexadecimalInt
  deriving (Eq, Show)


data FloatRepresentation
  = DecimalFloat
  | ExponentFloat
  deriving (Eq, Show)


data Literal
    = IntNum Int64 IntRepresentation
    | FloatNum Double FloatRepresentation
    | Chr Char
    | Str String Bool
    | Boolean Bool
    deriving (Eq, Show)


data TypeConstructor ctorRef
    = NamedConstructor ctorRef
    | TupleConstructor Int -- will be 2 or greater, indicating the number of elements in the tuple
    deriving (Eq, Show, Functor)

data Before; data After
data Typ typeRef ctorRef varRef pat typ expr
    = UnitType Comments
    | TypeVariable LowercaseIdentifier
    | TypeConstruction (TypeConstructor ctorRef) [C1 Before typ]
    | TypeParens (C2 Before After typ)
    | TupleType [C2Eol Before After typ]
    | RecordType
        { base :: Maybe (C2 Before After LowercaseIdentifier)
        , fields :: Sequence (Pair LowercaseIdentifier typ)
        , trailingComments :: Comments
        , forceMultiline :: ForceMultiline
        }
    | FunctionType
        { first :: C0Eol typ
        , rest :: Sequence typ
        , forceMultiline :: ForceMultiline
        }
    deriving (Eq, Show)
