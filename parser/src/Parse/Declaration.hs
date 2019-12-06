module Parse.Declaration where

import Text.Parsec ( (<|>), (<?>), choice, digit, optionMaybe, string, try )

import AST.Declaration
import AST.Structure
import AST.Variable as Var
import ElmVersion
import Parse.Comments
import qualified Parse.Expression as Expr
import Parse.Helpers as Help
import qualified Parse.Type as Type
import AST.V0_16
import Parse.IParser
import Parse.Whitespace
import Reporting.Annotation (Located)


declaration :: ElmVersion -> IParser (ASTNS Declaration Located [UppercaseIdentifier])
declaration elmVersion =
    typeDecl elmVersion <|> infixDecl elmVersion <|> port elmVersion <|> definition elmVersion


topLevelStructure :: IParser a -> IParser (TopLevelStructure a)
topLevelStructure entry =
    choice
        [ DocComment <$> docCommentAsMarkdown
        , Entry <$> entry
        ]



-- TYPE ANNOTATIONS and DEFINITIONS

definition :: ElmVersion -> IParser (ASTNS Declaration Located [UppercaseIdentifier])
definition elmVersion =
    (Expr.typeAnnotation elmVersion TypeAnnotation <|> Expr.definition elmVersion Definition)
    <?> "a value definition"


-- TYPE ALIAS and UNION TYPES

typeDecl :: ElmVersion -> IParser (Declaration typeRef ctorRef varRef pat (FixAST Typ Located typeRef ([UppercaseIdentifier], UppercaseIdentifier) varRef) expr)
typeDecl elmVersion =
  do  try (reserved elmVersion "type") <?> "a type declaration"
      postType <- forcedWS
      isAlias <- optionMaybe (string "alias" >> forcedWS)

      name <- capVar elmVersion
      args <- spacePrefix (lowVar elmVersion)
      (C (preEquals, postEquals) _) <- padded equals

      case isAlias of
        Just postAlias ->
            do  tipe <- Type.expr elmVersion <?> "a type"
                return $
                  AST.Declaration.TypeAlias
                    postType
                    (C (postAlias, preEquals) (name, args))
                    (C postEquals tipe)

        Nothing ->
            do
                tags_ <- pipeSep1 (Type.tag elmVersion) <?> "a constructor for a union type"
                return
                    AST.Declaration.Datatype
                        { nameWithArgs = C (postType, preEquals) (name, args)
                        , tags = exposedToOpen postEquals tags_
                        }


-- INFIX


infixDecl :: ElmVersion -> IParser (Declaration typeRef ctorRef (Var.Ref [UppercaseIdentifier]) pat typ e)
infixDecl elmVersion =
    expecting "an infix declaration" $
    choice
        [ try $ infixDecl_0_16 elmVersion
        , infixDecl_0_19 elmVersion
        ]


infixDecl_0_19 :: ElmVersion -> IParser (Declaration typeRef ctorRef varRef pat typ e)
infixDecl_0_19 elmVersion =
    let
        assoc =
            choice
                [ string "right" >> return AST.Declaration.R
                , string "non" >> return AST.Declaration.N
                , string "left" >> return AST.Declaration.L
                ]
    in
    AST.Declaration.Fixity_0_19
        <$> (try (reserved elmVersion "infix") *> preCommented assoc)
        <*> (preCommented $ (\n -> read [n]) <$> digit)
        <*> (commented symOpInParens)
        <*> (equals *> preCommented (lowVar elmVersion))


infixDecl_0_16 :: ElmVersion -> IParser (Declaration typeRef ctorRef (Var.Ref [UppercaseIdentifier]) pat typ e)
infixDecl_0_16 elmVersion =
  do  assoc <-
          choice
            [ try (reserved elmVersion "infixl") >> return AST.Declaration.L
            , try (reserved elmVersion "infixr") >> return AST.Declaration.R
            , try (reserved elmVersion "infix")  >> return AST.Declaration.N
            ]
      digitComments <- forcedWS
      n <- digit
      opComments <- forcedWS
      AST.Declaration.Fixity assoc digitComments (read [n]) opComments <$> anyOp elmVersion


-- PORT

port :: ElmVersion -> IParser (ASTNS Declaration Located [UppercaseIdentifier])
port elmVersion =
  expecting "a port declaration" $
  do  try (reserved elmVersion "port")
      preNameComments <- whitespace
      name <- lowVar elmVersion
      postNameComments <- whitespace
      let name' = C (preNameComments, postNameComments) name
      choice [ portAnnotation name', portDefinition name' ]
  where
    portAnnotation name =
      do  try hasType
          typeComments <- whitespace
          tipe <- Type.expr elmVersion <?> "a type"
          return (AST.Declaration.PortAnnotation name typeComments tipe)

    portDefinition name =
      do  try equals
          bodyComments <- whitespace
          expr <- Expr.expr elmVersion
          return (AST.Declaration.PortDefinition name bodyComments expr)
