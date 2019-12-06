module ElmFormat.Render.Text where

import Data.Coapplicative
import Elm.Utils ((|>))
import ElmVersion (ElmVersion)
import AST.Structure
import AST.V0_16

import qualified AST.Module
import qualified Box
import qualified Data.Text as Text
import qualified ElmFormat.Render.Box as Render


render :: Coapplicative annf => ElmVersion -> ASTNS (AST.Module.Module annf [UppercaseIdentifier]) annf [UppercaseIdentifier] -> Text.Text
render elmVersion modu =
    renderBox $ Render.formatModule elmVersion True 2 modu


renderBox :: Box.Box -> Text.Text
renderBox box =
    box
        |> Box.render


-- TODO: remove this and convert the Integration test to a test fixture
renderLiteral :: ElmVersion -> Literal -> Text.Text
renderLiteral elmVersion literal =
    renderBox $ Render.formatLiteral elmVersion literal
