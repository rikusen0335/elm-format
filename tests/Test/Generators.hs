{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Generators where

import Data.Map.Strict
import Test.QuickCheck

import AST.V0_16
import qualified AST.Declaration
import qualified AST.Expression
import qualified AST.Module
import qualified AST.Pattern
import AST.Structure
import qualified AST.Variable
import Data.Fix
import Data.Functor.Identity
import qualified Reporting.Annotation
import qualified Reporting.Region


capitalLetter :: Gen Char
capitalLetter =
    elements "ABCDEFGHIJKLMNOPQRSTUVWXYZÀΩЉԱႠḀℇ𐐅𝐴𝞨"
    -- Should also be capital, but Data.Char.isUpper does not agree: Ꭳ


capitalAscii :: Gen Char
capitalAscii =
    elements "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


lowerLetter :: Gen Char
lowerLetter =
    elements "abcdefghijklmnopqrstuvwxyz"


number :: Gen Char
number =
    elements "0123456789"


capIdentifier :: Gen UppercaseIdentifier
capIdentifier =
    do
        first <- capitalLetter
        rest <- listOf $ oneof [ capitalLetter, lowerLetter, number ]
        return $ UppercaseIdentifier $ first:rest


lowerIdentifier :: Gen LowercaseIdentifier
lowerIdentifier =
    do
        first <- lowerLetter
        rest <- listOf $ oneof [ capitalLetter, lowerLetter, number ]
        return $ LowercaseIdentifier $ first:rest


commented :: Gen a -> Gen (C2 before after a)
commented inner =
    C ([], []) <$> inner


instance Arbitrary AST.Variable.Value where
    arbitrary =
        do
            name <- capIdentifier
            return $ AST.Variable.Union (C [] name) AST.Variable.ClosedListing


listing :: Gen (AST.Variable.Listing a)
listing =
    return $ AST.Variable.OpenListing (C ([], []) ())


instance Arbitrary (ASTNS (AST.Module.Module Identity [UppercaseIdentifier]) Identity [UppercaseIdentifier]) where
    arbitrary =
        do
            name <- listOf1 $ capIdentifier
            listing <- listing
            moduleType <- fmap (\x -> if x then AST.Module.Port [] else AST.Module.Normal) arbitrary
            return $ AST.Module.Module
                []
                (Just $ AST.Module.Header
                  moduleType
                  (C ([], []) name)
                  Nothing
                  (Just $ C ([], []) listing)
                )
                (Reporting.Annotation.at (Reporting.Region.Position 0 0) (Reporting.Region.Position 0 0) Nothing)
                (C [] empty)
                [ AST.Declaration.Entry $ pure $ AST.Declaration.Definition (FixAST $ pure $ AST.Pattern.Anything) [] [] (FixAST $ pure $ AST.Expression.TupleFunction 2)]
