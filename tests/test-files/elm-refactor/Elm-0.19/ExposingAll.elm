module ExposingAdd exposing (directReference)

import A exposing (..)


directReference =
    a1


hiddenByTopLevelParams a1 =
    a1


hiddenByLetDeclarationParams =
    let
        let1 a1 =
            a1
    in
    ()
