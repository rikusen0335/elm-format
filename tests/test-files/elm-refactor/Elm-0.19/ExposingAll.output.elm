module ExposingAdd exposing (directReference)


directReference =
    ()


hiddenByTopLevelParams a1 =
    a1


hiddenByLetDeclarationParams =
    let
        let1 a1 =
            a1
    in
    ()


hiddenBy_patterns =
    let
        byVar a1 =
            a1

        byData (Just a1) =
            a1
    in
    ()
