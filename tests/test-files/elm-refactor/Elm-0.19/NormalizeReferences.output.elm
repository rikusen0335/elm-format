module NormalizeReferences exposing (Maybe)

import Maybe exposing (Maybe)


{-| Qualified references that cannot be references directly should not be simplified.
-}
type alias Maybe =
    Maybe.Maybe


{-| Qualified references that cannot be references directly should not be simplified.
-}
map =
    Maybe.map
