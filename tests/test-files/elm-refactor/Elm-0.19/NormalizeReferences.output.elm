module NormalizeReferences exposing (Maybe)

import Maybe exposing (Maybe)
import Result as R exposing (Result)


{-| Qualified references that cannot be referenced directly should not be simplified.
-}
type alias Maybe =
    Maybe.Maybe


{-| Qualified references that cannot be referenced directly should not be simplified.
-}
map =
    Maybe.map


{-| Qualified references that cannot be referenced directly should use the appropriate alias.
-}
type alias Result =
    R.Result
