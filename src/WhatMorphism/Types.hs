--------------------------------------------------------------------------------
module WhatMorphism.Types
    ( WhatMorphismMode (..)
    ) where


--------------------------------------------------------------------------------
data WhatMorphismMode
    = WhatMorphismFull
    | WhatMorphismQuick
    deriving (Show, Eq)
