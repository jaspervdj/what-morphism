--------------------------------------------------------------------------------
module WhatMorphism.Types
    ( WhatMorphismMode (..)
    , WhatMorphismVerbosity (..)
    ) where


--------------------------------------------------------------------------------
data WhatMorphismMode
    = WhatMorphismFull
    | WhatMorphismQuick
    deriving (Show, Eq)


--------------------------------------------------------------------------------
data WhatMorphismVerbosity
    = WhatMorphismQuiet
    | WhatMorphismDebug
    deriving (Show, Eq)
