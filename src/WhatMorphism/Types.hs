--------------------------------------------------------------------------------
module WhatMorphism.Types
    ( WhatMorphismConfig (..)
    , WhatMorphismScope (..)
    , WhatMorphismMode (..)
    , WhatMorphismVerbosity (..)
    ) where


--------------------------------------------------------------------------------
data WhatMorphismConfig = WhatMorphismConfig
    { whatMorphismScope     :: WhatMorphismScope
    , whatMorphismMode      :: WhatMorphismMode
    , whatMorphismVerbosity :: WhatMorphismVerbosity
    } deriving (Show)


--------------------------------------------------------------------------------
data WhatMorphismScope
    = WhatMorphismFull
    | WhatMorphismQuick
    deriving (Show, Eq)


--------------------------------------------------------------------------------
data WhatMorphismMode
    = WhatMorphismTransform
    | WhatMorphismDetect
    deriving (Show, Eq)


--------------------------------------------------------------------------------
data WhatMorphismVerbosity
    = WhatMorphismQuiet
    | WhatMorphismDebug
    deriving (Show, Eq)
