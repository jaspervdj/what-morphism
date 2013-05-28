what-morphism
=============

A GHC Plugin to automatically transform explicit recursion into folds and builds
where possible. See also: <https://github.ugent.be/javdrjeu/thesis>.

Configuring & installing
------------------------

You might want to edit the options in `src/WhatMorphism.hs`, i.e. the
`whatMorphismConfig` value. The defaults are:

    whatMorphismConfig :: WhatMorphismConfig
    whatMorphismConfig = WhatMorphismConfig
        { whatMorphismScope     = WhatMorphismFull
        , whatMorphismMode      = WhatMorphismTransform
        , whatMorphismVerbosity = WhatMorphismDebug
        }

Then installation can be done by using simply:

    cabal install

Compiling code
--------------

In order to compile a piece of code, say, `test.hs`:

    data Tree a
        = Leaf a
        | Branch (Tree a) (Tree a)

    size :: Tree a -> Int -> Int
    size (Leaf _)     n = n + 1
    size (Branch l r) n = size r (size l (n+ 1))

Take the following steps:

1. Due to some limitations with the current symbol lookup code, you need to move
   the datatype to a separate module, e.g. `Types.hs`:

        data Tree a
            = Leaf a
            | Branch (Tree a) (Tree a)

2. Add the necessary imports, annotations and pragmas here:

        {-# LANGUAGE TemplateHaskell #-}
        {-# LANGUAGE Rank2Types      #-}

        import WhatMorphism.Annotations
        import WhatMorphism.TemplateHaskell

        data Tree a
            = Leaf a
            | Branch (Tree a) (Tree a)

        $(deriveFold ''Tree "foldTree")
        $(deriveBuild ''Tree "buildTree")
        {-# ANN type Tree (RegisterFoldBuild "foldTree" "buildTree") #-}

    In the simpler case when we're just dealing with lists, simply adding the
    import

        import WhatMorphism.HaskellList

    should suffice.

3. You should now be good to go. Compile the code using the following flags:

        ghc --make -fforce-recomp -dverbose-core2core \
            -package what-morphism -fplugin WhatMorphism test.hs

    This produces **a lot** of output, so you probably want to append something
    like `>build.log 2>&1` and then study the `build.log` file.
