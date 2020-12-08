# `mod`

Modular arithmetic using singletons:

    type Test = Mod 7 Int
    traverse_ (print @Test) [2 + 6, 3 * 7, -1, abs (-1), signum (-1)]
    Mod (SNat @7) 1
    Mod (SNat @7) 0
    Mod (SNat @7) 6
    Mod (SNat @7) 6
    Mod (SNat @7) 1

