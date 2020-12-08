{-# language DataKinds #-}
{-# language TypeApplications #-}
import Mod
import Data.Foldable

type Test = Mod 7 Int

main :: IO ()
main = do
  print $ fromInteger @Test 1 == 8
  traverse_ (print @Test) [2 + 6, 3 * 7, -1, abs (-1), signum (-1)]
