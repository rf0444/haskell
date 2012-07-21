
import Language.Haskell.Extension
import Data.List

main = putStrLn . unlines . sort . map show $ knownExtensions

