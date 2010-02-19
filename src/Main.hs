import System.Environment
import HJS.Parser


-- | 'main' runs the main program
main :: IO ()
main = do
  print (parseProgram "@foo function bar(a) { return 0; }")

