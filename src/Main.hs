import System.Environment
import Checker
import HJS.Parser
import HJS.Parser.JavaScript
import Extractor


-- takes a programm in as the first command line arguemnt and runs the checker
main :: IO ()
main = getArgs >>= print . runChecker . head

runExtract :: String -> [XType]
runExtract p = case parseProgram p of
                Right r -> runExtractor r
                Left l -> []               
                
