import System.Environment
import Checker
import HJS.Parser
import HJS.Parser.JavaScript


-- takes a programm in as the first command line arguemnt and runs the checker
main :: IO ()
main = getArgs >>= print . parseProgram . head
