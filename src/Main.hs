import System.Environment
import Checker


-- takes a programm in as the first command line arguemnt and runs the checker
main :: IO ()
main = getArgs >>= print . checkProgram . head
