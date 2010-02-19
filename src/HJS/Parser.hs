module HJS.Parser(parseProgram,lexProgram,lexFile,runLexer) where

import HJS.Parser.JavaScriptParser
import HJS.Parser.JavaScript


lexFile flags name = do
                            input <- readFile name
		            putStrLn $ show $ lexProgram input


