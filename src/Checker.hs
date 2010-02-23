module Checker where
import HJS.Parser
import HJS.Parser.JavaScript
import Text.ParserCombinators.Parsec.Error

checkProgram :: String -> Either ParseError [SourceElement]
checkProgram input = parseProgram input
