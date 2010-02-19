module HJS.Interpreter.Eval where

import Control.Monad.Trans

import HJS.Parser
import HJS.Parser.JavaScript
import HJS.Interpreter.InterpMDecl
import HJS.Interpreter.InterpM hiding (getArgs)
import HJS.Interpreter.Interp


eval :: String -> InterpM Value
eval s = do
            case parseProgram s of 
                        Right r -> interp r
                        Left s -> return $ inj undefinedValue
            