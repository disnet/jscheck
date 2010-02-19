module HJS.Interpreter.Error where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State 

import HJS.Interpreter.InterpMDecl
import HJS.Interpreter.InterpM
import HJS.Interpreter.ObjectBasic


errorConstructor :: InterpM Value
errorConstructor = do 
                        args <- getArgs
		        o <- newObject "Error"
			t <- getThis
		        p <- getProperty (toObjId t) "prototype"
                        putPropertyInternal o "__proto__" p 
                        case args of 
                            (x:_) -> putProperty o "message" x
                            _     -> return ()
                        return $ inj o


addErrorBuiltIn newConstructorWith putBuiltIn = do 
                 newConstructorWith "Error" errorConstructor
--                 p <- getProperty fo "prototype"
--                 let p' = toObjId p

