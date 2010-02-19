module HJS.Interpreter.Function where

import HJS.Interpreter.InterpMDecl
import HJS.Interpreter.InterpM

import HJS.Interpreter.Array

addFunctionBuiltIn newConstructor putBuiltIn callFunction = do 
                 fo <- newConstructor "Function"
                 putPropertyInternal fo "__proto__" (inj $ ObjId 2)
                 p <- getProperty fo "prototype"
                 let p' = toObjId p
		 putBuiltIn p' "apply" [] (apply callFunction)


apply callFunction = do 
                        args <- getArgs
                        this <- getThis
                        case args of
                              [] ->  callFunction this [] ObjIdNull
                              (target:[]) -> callFunction this [] (toObjId target)
                              (target:args':_) -> do 
						     args' <- toList args'
						     callFunction this args' (toObjId target)

		     