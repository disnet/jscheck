{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances  #-}
{-# OPTIONS -fallow-incoherent-instances #-}
{-
        Built in functions provided by 'host' 
-}
module HJS.Interpreter.Host where



import HJS.Interpreter.InterpMDecl
import HJS.Interpreter.InterpM
import HJS.Interpreter.Interp

import HJS.Interpreter.ObjectBasic
import HJS.Interpreter.Object
import HJS.Interpreter.Array
import HJS.Interpreter.Function
import HJS.Interpreter.Error
import HJS.Interpreter.String
import HJS.Interpreter.Regex

import Control.Monad.State 

globalObj = ObjId 1
objPrototype = ObjId 2


print' :: InterpM Value
print' = do
             args' <- getArgs
             op <- getProperty globalObj "_output"
             args <- mapM toRealString args'
             op' <- toRealString op
             let 
                 prefix = if isUndefined op then "" else (op' ++ "\n")
                 s = concat (prefix:args)
             putProperty globalObj "_output" (inj s)
             return (inj Undefined)

print'' :: InterpM Value
print'' = do
             args' <- getArgs
             op <- getProperty globalObj "_output"
             args <- mapM toRealString args'
             liftIO $ putStrLn $ concat args
             return undefinedValue

putBuiltIn :: ObjId -> String -> [String] -> InterpM Value -> InterpM ()
putBuiltIn obj name args f = do
                                fo <- newBuiltInFunction args f
				putProperty obj name (inj fo)

objectConstructor :: InterpM Value
objectConstructor = do 
                          o <- newObject "Object"
                          return $ inj o



newClassObject name = do
		 obj <- newObject "Object"
                 putProperty globalObj name (inj obj)
                 fo <- newBuiltInFunction [] objectConstructor
                 o <- newObject "Object"
                 putProperty obj "prototype" (inj o)
                 putProperty obj "Construct" (inj fo)

constructorConstructor :: InterpM Value
constructorConstructor = defaultConstructor  "Object"

--objectWithClassConstructor klass = do 
--                        o <- newObject klass
--			(((_,_,t):_), _) <- get
--                        args <- getArgs
--		        p <- getProperty t "prototype"
--                        putObjectProperty o "__proto__" p 
--                        return $ inj o


-- Creates a new constructor with supplied constructor
newConstructorWith :: String -> InterpM Value -> InterpM ObjId
newConstructorWith name c = do 
                            fo <- newFuncObject [] [] c
                            putObjectProperty fo "name" (inj name)
                            putObjectProperty globalObj name (inj fo)
			    return fo

-- Creates a new constructor
newConstructor name = do 
                            fo <- newFuncObject [] [] (defaultConstructor name)
                            putObjectProperty fo "name" (inj name)
                            putObjectProperty globalObj name (inj fo)
			    return fo

addBuiltIn :: InterpM ()
addBuiltIn = do
                 fo <- newBuiltInFunction ["arg1"]  print''
                 putProperty globalObj "print" (inj fo)
                 addObjectBuiltIn newConstructor putBuiltIn
                 addFunctionBuiltIn newConstructor putBuiltIn callFunction
                 addErrorBuiltIn newConstructorWith  putBuiltIn
		 addArrayBuiltIn newConstructorWith putBuiltIn 
	         addStringBuiltIn newConstructorWith
	         addRegexBuiltIn newConstructorWith putBuiltIn
                 return ()

initEnvironment = do
                      go <- newObjectRaw "global"
                      op <- newObjectRaw "Object"
                      putPropertyInternal go "__proto__" (inj op)
                      putPropertyInternal op "__parent__" (inj go)
                      pushContext ([go],go,go, ObjIdNull)
                      addBuiltIn
