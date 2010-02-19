module HJS.Interpreter.Array where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State 

import HJS.Interpreter.InterpMDecl
import HJS.Interpreter.InterpM
import HJS.Interpreter.ObjectBasic

arrayConstructor :: InterpM Value
arrayConstructor = do 
                        args <- getArgs
		        o <- newArrayObject args
			t <- getThis
		        p <- getProperty (toObjId t) "prototype"
                        putPropertyInternal o "__proto__" p 
                        return $ inj o

newArrayObject :: [Value] -> InterpM ObjId
newArrayObject vs = do 
                          a <- newObject "Array"
                          putObjectProperty a "length" (inj (0::Int))
                          zipWithM (\x y -> putProperty a (show x) y) [0..] vs
                          return a

toList :: Value ->  InterpM [Value]
toList v = do
              let id' = toObjId v
	      len <- getProperty id' "length"
              let len' = toRealInt len
 	      mapM (\x -> getProperty id' (show x)) [0..(len'-1)]

          

push :: InterpM Value
push = do
                  args <- getArgs
	          t <- getThis
                  let addone x= do 
				    len <- getProperty (toObjId t) "length"
				    let l = show $ toRealInt len
                                    putProperty (toObjId t) l x 
		  ls <- mapM addone args
		  return (inj Undefined)
                  
-- Create the Array function object and then add the built in functions to the prototype
addArrayBuiltIn newConstructorWith putBuiltIn = do 
                 fo <- newConstructorWith "Array" arrayConstructor
                 p <- getProperty fo "prototype"
                 let p' = toObjId p
		 putBuiltIn p' "push" [] push