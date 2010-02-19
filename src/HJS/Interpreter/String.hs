module HJS.Interpreter.String where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State 

import HJS.Interpreter.InterpMDecl
import HJS.Interpreter.InterpM
import HJS.Interpreter.ObjectBasic
import HJS.Interpreter.Regex
import HJS.Interpreter.Array

               
stringConstructor :: InterpM Value   
stringConstructor = do
                        args <- getArgs 
                        o <- newObject "String"
                        putPropertyInternal o "Value" (head args)
			setPrototype o
                        return (inj o)
                        

match :: InterpM Value
match = do
              t <- getThis
              (re:_) <- getArgs
              s <- getProperty (toObjId t) "Value" >>= toRealString 
              p <- getProperty (toObjId re) "Match" >>= toRealString
	      let res = case simpleMatch p s  of
                             Nothing -> []
                             Just ss -> ss
              let res' = map (\x -> (inj (x::String))::Value) res
              o <- newArrayObject res'
              return $ inj o
              

addStringBuiltIn newConstructorWith = do 
                 newConstructorWith "String" stringConstructor
