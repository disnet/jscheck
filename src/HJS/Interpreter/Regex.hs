module HJS.Interpreter.Regex where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

import Text.Regex 

import HJS.Interpreter.InterpMDecl
import HJS.Interpreter.InterpM
import HJS.Interpreter.ObjectBasic

regexConstructor :: InterpM Value
regexConstructor = do
                        args <- getArgs 
                        t <- getThis
                        case args of
                            (p:f:_) -> do newRegexObject t p f
                            (p:[]) -> do newRegexObject t p undefinedValue
                            _ -> newRegexObject t undefinedValue undefinedValue
     
test :: InterpM  Value            
test = do
               t <- getThis
               args <- getArgs
               p <- getProperty (toObjId t) "Match"
               p' <- toRealString p
               s  <- toRealString (head args)
               case simpleMatch p' s of
                  Nothing -> return $ inj $ False
                  _ -> return $ inj $ True
               

simpleMatch p s =  case matchRegexAll (mkRegex p) s of
		       Just (before,match,[],subexpr) -> Just subexpr
                       _ -> Nothing
                        
     
newRegex p f = do
              c <- getValue (inj $ Ref "Regex")
              newRegexObject c p f 
         
          
newRegexObject :: Value -> Value -> Value -> InterpM Value
newRegexObject c p f = do
		     o <- newObject "Regex"
                     putPropertyInternal o "Match" p
	             putPropertyInternal o "Flags" f 
		     p <- getProperty (toObjId c) "prototype"
                     putPropertyInternal o "__proto__" p 
                     return (inj o)


addRegexBuiltIn newConstructorWith putBuiltIn = do 
                 fo <- newConstructorWith "Regex" regexConstructor
                 p <- getProperty fo "prototype"
                 let p' = toObjId p
		 putBuiltIn p' "test" [] test
