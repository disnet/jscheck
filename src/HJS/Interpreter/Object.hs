module HJS.Interpreter.Object where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State 

import HJS.Interpreter.InterpMDecl
import HJS.Interpreter.InterpM

import HJS.Interpreter.ObjectBasic
import HJS.Interpreter.Array

import qualified Data.Map as M



addObjectBuiltIn newConstructor putBuiltIn = do
    fo <- newConstructor "Object"
    putPropertyInternal fo "prototype" (inj $ ObjId 2)
    putBuiltIn (ObjId 2) "propertyNames" [] propertyNames
    putBuiltIn (ObjId 2) "hasOwnProperty" [] hasOwnProperty
    putBuiltIn (ObjId 2) "toString" [] toString'


propertyNames :: InterpM Value
propertyNames = do
		   this <- getThis
	           ps <- getPropertyNames (toObjId this)
                   let ps' = map (\x -> (inj (x::String)::Value)) ps
                   o <- newArrayObject ps'
                   return $ ((inj (o::ObjId))::Value)

toString' :: InterpM Value
toString' = do 
                 this <- getThis
                 ts <- getProperty (toObjId this) "Value"
                 case ts == undefinedValue of 
                      True ->  return $ inj "[object]"
                      False -> toString ts

--                 toString ts
--                 case ts of 
--                        undefinedValue -> return $ inj "[an object]"
--                        _ -> toString ts

hasOwnProperty :: InterpM Value
hasOwnProperty = do
                  this <- getThis
                  (arg:_) <- getArgs
                  o <- getObject (prjObjId this)
                  s <- toRealString arg
                  return $ inj $ elem s (M.keys $ properties o)
