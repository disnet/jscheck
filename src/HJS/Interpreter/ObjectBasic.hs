module HJS.Interpreter.ObjectBasic where

import HJS.Interpreter.InterpMDecl
import HJS.Interpreter.InterpM

newObject name = do
		   o <- newObjectRaw name
                   putPropertyInternal o "__proto__" (inj $ ObjId 2)
	           return o
