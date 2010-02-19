module HJS.Interpreter.Interp where

import HJS.Interpreter.InterpMDecl

callIt :: Int -> Int

callFunction :: Value -> [Value] -> ObjId -> InterpM Value