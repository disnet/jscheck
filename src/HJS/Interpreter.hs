{-# OPTIONS -fglasgow-exts #-}
module HJS.Interpreter where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

import qualified Data.Map as M

import HJS.Interpreter.InterpMDecl
import HJS.Interpreter.InterpM
import HJS.Interpreter.Interp
import HJS.Interpreter.Host

import Debug.Trace

runInterp :: InterpC p => p -> InterpM Value
runInterp p = do
	          initEnvironment
                  interp p



                 

startState f = let db = if elem Debug f then [DBBreak (-1)] else []
               in emptyState { flags = f, debug = db }

runProgram' f p = runStateT  (runErrorT (runInterp p)) (startState f)

runProgram :: InterpC a => [RunFlag] -> a  -> IO Bool
runProgram flags  p = do 
                           (ret,state) <- runProgram' flags p
			   case ret of 
                                 Left err -> do 
                                                putStrLn $ "Position: " ++ (show $ pos state) 
                                                putStrLn (show err ) 
                                                handleRunResult flags (undefinedValue, state)
                                 Right r  -> handleRunResult flags (r,state)


                            
handleRunResult (ShowHeap : cs) (v,st) = do 
                                              let ec = ctx st
                                                  s = oheap st
                                                  p = pos st
 				              let out = getOut s
					      putStrLn $ "Return: " ++ (show v)
                                              putStrLn $ "Output: " ++ (show out)
                                              putStrLn $ "Execution Context" ++ (show ec)
					      putStrLn $ "Heap: "
				              mapM_ (\(i,o) -> do 
                                                  putStr  $ (show i) ++ " ->  "
						  putStrLn (show o)) (M.toList s)
                                              case (prj v) of 
                                                   (Just b :: Maybe Bool) -> return b
                                                   _ -> return False



handleRunResult [] (v,st) = do
				              let ec = ctx st
                                                  s = oheap st
                                                  p = pos st
 				              let out = getOut s
                                              putStrLn $ show $ debug st
                                              case out of 
                                                   Nothing -> return ()
					           (Just out') -> putStrLn $ "Output: " ++ (show out')
					      case (prj v) of 
                                                   (Just b :: Maybe Bool) -> return b
                                                   _ -> return False

handleRunResult (f:fs) (v,st) = handleRunResult fs (v,st)

getOut s = out where (out::Maybe String) = do  
	                                     go <- M.lookup globalObj s
                                             (o,_) <- M.lookup "_output" (properties go)
                                             return $ show o