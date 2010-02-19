{--
  A Simple Debugger for HJS. Makes no allowances for variations in entered text!
--}
module HJS.Interpreter.Debugger(debugPoint) where

import System.IO 
import Control.Monad.Trans
import Control.Monad.State 

import HJS.Interpreter.InterpMDecl hiding (Continue)
import HJS.Interpreter.InterpM

import {-# SOURCE #-} HJS.Interpreter.Eval


haltThisLine (StepOver:xs) (l,c)      = (True,xs)
haltThisLine (x@(DBBreak i):xs) (l,c) = if i == (-1) then (True,xs) else 
                                        if i == l then (True,x:xs) else let (f, xs') = haltThisLine xs (l,c) in (f,x:xs')
haltThisLine (x:xs) (l,c)             = let (f, xs') = haltThisLine xs (l,c) in (f,x:xs')
haltThisLine [] _                     = (False,[])


debugPoint :: (Int,Int) -> InterpM ()
debugPoint p = do
                 f <- getDebugFlags
		 -- liftIO $ putStrLn $ show (f,p)
                 let (b,f') = haltThisLine f p
                 putDebugFlags f'
		 case b of 
                    True -> debugLoop
                    False -> return ()


debugLoop :: InterpM ()
debugLoop = do
              l <- liftIO $ getDBLine
              f <- case l of
                     "c" -> doAction DBContinue
                     "s" -> doAction StepOver
                     ('p':[]) -> doAction PrintHeap
                     ('p':ls) -> doAction $ PrintObj (read ls)
                     "l" -> doAction PrintLine 
                     ('b':ls) -> doAction $ DBBreak (read ls)
                     ('e':ls) -> doAction $ Eval ls

              case f of 
                    True -> return ()
                    _ -> debugLoop

getDBLine :: IO String
getDBLine = do
            putStr "hjsd> "
            s <- hGetLine stdin
            return s 

doAction :: DebugAction -> InterpM Bool

doAction (Eval s) = do
                       v <- eval s
                       liftIO $ putStrLn $ show v
                       return False

doAction (DBBreak i) = do
                           f <- getDebugFlags
                           putDebugFlags ((DBBreak i):f)
                           return False

doAction (StepOver) = do
                        f <- getDebugFlags
                        putDebugFlags ((StepOver):f)
                        return True

doAction (PrintVar s) = do
                            v <- getValue (inj $ Ref s)
                            traceM (show v)
                            return False

doAction  (PrintHeap) = do
			   s <- get
		           liftIO $ putStrLn  (show $ oheap s)
                           return False
                            
doAction (PrintObj i) = do
                            o <- getObject (ObjId i)
                            liftIO $ putStrLn (show o)
                            return False

doAction (PrintLine ) = do
                            l <- getStmtLine
                            liftIO $ putStrLn $ "Current Line: " ++ (show l)
                            return False

doAction _ = return True

