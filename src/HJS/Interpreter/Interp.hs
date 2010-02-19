{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances  #-}
{-# OPTIONS -fallow-incoherent-instances #-}
module HJS.Interpreter.Interp where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State


import HJS.Parser.JavaScript
import HJS.Interpreter.InterpMDecl
import HJS.Interpreter.InterpM

import HJS.Interpreter.ObjectBasic
import HJS.Interpreter.Object
import HJS.Interpreter.Array
import HJS.Interpreter.Regex
import HJS.Interpreter.Debugger

import Data.Map (Map,fromList,lookup,empty,insert)


data MyError = NoMsg | Msg String deriving Show

instance Error MyError where
	 noMsg = NoMsg
	 strMsg = Msg 		 
    


class InterpC t where
    interp  :: t -> InterpM Value


instance InterpC Null where
   interp Null = return $ inj Null   

instance InterpC a => InterpC (Maybe a) where
   interp (Just x) = interp x
   interp _ = return $ inj Null

instance InterpC a => InterpC [a] where
   interp [] = return $ undefinedValue
   interp (x:[]) = interp x
   interp (x:xs) = do 
		       interp x
                       interp xs

instance (InterpC t1, InterpC t2) => InterpC (Either t1 t2) where
   interp (Left x) = interp x
   interp (Right x) = interp x

instance InterpC Literal where
   interp (LitInt i) = unitInj i
   interp (LitString s) = return $ inj s
   interp (LitBool b) = return $ inj b
   interp (LitNull) = return nullValue


pnameToString (PropNameId s) = s
pnameToString (PropNameStr s) = s
pnameToString (PropNameInt i) = show i

instance InterpC PrimExpr where
   interp (Literal l) = interp l
   interp (Ident s)   = return $ inj (Ref s)
   interp (Brack e) = interp e
   interp (PEFuncDecl f) = interp f
   interp This = getThis
   interp (Array (ArrSimple a)) = do
                            vs <- mapM interp a
                            o <- newArrayObject vs
                            return $ inj o
   interp (Regex (p,f)) = do
		        newRegex (inj p) (inj f)
   interp (HJS.Parser.JavaScript.Object ls) = do
                            f <- getValue (inj $ Ref "Object")
                            o <- newCall f []
                            mapM_ (toProperty o) ls 
                            return o
                         
                            
   interp x = throwInternalError $ "Cannot interp PrimExpr " ++ (show x)

toProperty :: Value -> (Either (PropName, AssignE) GetterPutter) -> InterpM ()
toProperty o x = case x of 
                       Left (n,e) -> do
				        v <- interp e
					putProperty (toObjId o) (pnameToString n) v
				        return ()
                       _ -> return ()



instance InterpC MemberExpr where
   interp (MemPrimExpr p) = interp p
   interp (ArrayExpr me e) = do
			        o <- interp me >>= getValue
                                e' <- interp e
                                x <- toRealString e'
                                case (prj o) of 
                                    (Just (o::ObjId)) -> return (inj $ RefObj o x)
				    _ -> throwInternalError $ "Invalid array access: " ++ (show o)


--                                o <- interp me >>= getValue
--                                e' <- interp e
--                                case (prj o) of 
--                                    (Just (o::ObjId)) -> getProperty o pname(toRealString e')
--				    _ -> throwInternalError $ "Invalid array access" ++ (show o)

   interp (MemberNew me args) =  do
                                oref <- interp me >>= getValue
                                args' <- mapM interp args
                                newCall oref args'

--                                case (prj o) of 
--                                    (Just (o::ObjId)) -> do 
--							  args' <- mapM interp args
--                                                          fo <- getProperty o "Construct"
--                                                          callFunction fo args' o
--                                    _ -> throwInternalError $ "Type Error: Not an Object" ++ (show o)

   interp (MemberCall me s) = do
                                  ro <- getRefObj me s
                                  return $ inj ro


getRefObj me s = do 
                    o <- interp me >>= getValue
                    case (prj o) of 
                         (Just (o::ObjId)) -> return (RefObj o s)
                         _ -> throwInternalError $ "Not an object" ++ (show o)



instance InterpC NewExpr  where
   interp (MemberExpr  p) = interp p


-- LeftExpr will return a reference
instance InterpC LeftExpr where
   interp (NewExpr n) = interp n
   interp (CallExpr c) = interp c

instance InterpC CallExpr where
   interp (CallDot e s) = do
                              obj <- interp e
                              case (prj obj) of 
                                  (Just (i::ObjId)) -> return (inj $ RefObj i s)
                                  _ -> throwInternalError "Attempt to access property on a non-object"

--   interp (CallMember (MemberCall me s) args) = do
--                                  r@(RefObj o _) <- getRefObj me s
--                                  f <- getValue (inj r)
--                                  callFunction f args o

   interp (CallMember m args) = do
                                  r <- interp m
                                  args' <- mapM interp args
                                  case prj r of 
                                          Just (ref ::Ref) -> do 
							         f <- getValue r
                                                                 case ref of 
                                                                    RefObj t _ -> callFunction f args' t
                                                                    Ref t  -> callFunction f args' ObjIdNull
                                          _ -> throwInternalError "CallMember requires function reference"

   interp (CallPrim p) = interp p
   interp (CallCall m args) = do
                                  f <- interp m
                                  args' <- mapM interp args
                                  callFunction f args' ObjIdNull

   interp p = throwInternalError $ "Cannot interp " ++ (show p)

-- Call the function object with supplied arguments
callFunction :: Value -> [Value] -> ObjId -> InterpM Value
callFunction f args' this = do
                                  case (prj f) of 
                                        (Just (i::ObjId)) -> do
                                                         callFunction' i args' this
					_        -> throwInternalError $ "Internal Error: Invalid function object" ++ (show f)

-- Not used.
getCaller = do
               cargs <- getValue (inj $ Ref "arguments")
	       case cargs == undefinedValue of 
		  True -> return nullValue
                  False ->  getProperty (toObjId cargs) "callee" 

callFunction' i args' this = do 
                                                         c <- getCallee
                                                         putProperty i "caller" c
							 argo <- newObject "arguments"
						         act <- newObject "activation"
                                                         putProperty argo "callee" (inj i)
						         putProperty act "arguments" (inj argo)
                                                         sc' <- getProperty i "Scope"
                                                         let (Just (sc::[ObjId])) = (prj sc')
                                                         pushContext (act:sc,act,this,i)
                                                         putValue (inj $ Ref "arguments") (inj argo)
                                                         fargs' <- getProperty i "Args"
                                                         let (Just (fargs::[String])) = (prj fargs')
                                                         zipWithM (\x y -> putProperty argo (show x) y) [0..] args'
                                                         zipWithM (\x y -> putProperty act x y) fargs args'
                                                         putProperty argo "length" (inj (length args'))
                                                         se <- getProperty i "Call"
                                                         res <- (runFunc se) `catchError` handleReturn

                                                         popContext
                                                         return res
                                        



runFunc se = do
                 case (prj se) of
                               (Just (CallJS se)) ->  foldM (\v x -> interp x) (Left 0) se
                               (Just (CallBuiltIn f)) -> f
                               _ -> throwInternalError "Internal Error: Invalid function call block"


-- PostFix returns a real value, so this is where references get dereferenced
instance InterpC PostFix where
   interp (LeftExpr l) = do 
			    r <- interp l
                            getValue r

   interp (PostInc l) = do 
			    r <- interp l
                            v <- getValue r -- TODO fixme.
                            v' <- getValue r `bindPrj` \ i -> 
                                   unitInj (i+ 1)
                            putValue r v'
                            return v


instance InterpC UExpr where
   interp (PostFix p) = interp p
   interp (Not u ) = do u' <- interp u
                        b <- toRealBool u'
                        case b  of
                            True -> return $ inj False
                            _ -> return $ inj True
   interp (TypeOf p) = interp p >>= typeOfString
   interp (UnaryMinus p) = do x <- interp p
                              return $ inj $ ((-1) * (toRealInt x))
   interp x = throwInternalError $ "Internal Error: Cannot handle " ++ (show x)
                            
{-
instance InterpC AddExpr where
   interp (MultExpr x) = interp x
   interp (Plus x y) = liftIt (+) x y
   interp (Minus x y) = liftIt (-) x y

instance InterpC ShiftE where
   interp (AddExpr p) = interp p

instance InterpC RelE where
   interp (ShiftE p) = interp p
   interp (LessThan x y) = liftRel (<) x y
   interp (GreaterThan x y) = liftRel (>) x y 
   interp (LessEqual x y) = liftRel (<=) x y
   interp (GreaterEqual x y)  = liftRel (>=) x y


instance InterpC EqualE where
   interp (RelE p) = interp p
   interp (Equal x y) = liftRel (==) x y

instance InterpC BitAnd where
   interp (EqualE p) = interp p

instance InterpC BitXOR where
   interp (BitAnd p) = interp p

instance InterpC BitOR where
   interp (BitXOR p) = interp p

instance InterpC LogAnd where
   interp (BitOR p) = interp p
   interp (LALogAnd x y) =  liftBool (&&) x y



instance InterpC LogOr where
   interp (LogAnd p) = interp p
   interp (LOLogOr x y) =  liftBool (||) x y
--}

instance InterpC AExpr where
   interp (AEUExpr e) = interp e
   interp (AOp "-" x y) = liftIt (-) x y
   interp (AOp "+" x y) = liftIt22 (+) x y
   interp (AOp "*" x y) = liftIt (*) x y
   interp (AOp "&&" x y) = liftBool (&&) x y
   interp (AOp "==" x y) = do x' <- interp x; y' <- interp y; return $ inj $ abstractEquality x' y'
   interp (AOp "!=" x y) = do x' <- interp x; y' <- interp y; return $ inj $ not $ abstractEquality x' y'
   interp (AOp "===" x y) = do x' <- interp x; y' <- interp y; return $ inj $ strictEquality x' y'
   interp (AOp "<" x y) = liftRel (<) x y
   interp (AOp ">" x y) = liftRel (>) x y
   interp (AOp "<=" x y) = liftRel (<=) x y
   interp (AOp ">=" x y) = liftRel (>=) x y

   interp (AOp op x y) = throwInternalError $ "Operator not implemented: " ++ (show op)

liftIt22 :: (InterpC x, InterpC y) => (Int -> Int -> Int) -> x -> y -> InterpM Value
liftIt22 f x y = do
                    x' <- interp x >>= toPrimitive HNone
                    y' <- interp y >>= toPrimitive HNone
                    case typeOf x' == typeOf nullStringValue || typeOf y' == typeOf nullStringValue of
                           True -> do
                                      x'' <- toRealString x' 
                                      y'' <- toRealString x'
                                      return $ inj (x''  ++ y'')
                           False -> return $ inj $ f (toRealInt x') (toRealInt y')
                    

strictEquality x y = if typeOf x /= typeOf y then False
                     else if ( x == nullValue || y == undefinedValue) then True
                     else ((==) x y)

abstractEquality x y = if typeOf x == typeOf y then ((==) x y)
                       else ( x == nullValue && y == undefinedValue) || ( y == nullValue && x == undefinedValue)

--if  typeOf x == (typeOf nullValue) || typeOf x == (typeOf undefinedValue) then True else False

liftBool f x y = do
		    x' <- interp x >>= toRealBool
                    y' <- interp y >>= toRealBool
                    return $ inj $ (f x' y')

liftRel f x y =  do 
                    x' <- interp x 
                    y' <- interp y
                    return $ inj $ (f x' y')


instance InterpC CondE where
   interp (AExpr p) = interp p

instance InterpC AssignE where
    interp (CondE p) = interp p
    interp (Assign left AssignNormal right) = do 
		                                  v <- interp right
                                                  r <- interp left
                                                  putValue r v
                                                  return v
    interp (Assign left op right ) = do
                                            v <- interp right
                                            r <- interp left
                                            rval <- getValue r 
                                            v' <- case op of 
                                                        AssignOpPlus -> liftIt3 (+) rval v
                                            putValue r v'
                                            return v'
    interp (AEFuncDecl fd) = interp fd

instance InterpC FuncDecl where
    interp (FuncDecl (Just s) Nothing args ses) = do
                                            fo <- newFuncObject args ses (defaultConstructor "Object")
                                            putProperty fo "name" (inj s)
                                            putValue (inj (Ref s)) (inj fo)
                                            return (inj fo)
                   
  
instance InterpC Expr where
    interp (AssignE p) = interp p

instance InterpC VarDecl where
    interp (VarDecl s (Just e)) = do v <- interp e; putValue (inj (Ref s)) v; return v
    interp (VarDecl s Nothing) = do putValue (inj (Ref s)) (inj Undefined); return (inj Undefined)

instance InterpC IfStmt where
    interp (IfElse e s1 s2) = do
                                 b <- interp e >>= toRealBool
                                 case b of
                                       True  -> interp s1
                                       _ -> interp s2
    interp (IfOnly e s) = do 
                                 b <- interp e >>= toRealBool
                                 case b of
                                       True  -> interp s
				       _ -> return (inj Undefined)


-- handleBreakContinue 
handleBreakContinue (ThrowBreak s) = return (inj Break)
handleBreakContinue (ThrowContinue s) = return (inj (0::Int))
handleBreakContinue e = throwError e
                              
                          

-- TODO Need to syntacticall check break/continue are within It statement
--      Handle values of the ItStmt
--      Push a context so that vars local to this are lost when leaving.
instance InterpC ItStmt where
    interp (DoWhile s e ) = do
                            vv <- (interp s) `catchError` handleBreakContinue 
                            case (prj vv) of
                                  (Just Break)  -> return (inj (0::Int))
                                  _   -> do 
                                            b <- interp e >>= toRealBool
                                            case b of
                                                  False -> return vv
                                                  _ -> interp (DoWhile s e )
    interp (While e s) = do
                            b <- interp e >>= toRealBool
			    case b of
                                 False -> return (inj Undefined) -- FIXME 
                                 _ -> do
                                         vv <- (interp s) `catchError` handleBreakContinue
                                         case (prj vv) of
                                                (Just Break)  -> return (inj (0::Int))
                                                _   -> interp (While e s)
    interp (For e1 e2 e3 s) = interpFor e1 e2 e3 s
    interp (ForVar e1 e2 e3 s) = interpFor e1 e2 e3 s 
    interp (ForIn e1 e2 s ) = interpForIn e1 e2  s 

-- Note that e2 is not evaluated each time around the loop. TODO - check this.
interpForIn e1 e2 s = do
                          v <- interp e1
                          e <- interp e2
			  ps <- getPropertyNames (toObjId e)
                          mapM (\p -> do putValue v (inj p); interp s) ps
			  return (undefinedValue)
                          


interpFor e1 e2 e3 s = do
                          interp e1
                          b <- interp e2 >>= toRealBool
                          case b of 
					False -> return (inj Null)
                                        _ -> do
                                                vv <- interp s `catchError` handleBreakContinue
						case (prj vv) of
                                                    (Just Break) -> return (inj (0::Int))
                                                    _ -> do  interp e3; interp (For Nothing e2 e3 s)
                                
                                


interpList :: InterpC a => [a] -> InterpM Value
interpList (x:[]) = interp x

                     
interpList (x:xs) = do
                       interp Null
                       interp x
                       interpList xs
--
--interpList (x:[]) = do
--                        interp x

instance InterpC Stmt where
    interp (StmtPos p s) = do 
  		              putPosition p
                              debugPoint p
 		              interp s

instance InterpC Stmt' where
    interp (ExprStmt p) = interp p
    interp (IfStmt p) = interp p
    interp (Block xs) = interp xs 
    interp (ItStmt p) = interp p
    interp (ReturnStmt (Just p)) = interp p >>= throwReturn
    interp (ReturnStmt Nothing) = throwReturn (inj Undefined)
    interp (BreakStmt s) = throwBreak s
    interp (ContStmt s) = throwContinue s
    interp (EmptyStmt) = return $ inj Undefined
    interp (VarStmt v) = do vs <- mapM interp v; return $ head vs
    interp (ThrowExpr e) = interp e >>= throwException
    interp (TryStmt e) = interp e
    interp (Switch e s) = do
                             x <- interp e
                             handleSwitch s x `catchError` handleBreakContinue

    interp s = error $ "Missing Stmt handling" ++ (show s)

handleSwitch ((CaseClause e s):cs) x = do 
				            y <- interp e
                                            case abstractEquality x y of 
                                                 True -> do interp s; fallThruSwitch  cs
                                                 False -> handleSwitch cs x

handleSwitch ((DefaultClause s):cs) x = do interp s; fallThruSwitch cs

handleSwitch [] x = return $ undefinedValue -- FIXME Wrong if we want to handle values of stmts correctly

fallThruSwitch ((CaseClause e s):cs) = do interp s; fallThruSwitch cs
fallThruSwitch ((DefaultClause s):cs) = do interp s; fallThruSwitch cs
fallThruSwitch [] = return $ undefinedValue

instance InterpC TryStmt where
    interp (TryTry s1 c s2) = interp s1 `catchError` (handleException c)

instance InterpC Catch where
    interp (Catch _ s) = interp s
    interp (CatchIf _ s e) = interp s 
    interp (CatchCatch i _ s) = interp s

-- TODO - remove the putValue i
handleException ((CatchCatch i _ s):_) (ThrowException v) = do
                                                            putValue (inj $ Ref i) v
                                                            interp s
handleException _ e = throwError e

instance InterpC SourceElement where
    interp (Stmt s) = interp s
    interp (SEFuncDecl fd) = interp fd

instance InterpC JSProgram where
    interp (JSProgram xs) = interp xs


-- This needs to have sub -> sub1 -> Int otherwise get problem with bindPrj.
liftIt ::(SubType sub Value, SubType sub1 Value, InterpC t1, InterpC t) => (sub -> sub1 -> Int ) -> t -> t1 -> InterpM Value
liftIt f x y = interp x `bindPrj`\i -> 
               interp y `bindPrj` \j -> 
                   (return . inj) ((f i j))

liftIt3 ::(SubType sub Value, SubType sub1 Value) => (sub -> sub1 -> Int ) -> Value -> Value-> InterpM Value
liftIt3 f x y = case (prj x) of 
                    (Just x') -> case (prj y) of 
                                     (Just y') -> return $ inj $ f x' y'
                                     _ -> throwInternalError "Cannot prj"
                    _ -> throwInternalError "Cannot prj"

liftIt2 g x y = interp x `bindPrj`\i -> 
               interp y `bindPrj` \j -> g i j 



{--
instance InterpC MultExpr where
   interp (UExpr x) = interp x
   interp (Times x y) = liftIt (*) x y
   interp (Div x y) =  liftIt2 (\i j -> 
			   if j == 0 then 
                                 throwInternalError "Run Time Error: Divide by Zero"
                           else 
                                 unitInj ((i `div` j )::Int)) x y

   interp (Mod x y) =  liftIt2 (\i j -> 
			   if j == 0 then 
                                 throwInternalError "Run Time Error: Divide by Zero"
                           else 
                                 unitInj ((i `mod` j )::Int)) x y
--}
unitInj = return . inj

m `bindPrj` k = 
    m >>= \a -> 
	case (prj a) of
		     Just x -> k x
		     Nothing -> (throwInternalError $ "Internal Error: Cannot prj Value" ++ (show a))


defaultConstructor :: String -> InterpM Value
defaultConstructor name = do 
                        o <- newObject name
			t <- getThis
                        args <- getArgs
		        p <- getProperty (toObjId t) "prototype"
                        putProperty o "__proto__" p 
                        callFunction' (toObjId t) args o
                        return $ inj o


x :: Value
x = inj (ObjId 1)

callIt :: Int -> Int
callIt _ = 99
