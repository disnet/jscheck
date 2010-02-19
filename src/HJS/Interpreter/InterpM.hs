{-# LANGUAGE OverlappingInstances, IncoherentInstances, FlexibleInstances,
  MultiParamTypeClasses, PatternSignatures, TypeSynonymInstances #-}
module HJS.Interpreter.InterpM where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import System.IO.Unsafe
import Debug.Trace
import HJS.Interpreter.Printf


import HJS.Parser.JavaScript hiding (Object)

import HJS.Interpreter.InterpMDecl
import {-# SOURCE #-} HJS.Interpreter.Interp

import Data.Map as M
import Data.List
{-

               VALUE

-}

data PrimHint = HString | HNumber | HNone

-- These correspond to the ToX functions covered in the specification.
-- Converting from one type to another remaining within the Value type
class Convert a where
   typeOf :: a -> [Int]
   typeOf _ = []
   toBoolean :: a -> InterpM Value
   toBoolean _ = return $ inj False
   toNumber :: a -> Value
   toNumber _ = inj (0::Int)
   toString :: a -> InterpM Value
   toString _ = return $ inj ""
   toPrimitive :: PrimHint -> a  -> InterpM Value
   toPrimitive  _ i = return $ undefinedValue



instance (Convert a, Convert b) => Convert (Either a b) where
   toBoolean (Left x) = toBoolean x
   toBoolean (Right x) = toBoolean x
   toNumber (Left x) = toNumber x
   toNumber (Right x) = toNumber x
   toString (Left x) = toString x
   toString (Right x) = toString x
   typeOf (Left x) = 0:typeOf x
   typeOf (Right y) = 1:typeOf y
   toPrimitive h (Left x) = toPrimitive h x
   toPrimitive h (Right x) = toPrimitive h x



instance Convert Int where
   toBoolean 0 = return $ inj False
   toBoolean _ = return $ inj True
   toNumber i = inj (i::Int)
   toString i = return $ inj (show i)
   toPrimitive _ i = return $ inj i


instance Convert String where
   toBoolean "" = return $ inj False
   toBoolean _ = return $ inj True
   toNumber s = inj $ ((read s)::Int)
   toString s = return $ inj s
   toPrimitive _ i = return $ inj i

instance Convert Bool where
   toBoolean b = return $ inj b
   toNumber False = inj $ (0::Int)
   toNumber True = inj $ (1::Int)
   toString False = return $ inj "false"
   toString True = return $ inj "true"
   toPrimitive _ i = return $ inj i


instance Convert Ref
instance Convert Null where
   toString _ = return $ inj "null"
   toPrimitive _ i  = return $ inj i



instance Convert Undefined where
   toString _ = return $ inj "undefined"
   toPrimitive _ i = return $ inj i


instance Convert ()

instance Convert [Int] where
   toString i = return $ inj (show i)

instance Convert [SourceElement] where
   toString _ = return $ inj "<code>"

instance Convert [String] where
   toString ss = return $ inj (show ss)

instance Convert CallValue

instance Convert ObjId where
   toString o = do
                    v <- getProperty o "toString"
                    case v == undefinedValue of
                                False -> callFunction v [] o
                                True -> return $ inj "[object]"

   toPrimitive h o   = do
                          m <- getProperty o "toString"
                          case m == undefinedValue of
                                False -> callFunction m [] o
                                True -> return undefinedValue
   toBoolean (ObjId i) = return $ inj True
   toBoolean _ = return $ inj False


instance Convert [ObjId]

instance Ord SourceElement where
    compare _ _ = EQ

instance Eq SourceElement where
    (==) _ _ = True

class SubType sub sup where
    inj :: sub -> sup
    prj :: sup -> Maybe sub

instance SubType a (Either a b) where
   inj = Left
   prj (Left x) = Just x
   prj _ = Nothing

instance SubType a b => SubType a (Either c b) where
   inj = Right . inj
   prj (Right a) = prj a
   prj _ = Nothing

instance SubType sub () where
   inj _ = ()
   prj _ = Nothing

toObjId = prjObjId

prjObjId :: Value -> ObjId
prjObjId v = case prj v of
                      (Just s) -> s
                      _        -> error $ "Impossible Error converting to ObjId" ++ (show v)

prjInt :: Value -> Int
prjInt v= case prj v of
                      (Just s) -> s
                      _        -> error "Impossible Error"

prjBool :: Value -> Bool
prjBool v= case prj v of
                      (Just s) -> s
                      _        -> error "Impossible Error"

prjString :: Value -> String
prjString v = case prj v of
                      (Just s) -> s
                      _        -> error "Impossible Error"

toValueList :: Value -> [Value]
toValueList v = case prj v of
                      (Just (s::[Value])) -> s
                      _        -> error "Impossible Error"

toRealInt :: Convert a => a -> Int
toRealInt v = case prj (toNumber v) of
                      (Just s) -> s
                      _        -> error "Impossible Error"


toRealString :: Convert a => a -> InterpM String
toRealString v = do
                    s <- toString v
                    return $ prjString s

toRealBool :: Convert a => a -> InterpM Bool
toRealBool v = do
                  b <- toBoolean v
                  return $ prjBool b


typeOfString :: Value -> InterpM Value
typeOfString v | typeOf v == typeOf undefinedValue = return $ inj "undefined"
typeOfString v | typeOf v == typeOf nullValue = return $ inj "object"
typeOfString v | typeOf v == typeOf trueValue = return $ inj "boolean"
typeOfString v | typeOf v == typeOf zeroValue = return $ inj "number"
typeOfString v | typeOf v == typeOf nullStringValue = return $ inj "string"
typeOfString v | typeOf v == typeOf nullObjValue = return $ inj "object"
typeOfString _ = return $ inj "unknown"



instance Convert BreakContinue

nullValue :: Value
nullValue = inj Null

undefinedValue :: Value
undefinedValue = inj Undefined

trueValue :: Value
trueValue = inj True

zeroValue :: Value
zeroValue = inj (0::Int)

nullStringValue :: Value
nullStringValue = inj ""

nullObjValue :: Value
nullObjValue = inj ObjIdNull


{-
             OBJECT

-}






getObject :: ObjId -> InterpM Object
getObject i = do
                   s  <- get
                   case M.lookup i (oheap s) of
                      Just o -> return o
                      Nothing -> throwInternalError "Object not found"

putObject :: Object -> InterpM ()
putObject o = do
                   s <- get
                   put s { oheap = M.insert (idd o) o (oheap s) }


getObjectProperty' :: Object -> String -> InterpM (Maybe ObjId)
getObjectProperty' o k = do
                            v <- getProperty' o k
                            case (prj v) of
                                  Just (id::ObjId) -> return $ Just id
                                  Nothing -> return Nothing

getPrototypeObject' o = do
                            case M.lookup "__proto__" (properties o) of
                                    Just (v,_) -> do
                                                      case prj v of
                                                          Just (id::ObjId) -> return $ Just id
                                                          Nothing -> return Nothing
                                    Nothing -> return Nothing


getProperty' :: Object -> String -> InterpM Value
getProperty' o k = do
                       case M.lookup k (properties o) of
                        Just (v,_) -> return v
                        Nothing -> do
                                     proto <- getPrototypeObject' o
                                     case proto  of
                                                  Nothing -> return $ inj Undefined
                                                  Just p -> getProperty p k

getProperty :: ObjId -> String -> InterpM Value
getProperty id k = do
                     traceM $ printf "getProperty %s %s" id k
                     o <- getObject id
                     getProperty' o k


putPropertyInternal id k v = do
                             o <- getObject id
                             cp <- canPut o k
                             case cp of
                                       True ->  do
                                             let o' = o { properties = M.insert k (v,[DontEnum]) (properties o)}
                                             putObject o'
                                       False -> return ()

putProperty :: ObjId -> String -> Value -> InterpM ()
putProperty id k v = do
                         traceM $ printf "putProperty %s %s" id k
                         o <- getObject id
                         case klass o of
                                "Array" -> putArrayProperty id k v
                                _ -> putObjectProperty id k v

putObjectProperty id k v = do
                                       o <- getObject id
                                       cp <- canPut o k
                                       case cp of
                                               True ->  do
                                                             let o' = o { properties = M.insert k (v,[]) (properties o)}
                                                             putObject o'
                                               False -> return ()

toUInt32 :: String -> Int
toUInt32 s = read s

putArrayProperty id k v = do
                                 o <- getObject id
                                 cp <- canPut o k
                                 case cp of
                                     True ->  do
                                                 len <- getProperty id "length"
                                                 case prj len of
                                                      (Just (l::Int)) -> do
                                                                             let idx = toUInt32 k
                                                                                 newlen = if idx < l then l else idx + 1
                                                                             putObjectProperty id k v
                                                                             putObjectProperty id "length" (inj newlen)
                                                      _ -> throwInternalError $ "Invalid value for array length: " ++ (show len)
                                     False -> return ()


checkAttr :: Object -> String -> Attribute -> InterpM Bool
checkAttr o k attr = do attrs <- getAttr o k; return (elem attr attrs)

getAttr :: Object -> String -> InterpM [Attribute]
getAttr o k = case M.lookup k (properties o) of
                 Just (_,a) -> return a
                 Nothing -> case prototype o of
                              Nothing -> return []
                              Just p -> do
                                           p' <- getObject p
                                           getAttr p' k

putAttr o k attr = case M.lookup k (properties o) of
                 Just (v,attrs) -> do
                                      let o' = o { properties = M.insert k (v,attr:attrs) (properties o)}
                                      putObject o'
                 Nothing -> case prototype o of
                              Nothing -> return ()
                              Just p -> do
                                           p' <- getObject p
                                           putAttr p' k attr


canPut o k = do f <- checkAttr o k ReadOnly; return (not f)
canEnum o k = do f <- checkAttr o k DontEnum; return (not f)


hasProperty :: Object -> String -> Bool
hasProperty o k =  M.member k (properties o)

deleteProperty :: Object -> String -> Object
deleteProperty o k = o { properties = M.delete k (properties o) }

-- Creates a new raw object and inserts it into the object store
newObjectRaw :: String -> InterpM ObjId
newObjectRaw k = do
               traceM $ printf "newObjectRaw %s" k
               s <- get
               let os = oheap s
               let id' = ObjId $ (size os)+1
                   o = Object { idd = id', prototype = Nothing, klass=k, value=Nothing, properties = M.empty }
               putObject o
               return id'



{-
          VALUES
-}


-- Get value from environment if Value is a reference, return argument if it isn't, throwError if ref
-- doesn't reference anything.

-- Get the get function being called
getCallee:: InterpM Value
getCallee = do
                    s <- get -- (((_,_,_,c):_), _,_) <- get
                    let ((_,_,_,c):_) = ctx s
                    case c of
                        ObjIdNull -> return nullValue
                        _ -> return $ inj c

getStmtLine :: InterpM Int
getStmtLine = do
                s <- get
                let (l,c) = pos s
                return l

putDebugFlags :: [DebugAction] -> InterpM ()
putDebugFlags d = do
                s <- get
                put $ s { debug=d }

getDebugFlags :: InterpM [DebugAction]
getDebugFlags = do
                s <- get
                return $ debug s

getFlags :: InterpM [RunFlag]
getFlags = do
                s <- get
                return $ flags s

getThis :: InterpM Value
getThis = do
          s <- get
          let (_,_,t,_):_ = ctx s
          return $ inj t

getValue :: Value -> InterpM Value
getValue v = do
                 sc <- getScopeChain
                 case prj v of
                     ((Just (Ref s))::Maybe Ref) -> getValue' sc s
                     ((Just (RefObj o s))::Maybe Ref) -> getProperty o s
                     _ -> return v

getObjectValue :: Value -> InterpM ObjId
getObjectValue v = do
                       ov <- getValue v
                       case prj ov of
                             (Just (o::ObjId)) -> return o
                             _ -> throwInternalError "Expecting to get an object value"

getValue' :: [ObjId] -> String -> InterpM Value
getValue' (c:cs) s = do
                        p <- getProperty c s
                        case prj p of
                            ((Just Undefined)::Maybe Undefined) -> getValue' cs s
                            _ -> return p

getValue' [] s = return $ inj Undefined


-- Put value into environment if Value is a reference, throw error if it isn't
putValue :: Value -> Value -> InterpM ()
putValue r v = do
                  s <- get
                  let (_,vo,_,_):_ = ctx s
                  case prj r of
                        ((Just (Ref s))::Maybe Ref) -> putProperty vo s v
                        ((Just (RefObj o s))::Maybe Ref) -> putProperty o s v
                        _ -> throwInternalError "Internal Error: Invalid Reference"

setPrototype o = do
                    c <- getThis
                    p <- getProperty (toObjId c) "prototype"
                    putPropertyInternal o "__proto__" p

setFuncProto fo  = do
                        v <- getValue  (inj $ Ref "Function")
                        case prj v of
                             (Just (o::ObjId)) -> do getProperty o "prototype" >>= putProperty fo "__proto__"; return ()
                             Nothing -> return $ inj (ObjId 2)

getScopeChain = do
                   s <- get
                   let (sc,_,_,_):_ = ctx s
                   return sc

newFuncObject :: [String] -> [SourceElement] -> InterpM Value -> InterpM ObjId
newFuncObject args ses fc = do
                             sc <- getScopeChain
                             fo <- newObjectRaw "Function"
                             setFuncProto fo
                             p <- newObjectRaw "Object"
                             putPropertyInternal p "__proto__" (inj $ ObjId 2)
                             putProperty fo "prototype" (inj p)
                             putProperty fo "Scope" (inj sc)
                             putProperty fo "Call" (inj (CallJS ses))
                             putProperty fo "Args" (inj args)
                             cf <- newBuiltInFunction [] fc
                             putProperty fo "Construct" (inj cf)
                             return fo

newBuiltInFunction :: [String] -> InterpM Value -> InterpM ObjId
newBuiltInFunction args f = do
                             sc <- getScopeChain
                             fo   <- newObjectRaw "Function"
                             p <- newObjectRaw "Object"
                             putProperty fo "prototype" (inj p)
                             putProperty fo "Scope" (inj sc)
                             putProperty fo "Call" (inj (CallBuiltIn f))
                             putProperty fo "Args" (inj args)
                             return fo


{-
-- Some test objects

newObject = Object { prototype = Nothing, klass="Object", value=Nothing, properties = M.empty }

objectHeap = M.empty

o1 = newObject { klass="Top" }
o2 = newObject { klass="Child", prototype=Just o1}

o1' = putProperty o1 "x" (inj (1::Int)::Value)
-}




throwReturn v = throwError (ThrowReturn v)
throwInternalError s = throwError (ThrowInternalError s)
throwBreak s = throwError (ThrowBreak s)
throwContinue s = throwError (ThrowContinue s)
throwTypeError s = throwError (ThrowTypeError s)
throwException v = throwError (ThrowException v)

handleReturn (ThrowReturn v) = return v
handleReturn s = throwError s

handleBreak (ThrowBreak s) = return False
handleBreak e = throwError e

handleContinue (ThrowContinue s) = return True
handleContinue e = throwError e



-- The order of Monad composition is important. If you don't then when
-- throwing you lose state
-- type InterpM  = ErrorT Throwable (StateT JSState Identity)
--type InterpM  = StateT JSState (ErrorT Throwable Identity)

instance Error Throwable where
   noMsg = ThrowInternalError "An Error"
   strMsg s = ThrowInternalError s

emptyState = JSS { ctx = [] , oheap = M.empty, pos = (0::Int,0::Int), flags = [], debug=[]}

--putGlobalContext :: InterpM ()
--putGlobalContext = do
--                     put globalContext

putPosition p = do
                    s <- get
                    put s { pos = p }

pushContext :: Ctx -> InterpM ()
pushContext c = do
                  s <- get
                  let cs = ctx s
                  put s { ctx = c:cs }

popContext :: InterpM ()
popContext = do
                s <- get
                let (c:cs) = ctx s
                put s { ctx = cs }

--globalContext = ( [([ObjId 1],ObjId 1,ObjId 1)], M.fromList [(ObjId 1, Object { idd = ObjId 1, prototype = Nothing, klass="_global", value=Nothing, properties = M.empty })])



-- instance Show a => Show (InterpM a) where
--      show s = show $ runIt s


getArgs :: InterpM [Value]
getArgs = do
             arg' <- getValue (inj $ Ref "arguments")
             let arg = prjObjId arg'
             len <- getProperty arg "length"
             mapM (\x -> getProperty arg (show x)) [0.. (toRealInt len)-1]

getPropertyNames :: ObjId -> InterpM [String]
getPropertyNames id = do
                             o <- getObject id
                             getPropertyNames' o

getPropertyNames' :: Object -> InterpM [String]
getPropertyNames' o  = do
                               let ks = keys $ properties o
                               ks <- filterM (canEnum o) ks
                               p <- getProperty' o "__proto__"
                               case prj p of
                                    (Just (i::ObjId)) -> do
                                                             ks' <- getPropertyNames i
                                                             return $ Data.List.union ks ks'
                                    _ -> return ks





-- All internal new object calls actions must go through this.
newCall name args = do
                        o <- getValue name
                        case (prj o) of
                                    (Just (o::ObjId)) -> do
                                                          fo <- getProperty o "Construct"
                                                          callFunction fo args o
                                    _ -> throwInternalError $ "Type Error: Not an Object" ++ (show o)


isUndefined x = x == undefinedValue

--argsListS :: forall a r. (Show a, BuildList a r) => a -> r
--argsListS x = Data.List.map show $ argsList x

traceM :: String -> InterpM ()
traceM s = do
               f <- getFlags
               case elem Trace f of
                   True -> liftIO $ putStrLn s
                   False -> return ()

