module HJS.Interpreter.InterpMDecl where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State 

import Data.Map as M

import HJS.Parser.JavaScript hiding (Object)

data Ref = Ref String | RefObj ObjId String deriving (Eq,Show,Ord)

data ObjId = ObjId Int | ObjIdNull deriving (Eq,Show,Ord)

data Undefined = Undefined deriving (Eq,Show,Ord)
data Null = Null deriving (Eq,Show,Ord)

data RunFlag = Debug | Trace | ShowHeap deriving (Show,Eq)

-- Either JS source from a function or a built in function
data CallValue = CallJS [SourceElement] | CallBuiltIn (InterpM Value)

data BreakContinue = Break | Continue deriving (Show,Eq,Ord)

data ArgList = ArgList [String] | VarArgList

-- Think I should be including Ref (ie what is returned by LeftExpr) into the Value type.
type Value =  Either Int                       -- Primitive values
            ( Either String 
            ( Either Bool                              
            ( Either Undefined 
            ( Either Null 
                                               -- Internal values
            ( Either ObjId
            ( Either Ref                       -- Reference
            ( Either [ObjId]                   -- List of object ids
            ( Either CallValue                 -- Code, used 'Call' property of Function object. 
            ( Either [String]                  -- List of argument names
	    ( Either BreakContinue
              ()))))))))))


type InterpM  = ErrorT Throwable (StateT JSState IO) -- Identity)

-- Execution Context = Scope chain, variable object, this object, current function
type Ctx = ([ObjId],ObjId,ObjId,ObjId)
data JSState =  JSS { 
		     ctx :: [Ctx], 
		     oheap :: Map ObjId Object, 
		     pos :: (Int,Int), 
		     flags :: [ RunFlag ],
		     debug :: [DebugAction]
		     } deriving Show



data Throwable = ThrowReturn Value 
	       | ThrowBreak (Maybe String)
	       | ThrowContinue (Maybe String)
	       | ThrowException Value
	       | ThrowTypeError String
	       | ThrowInternalError String 
		 deriving Show

-- Section 8.6.2
data Object = Object {
                      idd :: ObjId,
		      prototype :: Maybe ObjId,
		      klass :: String,
		      value :: Maybe Value,
		      properties :: M.Map String (Value,[Attribute]) 
		     } deriving Show

data Attribute = ReadOnly | DontEnum | DontDelete | Internal deriving (Eq,Show)

instance Show CallValue where
   show (CallJS x) = show x
   show (CallBuiltIn _) = show "<builtin>"

instance Ord CallValue where
   compare _ _ = EQ

instance Eq CallValue where
   (==) _ _ = True

data DebugAction = DBBreak Int          -- Set a break point at a line
		  | DBContinue          -- Continue execution to next break point
		  | StepOver            -- Step over a statement
		  | StepInto            -- Step into a statement
		  | PrintObj Int        -- Print object with ID
		  | PrintHeap           -- Print the whole heap
		  | PrintVar String     -- Print variable
		  | PrintLine           -- Print current statement
		  | PrintStack          -- Print the stack
                  | Eval String 
		    deriving (Show,Eq)