module Checker where
import HJS.Parser
import HJS.Parser.JavaScript
import Text.ParserCombinators.Parsec.Error
import Data.List

-- a simple environment containing only one method.   
-- environment = [ XType, XType, XType]
-- XType { typeName :: String, typeFields :: [String] } deriving (Show, Eq) 
             
-- talk to the environment (env) and see if k is defined, and whether k
-- has a method b.
typecheck :: (Eq k, Eq b) => k -> b -> [(k, [b])] -> Bool
typecheck key beh env = case lookup key env of
                    Just e -> elem beh e
                    Nothing -> False 


statementCheck (v:vs) (y:ys) = case y of
    Stmt (StmtPos p s) -> case s of 
        ExprStmt (AssignE (CondE (AExpr (AEUExpr (PostFix (LeftExpr (CallExpr (CallPrim (MemberCall me e))))))))) -> case me of
            MemPrimExpr (Ident id) -> if v==id
                                      then typecheck id e [("x", ["boo","foo","oo"]), ("y", ["bo","fo","o"])] && if null ys 
                                                                                         then True
                                                                                         else statementCheck vs ys
                                      else True
            _ -> False 
        _ -> False    

                        
class CheckC t where
  check :: t -> Bool

instance CheckC SourceElement where
  check (Stmt s) = check s 
  check _ = True

instance CheckC Stmt where
  check (StmtPos p s) = check s 

instance CheckC Stmt' where
  check (VarStmt (v:vs)) = check v 
  check (ExprStmt p) = check p
  check (EmptyStmt) = True
  
instance CheckC VarDecl where
  check (VarDecl s (Just e)) = True -- 4th old True
  check (VarDecl s Nothing)  = typecheck s "boo" [("x", ["boo","foo","oo"]), ("y", ["bo","fo","o"])] 
                    

instance CheckC MemberExpr where
  check (MemberCall me e) = case me of
                                MemberCall me e -> case me of
                                    MemPrimExpr (Ident s) -> typecheck s e [("x", ["boo","foo","oo"]), ("y", ["bo","fo","o"])]
                                    _ -> False                                     
                                _ -> False
  check (MemPrimExpr p)   = case p of 
                                PEFuncDecl e -> case e of -- begin (1)
                                     FuncDecl (Just a1) (Just a2) v y -> statementCheck v y
                                     _ -> False
                                _ -> False
                                                  
  check (ArrayExpr me e)  = False
  check (MemberNew me e)  = False

      
instance CheckC Expr where
  check (AssignE p) = check p

  
instance CheckC AssignE where
  check (CondE s) = check s
  check (Assign left AssignNormal right) = (check left) && (check right)
  check (Assign left op right) = (check left) && (check right)
  check _ = False
 

instance CheckC CondE where
  check (AExpr e) = check e
  
instance CheckC AExpr where
   check (AEUExpr e) = check e
   
instance CheckC UExpr where
   check (PostFix p) = check p
      
instance CheckC PostFix where
   check (LeftExpr l) = check l
 
instance CheckC LeftExpr where
  check (CallExpr x) = check x
 
instance CheckC CallExpr where
  check (CallPrim p) = check p
  check (CallMember m args) = check m

                    
checkProgram :: String -> Either ParseError [SourceElement]
checkProgram input = parseProgram input

-- testing method
runChecker = do case checkProgram "function(x, y){x.boo;y.fo;};" of
                  Right (r:rs) -> check r
                  Left l -> False  
