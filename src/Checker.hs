module Checker where
import HJS.Parser
import HJS.Parser.JavaScript
import Text.ParserCombinators.Parsec.Error
import Data.List
import Extractor


-- result data type
data Result = Warning String | Good String | Error String deriving Show


-- environment = [ XType, XType, XType]
-- XType { typeName :: String, typeFields :: [String] } deriving (Show, Eq) 
                    
typecheck :: String -> String -> [XType] -> Bool 
typecheck k b [] = True
typecheck k b (e:es) = if (k == (typeName e))
                       then elem b (typeFields e)
                       else typecheck k b es

-- helper function that allow us to iterate over statements and sub-statements 
-- 
statementCheck (v:vs) (y:ys) en = case y of
    Stmt (StmtPos p s) -> case s of 
        ExprStmt (AssignE (CondE (AExpr (AEUExpr (PostFix (LeftExpr (CallExpr (CallPrim (MemberCall me e))))))))) -> case me of
            MemPrimExpr (Ident id) -> if v==id
                                      then typecheck id e en && if null ys 
                                                                then True
                                                                else statementCheck vs ys en
                                      else True
            _ -> True 
        ReturnStmt (Just (AssignE (CondE (AExpr (AEUExpr (PostFix (LeftExpr (CallExpr (CallPrim (MemberCall me e)))))))))) -> case me of
            MemPrimExpr (Ident id) -> if v==id
                                      then typecheck id e en && if null ys
                                                                then True
                                                                else statementCheck vs ys en
                                      else True
        _ -> True    

                      
class CheckC t where
  check :: t -> [XType] -> Bool

instance CheckC SourceElement where
  check (Stmt s) env = check s env 
  check _ _ = True

instance CheckC Stmt where
  check (StmtPos p s) env = check s env

instance CheckC Stmt' where
  check (VarStmt (v:vs)) env = check v env 
  check (ExprStmt p) env = check p env
  check (EmptyStmt) env = True
  
instance CheckC VarDecl where
  check (VarDecl s (Just e)) env = True
  check (VarDecl s Nothing) env = typecheck s "" env 
                    

instance CheckC MemberExpr where
  check (MemberCall me e) env = case me of
                                MemberCall me e -> case me of
                                    MemPrimExpr (Ident s) -> typecheck s e env
                                    _ -> True                                     
                                _ -> True
  check (MemPrimExpr p) env  = case p of 
                                PEFuncDecl e -> case e of
                                     FuncDecl (Just a1) a2 v y -> statementCheck v y env
                                     _ -> True
                                _ -> True
                                                  
  check (ArrayExpr me e) env  = True
  check (MemberNew me e) env  = True

      
instance CheckC Expr where
  check (AssignE p) env = check p env

  
instance CheckC AssignE where
  check (CondE s) env = check s env
  check (Assign left AssignNormal right) env = (check left env) && (check right env)
  check (Assign left op right) env = (check left env) && (check right env)
  check _ _ = True
 

instance CheckC CondE where
  check (AExpr e) env = check e env
  
instance CheckC AExpr where
   check (AEUExpr e) env = check e env
   
instance CheckC UExpr where
   check (PostFix p) env = check p env
      
instance CheckC PostFix where
   check (LeftExpr l) env = check l env
 
instance CheckC LeftExpr where
  check (CallExpr x) env = check x env
 
instance CheckC CallExpr where
  check (CallPrim p) env = check p env
  check (CallMember m args) env = check m env
  
  
parse_now :: [Char] -> ([SourceElement] -> Result) -> Result
parse_now p f = case parseProgram p of
                      Right r -> f r
                      Left l -> Error "Ouch, a type violation was found."
                      
-- todo(huascar) contact Tim and ask him about the annotations that would be used
-- to indicate types. are we gonna use them?
-- todo(Huascar) it seems that the extractor gets run after the check was executed. hmmm, this is bad.
-- sample data -- [XType {typeName="a", typeFields=["boo", "arrrg"]}, XType {typeName="b", typeFields=["wal", "tal"]}]
runChecker input  = parse_now input (\p -> 
        if (check (head p)(runExtractor p))
          then Good "good stuff"
          else Warning "bad stuff")
