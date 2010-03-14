module Checker where
import HJS.Parser
import HJS.Parser.JavaScript
import Text.ParserCombinators.Parsec.Error
import Data.List
import Extractor
import System.Environment
-- import Data.Typeable (Typeable, cast)
-- import Data.Maybe (fromMaybe)



-- toString :: (Show a, Typeable a) => a -> String
-- toString x = fromMaybe (show x) (cast x)



-- result data type
data Result = Warning String | Good String | Error String deriving Show

                    
typecheck :: String -> String -> [XType] -> Bool 
typecheck k b [] = True
typecheck k b (e:es) = if (k == (typeName e))
                       then elem b (typeFields e)
                       else typecheck k b es


statementCheck a2 (v:vs) (y:ys) en = case y of
    Stmt (StmtPos p s) -> case s of 
        ExprStmt (AssignE (CondE (AExpr (AEUExpr (PostFix (LeftExpr (CallExpr (CallMember (MemberCall me e) [])))))))) -> case me of
            MemPrimExpr (Ident id) -> if v==id
                                      then annotationCheck id e a2 en && if null ys
                                                                         then True
                                                                         else if null vs 
                                                                              then statementCheck a2 [v] ys en
                                                                              else statementCheck a2 vs ys en
                                      else True
        ExprStmt (AssignE (CondE (AExpr (AEUExpr (PostFix (LeftExpr (CallExpr (CallPrim (MemberCall me e))))))))) -> case me of
            MemPrimExpr (Ident id) -> if v==id
                                      then annotationCheck id e a2 en && if null ys -- promptLocation (annotationCheck id e a2 en && if null ys...)  ... means that is the whole if-then-else block
                                                                         then True
                                                                         else if null vs 
                                                                              then statementCheck a2 [v] ys en
                                                                              else statementCheck a2 vs ys en
                                      else True
            _ -> True 
        ReturnStmt (Just (AssignE (CondE (AExpr (AEUExpr (PostFix (LeftExpr (CallExpr (CallPrim (MemberCall me e)))))))))) -> case me of
            MemPrimExpr (Ident id) -> if v==id
                                      then annotationCheck id e a2 en && if null ys -- promptLocation (annotationCheck id e a2 en && if null ys...)  ... means that is the whole if-then-else block
                                                                         then True
                                                                         else if null vs 
                                                                              then statementCheck a2 [v] ys en
                                                                              else statementCheck a2 vs ys en
                                      else True
        ReturnStmt (Just (AssignE (CondE (AExpr (AEUExpr (PostFix (LeftExpr (CallExpr (CallMember (MemberCall me e)[]))))))))) -> case me of
            MemPrimExpr (Ident id) -> if v==id
                                      then annotationCheck id e a2 en && if null ys -- promptLocation (annotationCheck id e a2 en && if null ys...)  ... means that is the whole if-then-else block
                                                                         then True
                                                                         else if null vs 
                                                                              then statementCheck a2 [v] ys en
                                                                              else statementCheck a2 vs ys en
                                      else True           
        _ -> True
     


annotationCheck :: String -> String -> [TypeAnnotation] -> [XType] -> Bool
annotationCheck id b (x:xs) env = case x of
        TypeAnnotation typ arg -> if arg==id 
                                  then typecheck typ b env
                                  else if null xs
                                       then True
 
                                       else annotationCheck id b xs env

                                       
-- get the checkers' returned value, print it, then return the value back to the caller.
-- this way, every check will be recorded, and then printed. 
-- promptLocation ans location = do
--     let out = if ans then "Ok: " ++ toString (location) else "Type Error: " ++ toString(location)
--     print out
--     return ans
    

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
  check (VarDecl s Nothing) env  = typecheck s "" env 
                    

instance CheckC MemberExpr where
  check (MemberCall me e) env = case me of
                                MemberCall me e -> case me of
                                    MemPrimExpr (Ident s) -> typecheck s e env
                                    _ -> True                                   
                                _ -> True
  check (MemPrimExpr p) env  = case p of 
                                PEFuncDecl e -> case e of
                                     FuncDecl (Just a1) a2 v y -> if null a2
                                                                  then True 
                                                                  else statementCheck a2 v y env
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


-- sample data -- [XType {typeName="a", typeFields=["boo", "arrrg"]}, XType {typeName="b", typeFields=["wal", "tal"]}]
check_now :: [SourceElement] -> [XType] -> Bool
check_now [] []    = True
check_now (x:[]) a = check x a
check_now (x:xs) a = (check x a) && (check_now xs a)

runChecker input  = parse_now input (\p -> 
        if (check_now p (runExtractor p))
        then Good "You know how to use your types."
        else Warning "Either you forgot to use the right types or you entered a malformed javascript code.")
