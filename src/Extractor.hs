module Extractor where
import HJS.Parser
import HJS.Parser.JavaScript
import Data.List

data XType = XType { typeName :: String
                   , typeFields :: [String] } deriving (Show, Eq)

class ExtractC t where
  extract :: t -> [XType] -> [XType]

instance ExtractC SourceElement where
  extract (Stmt s) c = extract s c
--  extract (FuncDecl fd) = extract fd


instance ExtractC MemberExpr where
--  extract (MemPrimExpr p) = extract p
--  extract (ArrayExpr me e) = []
--  extract (MemberNew me e) = []
  extract (MemberCall me e) c = 
    case me of
      MemberCall me "prototype" -> case me of
          MemPrimExpr (Ident s) -> insert_xtype XType {typeName=s, typeFields=[e]} c
          _ -> []
      _ -> []

insert_xtype ::  XType -> [XType] -> [XType]
insert_xtype xtype current_list = 
  if any (\p -> new_name == (typeName p)) current_list
    then map (\oldtype -> 
      if (typeName oldtype) == new_name 
        then XType {typeName=new_name, typeFields=(typeFields oldtype) ++ (new_fields)}
        else oldtype) current_list
      else current_list ++ [xtype]
    where new_name = (typeName xtype) 
          new_fields = (typeFields xtype)


instance ExtractC Stmt where
  extract (StmtPos p s) c = extract s c

instance ExtractC Expr where
  extract (AssignE p) c = extract p c

instance ExtractC AssignE where
  extract (Assign left AssignNormal right) c = (extract left c) ++ (extract right c)
  extract (Assign left op right) c = (extract left c) ++ (extract right c)
  extract _ _ = []



instance ExtractC LeftExpr where
  extract (CallExpr x) c = extract x c

instance ExtractC CallExpr where
  extract (CallPrim p) c = extract p c
 -- extract (CallMember m args)

instance ExtractC Stmt' where
  extract (ExprStmt p) c = extract p c
--  extract (IfStmt p) = extract p
--  extract (Block xs) = extract xs
--  extract (ItStmt p) = extract p
--  extract (ReturnStmt (Just p)) = extract p
--  extract (ReturnStmt Nothing) = []
--  extract (BreakStmt s) = extract s
--  extract (ContStmt s) = extract s
--  extract (EmptyStmt) = []
--  extract (VarStmt v) = extract v
--  extract (ThrowExpr e) = extract e
--  extract (TryStmt e) = extract e
--  extract (Switch e s) = extract s


runExtractor :: [SourceElement] -> [XType]
runExtractor (x:xs) = (extract x []) ++ (runExtractor xs)
runExtractor _ = []

