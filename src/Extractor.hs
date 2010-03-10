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
  extract (SEFuncDecl fd) c = c


instance ExtractC MemberExpr where
  extract (MemPrimExpr p) c = c
  extract (ArrayExpr me e) c = c
  extract (MemberNew me e) c = c
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
  -- TODO: these will need to change
  extract (CondE p) c = c
  extract (AEFuncDecl fd) c = c


instance ExtractC LeftExpr where
  extract (CallExpr x) c = extract x c

instance ExtractC CallExpr where
  extract (CallPrim p) c = extract p c
  extract (CallMember m args) c = c

instance ExtractC Stmt' where
  extract (ExprStmt p) c = extract p c
  extract (IfStmt p) c = c
  extract (Block xs) c = c
  extract (ItStmt p) c = c
  extract (ReturnStmt (Just p)) c = c
  extract (ReturnStmt Nothing) c = c
  extract (BreakStmt s) c = c
  extract (ContStmt s) c = c
  extract (EmptyStmt) c = c
  extract (VarStmt v) c = c
  extract (ThrowExpr e) c = c
  extract (TryStmt e) c = c
  extract (Switch e s) c = c


runExtractor :: [SourceElement] -> [XType]
runExtractor (x:[]) = extract x []
runExtractor (x:xs) = (extract x (runExtractor xs))

