module Extractor where
import HJS.Parser
import HJS.Parser.JavaScript

data XType = XType { typeName :: String
                   , typeFields :: [String]
                   } deriving (Show, Eq)

class ExtractC t where
  extract :: t -> [XType]

instance ExtractC SourceElement where
  extract (Stmt s) = extract s
--  extract (FuncDecl fd) = extract fd


instance ExtractC MemberExpr where
--  extract (MemPrimExpr p) = extract p
--  extract (ArrayExpr me e) = []
--  extract (MemberNew me e) = []
  extract (MemberCall me e) = case me of
                                MemberCall me "prototype" -> case me of
                                    MemPrimExpr (Ident s) -> [XType {typeName=s, typeFields=[e]}]
                                    _ -> []
                                _ -> []

instance ExtractC Stmt where
  extract (StmtPos p s) = extract s

instance ExtractC Expr where
  extract (AssignE p) = extract p

instance ExtractC AssignE where
  extract (Assign left AssignNormal right) = (extract left) ++ (extract right)
  extract (Assign left op right) = (extract left) ++ (extract right)
  extract _ = []



instance ExtractC LeftExpr where
  extract (CallExpr c) = extract c

instance ExtractC CallExpr where
  extract (CallPrim p) = extract p
 -- extract (CallMember m args)

instance ExtractC Stmt' where
  extract (ExprStmt p) = extract p
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
runExtractor (x:xs) = (extract x) ++ (runExtractor xs)
runExtractor _ = []

