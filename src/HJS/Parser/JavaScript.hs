module HJS.Parser.JavaScript where


class EmitHaskell a where
   eHs :: a -> String

data Literal = LitInt Int 
	     | LitString String
             | LitNull
             | LitBool Bool
	       deriving Show

data PrimExpr 
      = Literal Literal
      | Ident String 
      | Brack Expr
      | This
      | Regex (String,String)
      | Array ArrayLit 
      | Object [Either (PropName, AssignE) GetterPutter ]
      | PEFuncDecl FuncDecl
      deriving Show

data GetterPutter = 
		  GetterPutter PropName FuncDecl | Putter FuncDecl deriving Show

data PropName = PropNameId String | PropNameStr String | PropNameInt Int 
	      deriving Show

data ArrayLit = ArrSimple [AssignE]
	      deriving Show

data MemberExpr = MemPrimExpr PrimExpr 
		| ArrayExpr MemberExpr Expr
		| MemberNew MemberExpr [AssignE]		  
		| MemberCall MemberExpr String
		| MemberCall2 MemberExpr MemberExpr
		deriving Show

data CallExpr = CallPrim MemberExpr
              | CallMember MemberExpr [AssignE]
	      | CallCall CallExpr [AssignE]
	      | CallSquare CallExpr  Expr
	      | CallDot CallExpr String
		deriving Show

data NewExpr = MemberExpr MemberExpr 
	     | NewNewExpr NewExpr
	     deriving Show

data LeftExpr = NewExpr NewExpr 
	      | CallExpr CallExpr
	      deriving Show

data PostFix = LeftExpr LeftExpr
	     | PostInc LeftExpr
	     | PostDec LeftExpr
	       deriving Show

data UExpr = PostFix PostFix
                     | Delete UExpr
                     | Void UExpr
                     | TypeOf UExpr
		     | DoublePlus UExpr 
		     | DoubleMinus UExpr 
                     | UnaryPlus UExpr
                     | UnaryMinus UExpr
                     | Not UExpr
                     | BitNot UExpr
		     deriving Show

{-- 
data MultExpr = UExpr UExpr
            |   Times MultExpr UExpr
            |   Div MultExpr UExpr 
            |   Mod MultExpr UExpr deriving Show

data AddExpr  = MultExpr MultExpr 
	      | Plus AddExpr MultExpr
	      | Minus AddExpr MultExpr
		deriving Show

data ShiftE = AddExpr AddExpr 
	    | ShiftLeft ShiftE AddExpr
	    | ShiftRight ShiftE AddExpr
	    | ShiftRight2 ShiftE AddExpr
	      deriving Show

data RelE = ShiftE ShiftE 
	  | LessThan RelE ShiftE
	  | GreaterThan RelE ShiftE
	  | LessEqual RelE ShiftE
	  | GreaterEqual RelE ShiftE
	  | InstanceOf RelE ShiftE
	  | InObject RelE ShiftE 
	    deriving Show

data EqualE = RelE RelE
	    | Equal EqualE RelE
	    | NotEqual EqualE RelE
	    | Equal2 EqualE RelE
	    | NotEqual2 EqualE RelE

 deriving Show

data BitAnd = EqualE EqualE 
	    | BABitAnd BitAnd EqualE
	      deriving Show

data BitXOR = BitAnd BitAnd 
	    | BXBitXOR BitXOR BitAnd
	      deriving Show

data BitOR = BitXOR BitXOR
	   | BOBitOR BitOR BitXOR
	     deriving Show

data LogAnd = BitOR BitOR 
	    | LALogAnd LogAnd BitOR
	      deriving Show
data LogOr = LogAnd LogAnd 
	   | LOLogOr LogOr LogAnd
	     deriving Show
--}

data AExpr = AEUExpr UExpr | AOp String AExpr AExpr deriving Show

data CondE = AExpr AExpr
	   | CondIf AExpr AssignE AssignE 
	     deriving Show

data AssignOp = AssignNormal
	      | AssignOpMult 
	      | AssignOpDiv
	      | AssignOpMod
	      | AssignOpPlus 
	      | AssignOpMinus deriving Show

data AssignE = CondE CondE
             | Assign LeftExpr AssignOp AssignE 
             | AEFuncDecl FuncDecl
	       deriving Show



data Expr  = AssignE AssignE 
      deriving Show

data VarDecl = VarDecl String (Maybe AssignE) deriving Show

data IfStmt = IfElse Expr Stmt Stmt
	    | IfOnly Expr Stmt
            | If2 Expr
            | If3
	      deriving Show

data ItStmt = DoWhile Stmt Expr 
         |    While Expr Stmt
	 |    For (Maybe Expr) (Maybe Expr) (Maybe Expr) Stmt 
	 |    ForVar [VarDecl] (Maybe Expr) (Maybe Expr) Stmt 
	 |    ForIn LeftExpr  Expr Stmt 
         | It2 Expr 
	   deriving Show

data TryStmt = TryBC [Stmt] [Catch]
	     | TryBF [Stmt] [Stmt]
	     | TryBCF [Stmt] [Catch] [Stmt]
             | TryTry [Stmt] [Catch] [Stmt]
	       deriving Show

data Catch = Catch String [Stmt] | CatchIf String [Stmt] Expr | CatchCatch String  (Maybe Expr) [Stmt]
	   deriving Show

data Stmt = StmtPos (Int,Int) Stmt' deriving Show

data Stmt' = IfStmt IfStmt | EmptyStmt | ExprStmt Expr | ItStmt ItStmt | Block [Stmt]
	  | VarStmt [VarDecl ]
	  | TryStmt TryStmt
	  | ContStmt (Maybe String)
	  | BreakStmt (Maybe String)
	  | ReturnStmt (Maybe Expr)
	  | WithStmt Expr Stmt
	  | LabelledStmt String Stmt
	  | Switch Expr [CaseClause]
	  | ThrowExpr Expr
	  deriving Show

data Switch = SSwitch Expr [CaseClause]
	      deriving Show

data CaseBlock = CaseBlock [CaseClause]
		 deriving Show

data CaseClause = CaseClause Expr [Stmt] | DefaultClause [Stmt]
		  deriving Show

-- data DefaultClause = DefaultClause [Stmt] 
--		     deriving Show

data FuncDecl = FuncDecl (Maybe String) [TypeAnnotation] [String] [SourceElement]
	      deriving Show

data TypeAnnotation = TypeAnnotation String String
        deriving Show

data SourceElement = Stmt Stmt | SEFuncDecl FuncDecl 
		   deriving Show

data JSProgram = JSProgram [SourceElement]
	       deriving Show
