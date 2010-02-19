import Test.Framework(defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Checker
import HJS.Parser.JavaScript
import Test.HUnit

main = defaultMain tests

tests = [
        testGroup "Basic Tests" [
          testCase "simple" test_simple
        ]
    ]

test_simple = assertEqual "simple" computed "5"
  where computed = show (checkProgram "var x;")
--test_simple = assertEqual "simple" (checkProgram "var x;") ([Stmt (StmtPos (1,1) (VarStmt [VarDecl "x" (Just (CondE (AExpr (AEUExpr (PostFix (LeftExpr (CallExpr (CallPrim (MemPrimExpr (Literal (LitInt 5)))))))))))])),Stmt (StmtPos (1,10) EmptyStmt)])
