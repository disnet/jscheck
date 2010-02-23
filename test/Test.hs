import Test.Framework(defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Checker
import HJS.Parser.JavaScript
import Test.HUnit

-- bad example for unit test but it illustrates a way to walk the AST
class CheckC t where
  check :: t -> Assertion

instance CheckC SourceElement where
  check (Stmt s) = check s
  check fd = check fd

instance CheckC Stmt where
  check (StmtPos p s) = check s

instance CheckC Stmt' where
  check (VarStmt (v:vs)) = check v
  check (EmptyStmt) = assertBool "do nothing" True

instance CheckC VarDecl where
  check (VarDecl s (Just e)) = assertBool "do nothing" True
  check (VarDecl s Nothing)  = assertEqual "var should be 'x'" s "x"

main = defaultMain tests

tests = [
        testGroup "Basic Tests" [
          testCase "simple" test_simple
        ]
    ]

-- Right [Stmt (StmtPos (1,1) (VarStmt [VarDecl "x" Nothing])),Stmt (StmtPos (1,6) EmptyStmt)]
test_simple = do case checkProgram "var x;" of
                  Right (r:rs) -> check r
                  Left l -> assertFailure ("parse error: " ++ (show l))

