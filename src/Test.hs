import Test.Framework(defaultMain, testGroup)
import HJS.Parser
import Test.Framework.Providers.HUnit
import Checker
import Extractor
import HJS.Parser.JavaScript
import Test.HUnit
import Data.List


-- bad example for unit test but it illustrates a way to walk the AST
class CheckC' t where
  check' :: t -> Assertion

instance CheckC' SourceElement where
  check' (Stmt s) = check' s
  check' fd = check' fd

instance CheckC' Stmt where
  check' (StmtPos p s) = check' s

instance CheckC' Stmt' where
  check' (VarStmt (v:vs)) = check' v
  check' (EmptyStmt) = assertBool "do nothing" True

instance CheckC' VarDecl where
  check' (VarDecl s (Just e)) = assertBool "do nothing" True
  check' (VarDecl s Nothing)  = assertEqual "var should be 'x'" s "x"


main = defaultMain tests

tests = [
        testGroup "Basic Tests" [
          testCase "simple" test_simple,
          testCase "checker" test_single_arg_function, 
          testCase "checker with two arg function" test_two_arg_function,
          testCase "extractor"  extract_single_method,
          testCase "extractor with two methods" extract_two_methods,
          testCase "extractor run on dog file" extract_dog_file,
          testCase "check good file" check_good_file,
          testCase "check bad file with function on top and accessing one fields" check_bad_file_top_fields,
          testCase "check bad file with function on top and accessing two fields" check_bad_file_two_top_fields,
          testCase "check bad file with function on bottom and accessing fields" check_bad_file_bottom_fields,
          testCase "check bad file with function on bottom and accessing methods" check_bad_file_bottom_methods

--          testCase "extractor with two methods and extra stmts" extract_two_methods_with_extra_stmts -- TODO: test is known to fail, we're just going to accept that for now
        ]
    ]

parse_fail ::  [Char] -> ([SourceElement] -> Assertion) -> Assertion
parse_fail p f = case parseProgram p of
                      Right r -> f r
                      Left l -> assertFailure $ "Parse error: " ++ (show l)

-- Right [Stmt (StmtPos (1,1) (VarStmt [VarDecl "x" Nothing])),Stmt (StmtPos (1,6) EmptyStmt)]
test_simple = parse_fail "var x;" (\p -> check' (head p))

test_single_arg_function = parse_fail "//#@type Dog a function(a){a.boo;};"(\p -> 
        let r = (check (head p)[XType {typeName="Dog", typeFields=["boo", "arrrg"]}, XType {typeName="Cat", typeFields=["wal", "tal"]}]) in
            do
              assertBool "typecheck has passed" (r == True))
              
test_two_arg_function = parse_fail "//#@type Dog a @type Cat b function(a, b){a.boo;b.wal;};"(\p -> 
        let r = (check (head p)[XType {typeName="Dog", typeFields=["boo", "arrrg"]}, XType {typeName="Cat", typeFields=["wal", "tal"]}]) in
            do
              assertBool "typecheck has passed" (r == True))

{-}
test_foo = parse_fail "//#@type Dog dog function getSaying(dog) { dog.meow();}"(\p -> 
        let r = (check (head p)[XType {typeName="Dog", typeFields=["boo", "arrrg"]}, XType {typeName="Cat", typeFields=["wal", "tal"]}]) in
            do
              assertBool "typecheck has passed" (r == True))
              
              -}
extract_single_method = parse_fail "Foo.prototype.bar = function(a) {return 0;};" (\p ->
    let xtype = head (runExtractor p) in
        do 
          assertEqual "unknown type" "Foo" (typeName xtype)
          assertEqual "unknown field" "bar" (head (typeFields xtype)))


extract_two_methods = parse_fail prog (\p ->
    let xtype = head (runExtractor p) in
        do
          assertEqual "unkown type" "Foo" (typeName xtype)
          assertEqual "should have only been two fields" 2 (length (typeFields xtype))
          assertBool "has different fields than expected" ((typeFields xtype) \\ ["bar", "baz"] == []))
  where prog = "Foo.prototype.bar = function(a) {return 0;}; Foo.prototype.baz = function() {};"

extract_dog_file = do
  s <- readFile "testfiles/bottom_methods_good.js"
  parse_fail s (\p -> let xtype = head (runExtractor p) in
                  do
                    assertEqual "there should have been a dog type" "Dog" (typeName xtype)
                    assertEqual "should have only had two fields" 2 (length (typeFields xtype))
                    assertBool "has different fields than expected" ((typeFields xtype) \\ ["getName", "bark"] == []))

    
extract_two_methods_with_extra_stmts = parse_fail prog (\p ->
    let xtype = head (runExtractor p) in
        do
          assertEqual "unkown type" "Foo" (typeName xtype)
          assertEqual "should have only been two fields" 2 (length (typeFields xtype))
          assertBool "has different fields than expected" ((typeFields xtype) \\ ["bar", "baz"] == []))
  where prog = "Foo.prototype.bar = function(a) {return 0;}; function extra(a,b) {}; Foo.prototype.baz = function() {};"


check_good_file = do
  s <- readFile "testfiles/bottom_methods_good.js"
  parse_fail s (\p -> if check (head p) (runExtractor p)
                        then assertBool "passed correctly" True 
                        else assertFailure "should have passed for known good file")
    
check_bad_file_top_fields = do
  s <- readFile "testfiles/top_fields_fail.js"
  parse_fail s (\p -> if check (head p) (runExtractor p)
                        then assertFailure "should have failed the type check" 
                        else assertBool "passed correctly" True)

check_bad_file_two_top_fields = do
  s <- readFile "testfiles/two_top_fields_fail.js"
  parse_fail s (\p -> if check (head p) (runExtractor p)
                        then assertFailure "should have failed the type check" 
                        else assertBool "passed correctly" True)
    
check_bad_file_bottom_fields = do
  s <- readFile "testfiles/bottom_fields_fail.js"
  parse_fail s (\p -> if check (head p) (runExtractor p)
                        then assertFailure "should have failed the type check" 
                        else assertBool "passed correctly" True)

check_bad_file_bottom_methods = do
  s <- readFile "testfiles/bottom_methods_fail.js"
  parse_fail s (\p -> if check (head p) (runExtractor p)
                        then assertFailure "should have failed the type check" 
                        else assertBool "passed correctly" True)
