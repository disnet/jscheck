{--
       JavaScript Parser using Parsec

       Advantages over Happy/Alex are speed (for first parse) and ability to help with
       overcoming language
       grammer kinks easily (such auto semi colon insert); built in facilities for 
       handling expression sub-grammer.

       Disadvantages are that remodelling of grammer is required to remove left recursion etc.


   Limitations: 
     - Nested 'new' expressions are not supported. ie  "new new MyObject" are not support"
 
   To test individual productions use parseEof ie 
 
   parseEof callExpr "bob()[]"

--}

module HJS.Parser.JavaScriptParser(parseProgram,lexProgram,runLexer) where


import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr


import HJS.Parser.Utils
import HJS.Parser.JavaScript
import HJS.Parser.Prim
import HJS.Parser.Lexer (runLexer,lexProgram)


import Data.Char

-------------
-- Grammer 
------------

--
-- Expressions
--

primExpr = 
              do { rID "this" ;return This } 
          <|> do { name <- identifier; return $ Ident name} 
          <|> do { s <- regex; return $ Regex s} 
          <|> do { s <- sLit; return $ Literal (LitString s) }
          <|> do { s <- iLit; return $ Literal (LitInt s) }
          <|> do { s <- rID "true"; return $ Literal (LitBool True) }
          <|> do { s <- rID "false"; return $ Literal (LitBool False) }
          <|> do { s <- rID "null"; return $ Literal (LitNull) }
          <|> do { s <- arrayLit; return $ Array s }
          <|> do { s <- objectLit; return $ Object s}
          <|> do { r <- funcDecl; return $ PEFuncDecl r }
          <|> do { rOp "("; e <- expr; rOp ")"; return $ Brack e }


arrayLit = do { rOp "["; whiteSpace; r <- commaSep assigne; whiteSpace; rOp "]"; return $ ArrSimple r }

objectLit = do { rOp "{"; whiteSpace; r <- commaSep property; whiteSpace; rOp "}"; return $ r }

property =   do { n <- propertyName; 
                      do { rOp ":"; v <- assigne; return $ Left (n,v) } 
                  <|> do { f <- funcDecl2; return $ Right $ GetterPutter n f}
             }


propertyName =  do { r <- identifier; return $ PropNameId r }
         <|>    do { r <- sLit; return $ PropNameStr r }
         <|>    do { r <- iLit; return $ PropNameInt r }


memberExpr' = do { pe <- primExpr; rest $ MemPrimExpr pe} where
             rest x = do {  rOp "."; name <- identifier; rest $ MemberCall x name } 
                      <|> do { rOp "["; e <- expr; rOp "]"; rest $ ArrayExpr x e }
                     <|> return x

memberExpr = try (do { rID "new"; me <- memberExpr'; args <- option [] arguments; return $ MemberNew me args })
          <|> memberExpr' 




newExpr = do { r <- memberExpr; return $ MemberExpr r }
        <|> do { rID "new"; r <- newExpr; return $ NewNewExpr r } 


callExpr = do { x <- memberExpr;
               do {rOp "("; whiteSpace; args <- commaSep assigne; whiteSpace; rOp ")"; rest $ CallMember x args } 
           <|> do { return $ CallPrim x } 
           <|> do { rOp "++"; return $ CallPrim x }
              }
           where
                  rest x = 
                               try (do { rOp "("; args <- commaSep assigne; rOp ")" ; rest $ CallCall x args })
                           <|> try (do { rOp "."; i <- identifier; rest $ CallDot x i })
                           <|> try (do { rOp "["; e <- expr; rOp "]"; rest $ CallSquare x e })
                           <|> return x 

arguments = do { rOp "("; args <- commaSep assigne; rOp ")"; return args }

leftExpr = 
--          try (do { rID "new"; r <- newExpr; return $ NewExpr $ NewNewExpr r}) <|>
            try (do { r <- callExpr; return $ CallExpr r })

-- leftExpr' = do { me <- memberExpr; 

postFix = do { r <- leftExpr; 
                   do { rOp "++"; return $ PostInc r}
               <|> do { rOp "--"; return $ PostDec r }
               <|> do { return $ LeftExpr r}}

simpleaexpr = do { r <- postFix; return $ PostFix r }
          <|> do { rID "delete"; r <- simpleaexpr; return $ Delete r } 
          <|> do { rID "void"; r <- simpleaexpr; return $ Void r } 
          <|> do { rID "typeof"; r <- simpleaexpr; return $ TypeOf r } 
          <|> do { rOp "-"; r <- simpleaexpr; return $ UnaryMinus r } 
          <|> do { rOp "+"; r <- simpleaexpr; return $ UnaryPlus r } 
          <|> do { rOp "++"; r <- simpleaexpr; return $ DoublePlus r } 
          <|> do { rOp "--"; r <- simpleaexpr; return $ DoubleMinus r } 
          <|> do { rOp "!"; r <- simpleaexpr; return $ Not r } 
          <|> do { rOp "~"; r <- simpleaexpr; return $ BitNot r } 
          

simpleaexpr' = do { r <- simpleaexpr; return $ AEUExpr r}

aexpr = buildExpressionParser aritOperators simpleaexpr'

conde = do { r <- aexpr; 
               try (do { whiteSpace; rOp "?"; whiteSpace; a <- assigne; whiteSpace; rOp ":"; whiteSpace; b <- assigne; return $ CondIf r a b })
           <|> do { return $ AExpr r }} 

conde' =  do { r <-conde; return $ CondE r} 

assignOp = do { rOp "*="; return AssignOpMult } <|>
           do { rOp "+="; return AssignOpPlus } <|>
           do { rOp "="; return AssignNormal }

assigne' = do { left <- leftExpr; op <- assignOp; right <- assigne; return $ Assign left op right } 

assigne = choice [
		  try assigne',
                  try conde'
          ] 


expr = do { r <- assigne; return $ AssignE r }

aritOperators =
    [ [ op "*"  AssocLeft, op "/"  AssocLeft,  op "%"  AssocLeft]
    , [ op "+"  AssocLeft, op "-"  AssocLeft ]
    , [ op "<<" AssocLeft, op ">>" AssocLeft,  op ">>>" AssocLeft ]
    , [ op "<" AssocLeft ,op "<=" AssocLeft, op ">" AssocLeft, op ">=" AssocLeft
    ,   op "instanceof" AssocLeft , op "in" AssocLeft ]
    , [ op "==" AssocLeft , op "!=" AssocLeft
    ,   op "===" AssocLeft, op "!==" AssocLeft ]
    , [ op "&" AssocRight ], [ op "^" AssocRight ] ,[ op "|" AssocRight ] 
    , [ op "&&" AssocRight ], [ op "||" AssocRight ] 
  
    ]
    where
      op name assoc   = Infix (do{ rOp name
                                  ; return (\x y -> AOp name x y) 
                                  } 
                               <|> do { rID name; return (\x y -> AOp name x y)}

                         ) assoc


funcDecl = 
    do{ whiteSpace
      ; funcDecl2 }

funcDecl2 = do { 
       typeIdent <- option "" typeIdentifier 
      ; rID "function"
      ;  name <- option "" identifier
      ; rOp "("
      ; args <- commaSep identifier
      ; rOp ")"; whiteSpace
      ; rOp "{" ; whiteSpace
      ; se <- many sourceElement
      ; rOp "}"
      ; return $ FuncDecl (Just name) (Just typeIdent) args se }

exprStmt = do { r <- expr; autoSemi; whiteSpace; return r } 

ret = do { rID "return"; autoSemi; return $ ReturnStmt Nothing}
 
s =       try (do { r <- varStmt; return $ VarStmt r })
      <|> try (do { r <- exprStmt; return $ ExprStmt r })
      <|> try (do { i <- identifier; rOp ":"; whiteSpace; s <- stmt; return $ LabelledStmt i s })
     

stmt = do { pos <- getPosition; s <- stmt'; return $ StmtPos (sourceLine pos, sourceColumn pos) s }

stmt' =    do { r <- block; return $ Block r }
      <|> do { semi ; return EmptyStmt }
      <|> s
--      <|> do { r <- varStmt; return $ VarStmt r }
--      <|> do { r <- exprStmt; return $ ExprStmt r }
--      <|> do { i <- identifier; rOp ":"; whiteSpace; s <- stmt; return $ LabelledStmt i s }
      <|> do { rID "if"; r <- restOfIf; return $ IfStmt r}
      <|> do { rID "for"; r <- restOfFor; return $ ItStmt r}
      <|> do { rID "do"; whiteSpace; r <- restOfDo; return $ ItStmt r }
      <|> do { rID "while"; r <- restOfWhile; return $ ItStmt r }
      <|> do { rID "return";    do { e <- expr; autoSemi; return $ ReturnStmt (Just e)}
                            <|> do { autoSemi; return $ ReturnStmt Nothing} }
      <|> do { rID "break";     do { e <- identifier; return $ BreakStmt (Just e)}
                            <|> do { return $ BreakStmt Nothing}}
      <|> do { rID "continue";    do {e <- identifier; return $ ContStmt (Just e)}
                              <|> do { return $ ContStmt Nothing}}
      <|> do { rID "with"; rOp "("; e <- expr; rOp ")"; s <- stmt ; return $ WithStmt e s }
      
      <|> do { rID "throw"; e <- exprStmt; return $ ThrowExpr e}
      <|> do { rID "try"; r <- restOfTry ; return $ TryStmt r }
      <|> do { rID "switch"; rOp "("; e <- expr; rOp ")"; whiteSpace; rOp "{"; whiteSpace; s <- caseblock ; whiteSpace; rOp "}"; whiteSpace; return $ Switch e s }



block = braces $ do { whiteSpace; b <- many stmt; whiteSpace; return b}

caseblock = many caseclause

caseclause =   do { rID "case"; e <- expr; rOp ":"; whiteSpace; ss <- many stmt; return $ CaseClause e ss}
           <|> do { rID "default"; rOp ":"; whiteSpace; ss <- many stmt; return $ DefaultClause ss }
           <?> "case or default"

restOfDo = do {
                s <- stmt;
                rID "while";
                rOp "(";
                e <- expr;
                rOp ")";
                autoSemi; 
                return $ DoWhile s e}

restOfWhile = do { 
                rOp "(";
                e <- expr;
                rOp ")"; whiteSpace;
                s <- stmt;
                return $ While e s}

restOfTry = do { whiteSpace; b <- block;
                 cl <- many catchh;
                 f <- option [] finally;
                 return $ TryTry b cl f}

catchh = do { rID "catch"; rOp "("; i <- identifier;
             iff <- option Nothing (do { rID "if"; e <- expr;return $ Just e});
             rOp ")"; whiteSpace;
             b <- block;
             return $ CatchCatch i iff b }

finally = do { rID "finally"; whiteSpace; b <- block; return b }

restOfIf = do { rOp "("; e <- expr; rOp ")"; whiteSpace; s <- stmt; 
                         do { rID "else"; whiteSpace; s2 <- stmt; return $ IfElse e s s2 }
                     <|> do { return $ IfOnly e s }}



exprOpt = option Nothing (do { e <- expr; return $ Just e})

restOfFor = do { 
                 rOp "("; 
                
                 try (do { 
                   option () (rID "var");  
                   vars <- commaSep varDecl; 
                   rOp ";"; e1 <- exprOpt;
                   rOp ";"; e2 <- exprOpt;
                   rOp ")"; whiteSpace;
                   s <- stmt; 
                   return $ ForVar vars e1 e2  s } )
                 <|> do {
                   option () (rID "var");  
                   l <- leftExpr;
                   rID "in";
                   e <- expr;
                   rOp ")"; whiteSpace; 
                   s <- stmt;
                   return $ ForIn l e s 
                 }
                 <?> "restOfFor"
             }
            <?> "rest of for"




{-restOfFor' = do { 
                 rOp "("; rOp ")";
                 s <- stmt;
                 return $ ForVar [] Nothing Nothing EmptyStmt } <?> "rest of for"
-}
-- restOfIt = do { 

restOfVarStmt = do { r <- commaSep varDecl; return r }


varStmt = try (do { rID "var"; r <- commaSep varDecl; return r })
        <|> try (do { rID "const"; r <- commaSep varDecl; return r })

varDecl = do { i <- identifier; e <- initialiser; return $ VarDecl i e }

initialiser = do { rOp "="; e <- assigne; return $ Just e }
           <|> do { return Nothing }

sourceElement = do { r <- stmt; whiteSpace; return $ Stmt r }

program = do { whiteSpace; r <- many sourceElement; whiteSpace; eof; return r }



chainl1' p op1 op2 = do{ x <- p; rest [x] }
                    where
                      rest x    = do{ f <- op1
                                    ; y <- p
                                    ; r <- rest (y:x)
                                    ; g <- op2
                                    ; return r
                                    }
                                <|> return x





-----------------------------------------------
-- Utility
-----------------------------------------------


-- lexProgram input = runLexer $ processComments input

{--
runIO :: Show a => Parser a -> String -> IO ()
runIO p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x
--}

parse' p name input = runParser p newJSPState name input

parseProgram input = parse' program "" (runLexer $ processComments input)

-- Parse using 'p' and make sure all input is parsed.
parseEof p i = parse' (do { r <- p; eof ; return r }) "" (runLexer $ processComments i)

