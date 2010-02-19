{--

  Contained herein is the code for handling automatic semi-colon insert.

--}
module HJS.Parser.Prim where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import HJS.Parser.Lexer hiding (whiteSpace)
import Text.ParserCombinators.Parsec.Pos

data JSPState = JSPState {nlFlag::Bool}

newJSPState = JSPState { nlFlag = False }

clearNLFlag = updateState (\x -> x { nlFlag=False })
setNLFlag = updateState (\x -> x { nlFlag=True })
getNLFlag = do s <- getState; return $ nlFlag s                   

type JSParser a = GenParser (SourcePos,Token) JSPState a

--lexeme p = do{ x <- p; whiteSpaceNotNL; return x  }
lexeme p = do{ x <- p; clearNLFlag; whiteSpace; return x  }

mytoken ::  (Token -> Maybe a) -> JSParser a
mytoken test = token showTok posFromTok testTok
                      where
                          showTok (pos,t) = show t
                          posFromTok (pos,t) = pos
                          testTok (pos,t) = test t

anytok = mytoken (\tok -> Just tok )

nlPrior = do { s <- getNLFlag; tok <- mytoken (\tok -> if s then Just tok else Nothing ); putBack tok}

equal :: Token -> JSParser ()
equal test = mytoken (\tok -> if tok == test then Just () else Nothing)

rID :: String -> JSParser ()
rID name = lexeme $ mytoken (\tok -> case tok of 
                                    TokenRID a | a == name  -> Just ()
                                    _ -> Nothing)
regex = lexeme $ mytoken (\tok -> case tok of 
                                    TokenRegex s  -> Just s 
                                    _ -> Nothing)

sLit = lexeme $ mytoken (\tok -> case tok of 
                                    TokenStringLit s  -> Just s 
                                    _ -> Nothing)

iLit = lexeme $  mytoken (\tok -> case tok of 
                                    TokenInt i  -> Just i
                                    _ -> Nothing)

identifier :: JSParser String
identifier = lexeme $ mytoken (\tok -> case tok of 
                              TokenIdent name -> Just name
                              other -> Nothing)
typeIdentifier :: JSParser String
typeIdentifier = lexeme $ mytoken (\tok -> case tok of
                                  TokenTypeIdent name -> Just name
                                  other -> Nothing)

-- Binary operators are allowed to be followed by whitespace with NL.
rOp :: String -> JSParser () 
rOp name = if elem name binaryOp then do { rop; whiteSpace; return () } else rop
           where rop = lexeme $ mytoken (\tok -> case tok of 
                                    TokenROP a | a == name  -> Just ()
                                    _ -> Nothing)

otherOne "[" = Just "]"
otherOne "(" = Just ")"
otherOne "{" = Just "}"
otherOne _ = Nothing

nest :: JSParser String
nest = lexeme $ mytoken (\tok -> case tok of 
                                    TokenROP a  -> (otherOne a)
                                    _ -> Nothing)


whiteSpaceNotNL = try $ many $ do { equal TokenWhite }

whiteSpace = try $ many $ (do { equal TokenWhite } <|> do { (equal TokenNL); setNLFlag})

semi = try (do { rOp ";"; whiteSpace})

semiNL = try (rOp ";") <|> (equal TokenNL) <?> "semi or newline"

braces :: JSParser a -> JSParser a
braces p = do { r <- between (rOp "{") (rOp  "}") p ; whiteSpace; return r }

commaSep p = p `sepBy` (do { rOp ","; whiteSpace })

putBack tok = do 
              st <- getInput; 
              pos <- getPosition; 
              setInput ((pos,tok):st)

-- If semi-colon, newline or next token is }
autoSemi' = do { try semiNL 
                <|> do { rOp "}" ; putBack $ TokenROP "}" }
                <|> eof; whiteSpace}

autoSemi = do { try (rOp ";") 
                <|> nlPrior
                <|> do { rOp "}" ; putBack $ TokenROP "}" }
                <|> eof}

t = do { autoSemi; rOp "}" }

nlBefore t ((_,TokenNL):ts) _ = nlBefore t ts True
nlBefore t ((_,TokenWhite):ts) flag = nlBefore t ts flag
nlBefore t (t':ts) flag = if t' == t then flag else nlBefore t ts False
nlBefore t [] flag = False

t2 = nlBefore ((newPos "" 2 1),TokenROP "+") (runLexer "1\n2+2") False
