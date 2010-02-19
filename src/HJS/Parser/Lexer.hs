module HJS.Parser.Lexer where

import Data.Char
import Data.Set
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (javaStyle)

import HJS.Parser.Utils



javaScriptStyle=  javaStyle { 
		  P.reservedNames = [ "break","else","new","var","case",
                      "finally","return","void","catch","for","switch","while",
   		      "continue","function","this","with","default","if",
                      "throw","delete","in","try","do","instanceof","typeof",
 	              "abstract","enum","int","short",
 	              "boolean","export","interface","static",
	              "byte","extends","long","super",
	              "char","final","native","synchronized",
	              "class","float","package","throws",
	              "const","goto","private","transient",
	              "debugger","implements","protected","volatile",
	              "double","import","public",
                      "true", "false", "null"
				    ]
               , P.reservedOpNames = [ "==" , "=" , "+=" , "-=" , "++" , 
                      "--" , ";" , ":" , "?" , "," , "." , "|","^",
	              "!" , "*" , "/" , "(" , ")" , "{" , "}" , "[" , "]" , "^" , "&" , "&&" ,
	              "<=" , ">=" , "<" , ">" , "-" , "%" , "*=" , "/=" , "%=" , "+" ,
  	              "===" , "!==" , "!=" , "&&" , "||" , "," , "<<" , ">>" , ">>>" , "~"]
               , P.opLetter = oneOf "!%&()*+,-./:;<=>?[]^{|}~"
               , P.opStart = P.opLetter javaScriptStyle 
         }

binaryOp = [  "==" , "=" , "+=" , "-=" , "(", "[", "{", ",",
                      "--" , 
	               "*" , "/" , "^" , "&" , "&&" , "|",
	              "<=" , ">=" , "<" , ">" , "-", "%" , "*=" , "/=" , "%=" , "+" ,
  	              "===" , "!==" , "!=" , "&&" , "||" , "," , "<<" , ">>" , ">>>" , "~"]

javascript = P.makeTokenParser javaScriptStyle

reserved = P.reserved javascript

sortByLength = sortBy (\x y -> compare (length y) (length x))

{-
oper =
        do{ c <- (P.opStart javaScriptStyle)
          ; cs <- many (P.opLetter javaScriptStyle)
          ; return (c:cs)
          }
        <?> "operator"
-}


oper3 (c:cs) = try (do { s <- string c; return s} )
             <|> oper3 cs

oper3 [] = fail ""

oper = oper3 (sortByLength $  P.reservedOpNames javaScriptStyle)


--oper = oper' []

oper' acc=   do{ c <- (P.opStart javaScriptStyle)
              ; let cs = acc++[c]
              ; if( elem cs (P.reservedOpNames javaScriptStyle)) then return cs else oper' cs
          }
        <?> "operator"


matchTok (c:cs) ((p,TokenROP t):ts) = t == [c] && matchTok cs ts
matchTok [] _ = True
matchTok _ _ = False

merge ((p,TokenROP "+"):(_,TokenROP "+"):cs) = ((p,TokenROP "++"):merge cs)
merge ((p,TokenROP "!"):(_,TokenROP "="):cs) = ((p,TokenROP "!="):merge cs)
merge ((p,TokenROP "="):(_,TokenROP "="):cs) = ((p,TokenROP "=="):merge cs)
merge ((p,TokenROP "+"):(_,TokenROP "="):cs) = ((p,TokenROP "+="):merge cs)
merge ((p,TokenROP "-"):(_,TokenROP "="):cs) = ((p,TokenROP "-="):merge cs)
merge (c:cs) = (c:merge cs)
merge [] = []

data Token
      = 
        TokenWhite  
      | TokenInt Int
      | TokenIdent String
      | TokenTypeIdent String
      | TokenStringLit String
      | TokenNL
      | TokenRegex (String,String)
      | TokenEof
      | TokenRID String
      | TokenROP String
 deriving (Show,Eq)

--
-- Regular Expression Literals
--


regex = do { char '/'; body <- do { c <- firstchar; cs <- many otherchar; return $ concat (c:cs) }; char '/'; flg <- identPart; return $ (body,flg) }

firstchar = do { c <- satisfy (\c -> isPrint c && c /= '*' && c /= '\\' && c /= '/'); return [c]} <|> escapeseq

escapeseq = do { char '\\'; c <- satisfy isPrint; return ['\\',c]}

otherchar = do { c <- satisfy (\c -> isPrint c && c /= '\\' && c /= '/'); return [c]} <|> escapeseq


identPart = many letter

isOP c = do { c' <- char c; return $ TokenROP [c'] }

isRes c = do { reserved c; return $ TokenRID c }

resId = do { x <- many1 identChar; if (elem x $ P.reservedNames javaScriptStyle) then return x else unexpected "resId" }

resOp = do { x <- oper; return x  }

identChar = satisfy (\c -> isAlphaNum c || c == '_')
typeIdentChar = satisfy (\c -> isAlphaNum c || c == '_' || c == '@') --TODO: note that this is probably breaking js in a subtle way

atoken = 
         try (do { x <- many1 digit; return $ TokenInt $ read x })
     <|> try (do { x <- resId; return $ TokenRID x })
     <|> try (do { s <- regex; return $ TokenRegex s })
     <|> try (do { x <- resOp; return $ TokenROP x } )
     <|> try (do { cs <- many1 identChar ; return $ TokenIdent (cs)})
     <|> try (do { cs <- many1 typeIdentChar ; return $ TokenTypeIdent (cs)})


     <|> try (do { char '\n'; return TokenNL }) 
     <|> try (do { char '"'; x<- many stringCharDouble; char '"'; return $ TokenStringLit x })
     <|> try (do { char '\''; x<- many stringCharSingle; char '\''; return $ TokenStringLit x })
     <|> try (do { whiteSpace; return TokenWhite})

stringCharDouble = satisfy (\c -> isPrint c && c /= '"')
stringCharSingle = satisfy (\c -> isPrint c && c /= '\'')


lexer = many (do { p <- getPosition;t <- atoken;  return (p,t) })

runLexer :: String -> [(SourcePos,Token)]
runLexer s = case parse lexer "" s of
                  Right l -> l
                  Left _ -> []

whiteSpace = skipMany1 (satisfy (\c -> isSpace c && c /= '\n'))

runIO :: Show a => Parser a -> String -> IO ()
runIO p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x


lexFile flags name = do
                            input <- readFile name
		            putStrLn $ show $ lexProgram input

lexProgram input = runLexer $ processComments input
