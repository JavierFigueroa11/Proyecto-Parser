module Main where

import System.IO  
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Prim
import qualified Text.ParserCombinators.Parsec.Token as Token
--import qualified Text.Parsec.Token as P

data BExpr = BoolConst Bool
            | Not BExpr
            | BBinary BBinOp BExpr BExpr
            | RBinary RBinOp AExpr AExpr
             deriving (Show)

data BBinOp = And | Or | NOT deriving (Show)

data RBinOp = Greater | Less | GreaterEqual | LessEqual | Equals deriving (Show)

data AExpr = Var String
            | IntConst Integer
            | Neg AExpr
            | Semi String
            | ABinary ABinOp AExpr AExpr
              deriving (Show)
               
data Expr = IntExpr    AExpr 
          | BoolExpr   BExpr
          | StringExpr SExpr
          | VarExpr    String
          deriving (Show)
          
data SExpr = StringConst String
           | SVar String
           | Concat SExpr SExpr
           deriving (Show)
data ABinOp = Add
             | Subtract
             | Multiply
             | Divide
             | Modules
             deriving (Show)

data Stmt = Seq [Stmt]
           | Function Stmt
           | Assign  String AExpr
           | Assigni  String
           | If BExpr Stmt Stmt
           | While BExpr Stmt
           | Skip
           | Simple String
           | DeclarationStmt DataType [String]
           | BlockStmt  [Stmt]
           | Empty
           | WithoutDeclarationStmt String
           |FunctionWithoutParameters Stmt
             deriving (Show)


data DataType = INT String
               | CHAR String
               | VOID String
               | DOUBLE String
                  deriving(Show)



lenguageDef = emptyDef {
            Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter 
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "if"
                                      , "else if"
                                      , "else"
                                      , " "
                                      , "while"
                                      , "do"
                                      , "skip"
                                      , "true"
                                      , "false"
                                      , "not"
                                      , "and"
                                      , "or"
                                      ,"int"
                                      ,"double"
                                      , "char"
                                      ,"void"
                                      ,"float"
                                      , "int main"
                                      , "int main()"
                                      ,"&&"
                                      ,"||"
                                      ,"!"
                                      ,",",
                                      ";"
                                      ]
            , Token.reservedOpNames = ["+", "-", "*", "/", "=","%"
                                      , "<", ">","<=",">=","==","!=","and", "or", "not",";"
                                      ]
}

lexer = Token.makeTokenParser lenguageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
braces     = Token.braces     lexer  
integer    = Token.integer    lexer -- parses an integer

semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
comma      = Token.comma      lexer

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement =   sequenceOfStmt
           <|> statement'

sequenceOfStmt =
   do list <- (sepBy1 statement' semi)
      -- If there's only one statement return it without using Seq.
      return $ if length list == 1 then head list else Seq list 

statement' :: Parser Stmt
statement' =   funcionStmt
         <|> braces funcionStmt
         <|> braces ifStmt
         <|> braces whileStmt
         <|> braces skipStmt
         <|> ifStmt
         <|> whileStmt
         <|> skipStmt
         <|> assignStmt
         <|> parens statement
         <|> declarationStmt
         <|> parens declarationStmt
         <|> parseBlockStmt
         <|> emptyStmt
         <|> simpleAssignStmt
         <|> tryAssignStmt
         <|> decStmt



ifStmt :: Parser Stmt 
ifStmt =
   do reserved "if"
      cond  <- bExpression
      braces statement
      reserved "else if"
      stmt1 <- statement
      braces statement
      reserved "else"
      stmt2 <- statement
      return $ If cond stmt1 stmt2

funcionStmt :: Parser Stmt
funcionStmt   =
               do reserved "int main" 
                  parens decStmt
                  st1 <- parseBlockStmt
                  return $ Function st1
whileStmt :: Parser Stmt
whileStmt =
   do reserved "while"
      cond <- bExpression
      reserved "do"
      stmt <- statement
      return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt =
      do data_type
         var  <- identifier
         optional semi
         reservedOp "="
         expr <- aExpression
         optional semi
         return $ Assign var expr

simpleAssignStmt :: Parser Stmt
simpleAssignStmt = 
      do data_type
         vari  <- identifier
         semi
         return $ Assigni vari

tryAssignStmt :: Parser Stmt
tryAssignStmt = try simpleAssignStmt <|> assignStmt 

declarationStmt :: Parser Stmt
declarationStmt = do
         id <- data_type
         vars <- identifier `sepBy1` comma
         optional semi
         return $ DeclarationStmt id vars

withoutdeclarationStmt :: Parser Stmt
withoutdeclarationStmt = do
         id1 <- identifier
         return $ WithoutDeclarationStmt id1

decStmt :: Parser Stmt
decStmt = try withoutdeclarationStmt <|> declarationStmt

emptyStmt :: Parser Stmt
emptyStmt = do 
            semi
            return $ (Empty)


data_type :: Parser DataType     
data_type =  (reserved "int"  >> return (INT "INT" ))
            <|> (reserved "void" >> return (VOID "VOID")) 
            <|> (reserved "float"  >> return (INT "FLOAT" ))
            <|> (reserved "double" >> return (VOID "DOUBLE")) 


parseBlockStmt :: Parser Stmt
parseBlockStmt = do 
                  s1 <- braces (many statement')
                  return $ (BlockStmt s1)

skipStmt :: Parser Stmt
skipStmt = reserved "" >> return Skip

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm


bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
              , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft,
                 Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
              , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
                 Infix  (reservedOp "%"   >> return (ABinary Modules)) AssocLeft]
               ]

bOperators = [ [Prefix (reservedOp "!" >> return (Not             ))          ]
              , [Infix  (reservedOp "&&" >> return (BBinary And     )) AssocLeft,
                 Infix  (reservedOp "||"  >> return (BBinary Or      )) AssocLeft]
              ]
aTerm =  parens aExpression 
      <|> liftM Var identifier
      <|> liftM IntConst integer

bTerm =  parens bExpression
      <|> (reserved "true"  >> return (BoolConst True ))
      <|> (reserved "false" >> return (BoolConst False))
      <|> rExpression

rExpression =
   do a1 <- aExpression
      op <- relation
      a2 <- aExpression
      return $ RBinary op a1 a2 

relation =   (reservedOp ">" >> return Greater)
        <|> (reservedOp "<" >> return Less)
        <|> (reservedOp ">=" >> return GreaterEqual)
        <|> (reservedOp "<=" >> return LessEqual)
        <|> (reservedOp "==" >> return Equals)

parseString :: String -> Stmt
parseString str =
   case parse whileParser "" str of
     Left e  -> error $ show e
     Right r -> r

parseFile :: String -> IO Stmt
parseFile file =
   do program  <- readFile file
      case parse whileParser "" program of
        Left e  -> print e >> fail "parse error"
        Right r -> return r
        --ast <- parseFile "../PD2019/prueba1.txt" parseString
        

main :: IO ()
main = return ()
