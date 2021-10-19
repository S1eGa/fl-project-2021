import Control.Monad
import Text.Parsec.Language
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.IO
import System.Environment

type Id = String
type Type = String

data Op =     Or
            | And
            | BoolNeg
            | Eq
            | Neq
            | Leq
            | Le
            | Geq
            | Ge
            | Pow
            | Neg
            | Add
            | Sub
            | Mul
            | Div
            | ExprMerge
            | ApplyFunc
    deriving (Eq, Show)

data Expr =   EmptyExpr
            | Var Id
            | Num Integer
            | StringLiteral String 
            | BoolLiteral Bool
            | BinOp Op Expr Expr
            | UnOp Op Expr
    deriving (Eq, Show)

data Statement =   Empty
                 | Assign Id Expr
                 | Decl Type Id Expr
                 | If Expr Statement Statement
                 | While Expr Statement
                 | Union Statement Statement
                 | ExprSt Expr
                 | Return Expr
    deriving (Eq, Show)

data TypedArgList =   EmptyArg 
                    | Arg Type Id
                    | UnionArgs TypedArgList TypedArgList
    deriving (Eq, Show)

data Func = Func Type Id TypedArgList Statement
    deriving (Eq, Show)

data FuncList =   EmptyFunc
                | UnionFuncs Func FuncList
    deriving (Eq, Show)

languageDefinition =
     emptyDef    { Token.nestedComments  = False
                 , Token.caseSensitive   = True
                 , Token.identStart      = oneOf "qwertyuiopasdfghjklzxcvbnm"
                 , Token.identLetter     = alphaNum
                 , Token.reservedNames   = [ "{"
                                           , "}"
                                           , "("
                                           , ")"
                                           , ";"
                                           , "->"
                                           , "Func"
                                           , "While"
                                           , "If"
                                           , "Else"
                                           , "Var"
                                           , "Int"
                                           , "String"
                                           , "Bool"
                                           , "Return"
                                           ]
                 , Token.reservedOpNames = [ "||"
                                           , "&&"
                                           , "!"
                                           , "=="
                                           , "/="
                                           , "<="
                                           , "<"
                                           , ">="
                                           , ">"
                                           , "^"
                                           , "+"
                                           , "-"
                                           , "*"
                                           , "/"
                                           , ","
                                           , "$"
                                           , ":="
                                           ]
                }

lexer = Token.makeTokenParser languageDefinition

identifier = Token.identifier        lexer
reserved   = Token.reserved          lexer
reservedOp = Token.reservedOp        lexer
parens     = Token.parens            lexer
braces     = Token.braces            lexer
integer    = Token.integer           lexer
whiteSpace = Token.whiteSpace        lexer
stringL    = Token.stringLiteral     lexer

boolLiter :: Parser Expr
boolLiter = (do
    reserved "True"
    return $ BoolLiteral True)
        <|> (do
    reserved "False"
    return $ BoolLiteral False)

fromBinToInteger :: String -> Integer
fromBinToInteger = acc 0 
    where acc num (x:xs) | x == '0' = acc (num * 2)     xs
                         | x == '1' = acc (num * 2 + 1) xs
          acc num [] = num

binNumber :: Parser Expr
binNumber = do 
        prefix <- char 'B'
        str <- many1 ((char '0') <|> (char '1'))
        whiteSpace
        return $ (Num . fromBinToInteger) str

funcCall :: Parser Expr
funcCall = do
    name <- identifier
    expr <- (parens expression)
    return $ BinOp ApplyFunc (Var name) expr

varOrFuncCall :: Parser Expr
varOrFuncCall = do
    name <- identifier
    expr <- ((parens expression) <|> (return EmptyExpr))
    return $ if expr == EmptyExpr then Var name else BinOp ApplyFunc (Var name) expr 

term =      fmap StringLiteral stringL
        <|> fmap Num integer
        <|> varOrFuncCall
        <|> boolLiter
        <|> parens expression
        <|> binNumber
        <|> (return EmptyExpr)

expression :: Parser Expr
expression = buildExpressionParser operators term

operators = [ 
              [Infix (reservedOp "^"  >> return (BinOp Pow)) AssocRight]

            , [Prefix (reservedOp "-" >> return (UnOp Neg))]

            , [Infix (reservedOp "*"  >> return (BinOp Mul)) AssocLeft,
               Infix (reservedOp "/"  >> return (BinOp Div)) AssocLeft]

            , [Infix (reservedOp "+"  >> return (BinOp Add)) AssocLeft,
               Infix (reservedOp "-"  >> return (BinOp Sub)) AssocLeft]

            , [Infix (reservedOp "==" >> return (BinOp Eq))  AssocNone,
               Infix (reservedOp "/=" >> return (BinOp Neq)) AssocNone,
               Infix (reservedOp "<=" >> return (BinOp Leq)) AssocNone,
               Infix (reservedOp "<"  >> return (BinOp Le))  AssocNone,
               Infix (reservedOp ">=" >> return (BinOp Geq)) AssocNone,
               Infix (reservedOp ">"  >> return (BinOp Ge))  AssocNone]

            , [Prefix (reservedOp "!" >> return (UnOp BoolNeg))]

            , [Infix (reservedOp "&&"  >> return (BinOp And)) AssocRight]

            , [Infix (reservedOp "||"  >> return (BinOp Or)) AssocRight]
            
            , [Infix (reservedOp ","  >> return (BinOp ExprMerge)) AssocLeft]
            , [Infix (reservedOp "$"  >> return (BinOp ApplyFunc)) AssocRight]
            ]

assignStatement :: Parser Statement
assignStatement = do
    name <- identifier
    reservedOp ":="
    expr <- expression
    reserved ";"
    return $ Assign name expr

declType :: Parser Type
declType = (do 
    reserved "Int" 
    return "Int") <|>
           (do 
    reserved "Bool" 
    return "Bool") <|>
           (do 
    reserved "String" 
    return "String")


declStatement :: Parser Statement
declStatement = do
    reserved "Var"
    myType <- declType
    name <- identifier
    reservedOp ":="
    expr <- expression
    reserved ";"
    return $ Decl myType name expr

exprStatement :: Parser Statement
exprStatement = do
    expr <- expression
    reserved ";"
    return $ ExprSt expr


statement' :: Parser Statement
statement' =     assignStatement 
             <|> declStatement 
             <|> ifStatement 
             <|> whileStatement 
             <|> exprStatement
             <|> returnStatement

statement :: Parser Statement
statement =  do
    firstStatement <- statement'
    other <- (statement <|> return Empty)
    return $ Union firstStatement other

returnStatement :: Parser Statement
returnStatement = do
    reserved "Return"
    expr <- expression
    reserved ";"
    return $ Return expr

elseStatement :: Parser Statement
elseStatement = (do
    reserved "Else"
    action <- (braces statement)
    return action)
            <|> (return Empty)  

ifStatement :: Parser Statement
ifStatement = do
    reserved "If"
    condition <- (parens expression)
    ifAction  <- (braces statement)
    elseAction <- elseStatement 
    return $ If condition ifAction elseAction

whileStatement :: Parser Statement
whileStatement = do
    reserved "While"
    condition <- (parens expression)
    action <- (braces statement)
    return $ While condition action

arg :: Parser TypedArgList
arg = do
    myType <- declType
    name <- identifier
    return $ Arg myType name

typedArgList :: Parser TypedArgList
typedArgList = (do
    firstArg <- arg
    other <- ((do 
        reserved ","
        typedArgList) <|> return EmptyArg)
    return $ UnionArgs firstArg other) <|> (return EmptyArg)

funcDecl :: Parser Func
funcDecl = do
    reserved "Func"
    name <- identifier
    args <- (parens typedArgList)
    reserved "->"
    myType <- declType
    action <- (braces statement)
    whiteSpace
    return $ Func myType name args action

funcList :: Parser FuncList
funcList = do 
    firstFunc <- funcDecl
    other <- (funcList <|> return EmptyFunc)
    return $ UnionFuncs firstFunc other
  
parseAny parser str =
    case parse (whiteSpace >> parser) "" str of
        Left err -> error $ show err
        Right result -> result

buildAST :: String -> IO()
buildAST file = do
    contents <- readFile file 
    print (parseAny funcList contents)