module TemplateToolkitAST where

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Expr
import Control.Applicative ((<*))
import Control.Monad
import Numeric
import Data.Char (toUpper,toLower,isDigit)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

--
----- types
--

type Hash = Map.Map String Val
type Array = Seq.Seq Val
type Env = (Hash,Hash)
type StmtId = (Int,Int,Int) -- (ParentID,SelfID,LastID)
type LineN = Int
type TName = String
type Parser' = GenParser (TName,StmtId)

data IString = IString T.Text | IVar Var deriving (Show)

data Val = VString T.Text
         | VIString [IString]
         | VInt Integer
         | VFloat Double
         | VArray [Expr] -- array of Expressions (must be eval'ed)
         | VArrayRange Expr Expr -- range from Expr1 to Expr2 (must be eval'ed)
         | VArrayV Array -- array of Values (eval'ed and can be stored)
         | VHash [(String,Expr)] -- hash of Expressions
         | VHashV Hash  -- hash of Values
         | VRef String -- reference to VArrayV or VHashV
         | VCode Stmt
         | Undef
         deriving (Show)
        
data VarNode = VarKey String
             | VarIndex Int
             | VarMethod String [Expr]
             | VarRef Var
             deriving (Show)
             
data Var = Var [VarNode] deriving (Show)

data Expr = EVal Val
          | EVar Var
          | EAssign Var Expr
          | EUnOp UnOp Expr
          | EBinOp BinOp Expr Expr
          | ETerOp Expr Expr Expr
          deriving (Show)
        
data UnOp = Pos | Neg | Not
            deriving (Show)

data BinOp = Add | Sub | Mul | Div | Mod | Con | Gt | Ge | Lt | Le | Eq | Ne | And | Or
             deriving (Show)
          
data Else = Else Stmt | Elsif Expr Stmt (Maybe Else)
    deriving (Show)

data Stmt = Seq {sSeq :: [Stmt], sId :: StmtId, lineN :: LineN}
          | SComment {sId :: StmtId, lineN :: LineN}
          | SText {sText :: T.Text, sId :: StmtId, lineN :: LineN}
          | SIf {sCond :: Expr, sBody :: Stmt, sElse :: (Maybe Else), sId :: StmtId, lineN :: LineN}
          | SWhile {sCond :: Expr, sBody :: Stmt, sId :: StmtId, lineN :: LineN}
          | SForeach {sForeachVar :: Var, sForeachArr :: Expr, sBody :: Stmt, sId :: StmtId, lineN :: LineN}
          | SNext {sId :: StmtId, lineN :: LineN}
          | SLast {sId :: StmtId, lineN :: LineN}
          | SBlock {sBlockName :: String, sBody :: Stmt, sId :: StmtId, lineN :: LineN}
          | SProcess {sProcessName :: IString, sProcessAssigns :: [Stmt], sId :: StmtId, lineN :: LineN}
          | SWrapper {sWrapperName :: IString, sWrapperAssigns :: [Stmt], sBody :: Stmt, sId :: StmtId, lineN :: LineN}
          | SAssign {sAssignVar :: Var, sAssignExpr :: Expr, sId :: StmtId, lineN :: LineN}
          | SExpr {sExpr :: Expr, sId :: StmtId, lineN :: LineN}
          | SFilter {sFilterName :: String, sFilterParams :: [Expr], sBody :: Stmt, sId :: StmtId, lineN :: LineN}
          deriving (Show)
--
----- /types
--

--
----- parsers
--
spaces1 :: Parser' ()
spaces1 = skipMany1 space

spacesAround :: Parser' a -> Parser' a
spacesAround = between spaces spaces

spaces1Around :: Parser' a -> Parser' a
spaces1Around = between spaces1 spaces1

parens :: Parser' a -> Parser' a
parens = between (char '(' >> spaces) (spaces >> char ')')

charCi :: Char -> Parser' Char
charCi c = (char $ toUpper c) <|> (char $ toLower c)

stringCi :: String -> Parser' String
stringCi s = mapM charCi s

escapedChar :: Parser' Char
escapedChar = do
    char '\\'
    x <- oneOf "\\$\"'nt"
    return $ case x of
        '\\' -> x
        '$' -> x
        '"' -> x
        '\'' -> x
        'n' -> '\n'
        't' -> '\t'

parseString :: Parser' Val
parseString = do
    char '\''
    x <- many $ noneOf "'" <|> escapedChar
    char '\''
    return $ VString (T.pack x)
    
parseInterpolatedVar :: Parser' Var
parseInterpolatedVar = try $ (try $ between (string "${") (char '}') parseVar) <|> (char '$' >> parseVar)

parseInterpolatedString :: Parser' Val
parseInterpolatedString = do
    char '"'
    iStrings <- many $ ((many1 $ noneOf "\"$\\" <|> escapedChar) >>= return . IString . T.pack) <|> (parseInterpolatedVar >>= return . IVar)
    char '"'
    return $ VIString iStrings

parseInt :: Parser' Val
parseInt = do
    num <- many1 digit
    return $ VInt (read num)

parseFloat :: Parser' Val
parseFloat = try $ do
    d1 <- many1 digit
    char '.'
    d2 <- many1 digit
    return $ VFloat (fst $ (readFloat $ d1++"."++d2) !! 0)

parseArray :: Parser' Val
parseArray = do
    char '[' >> spaces >> many (char ',' >> spaces)
    list <- sepEndBy parseExpr (many $ spacesAround (char ','))
    spaces >> char ']'
    return $ VArray list
    
parseArrayRange :: Parser' Val
parseArrayRange = try $ do
    char '[' >> spaces
    e1 <- parseExpr
    spacesAround $ string ".."
    e2 <- parseExpr
    spaces >> char ']'
    return $ VArrayRange e1 e2

parseHashKeyVal :: Parser' (String,Expr)
parseHashKeyVal = do
    key <- (parseString >>= \(VString str) -> return $ T.unpack str) <|> many1 alphaNum
    spaces >> string "=>" >> spaces
    val <- parseExpr
    return (key,val)
    
parseHash :: Parser' Val
parseHash = do
    char '{' >> spaces >> many (char ',' >> spaces)
    list <- sepEndBy parseHashKeyVal (many $ spacesAround (char ','))
    spaces >> char '}'
    return $ VHash list

parseVarKey :: Parser' VarNode
parseVarKey = try $ do
    let varletter' = ['A'..'Z']++['a'..'z']++['_']
    c <- oneOf varletter'
    cs <- many $ oneOf (varletter' ++ ['0'..'9'])
    if reserved' (c:cs) then unexpected "reserved"
    else return $ VarKey (c:cs)
        where reserved' n = elem n ["BLOCK", "ELSE", "ELSIF", "END", "EXIT", "FILTER", "FOREACH", "IF", "LAST", "NEXT", "PROCESS", "WHILE", "WRAPPER"]
    
parseVarIndex :: Parser' VarNode
parseVarIndex = liftM (VarIndex . read) $ many1 digit
    
parseVarMethod :: Parser' VarNode
parseVarMethod = try $ do
    VarKey methodName <- parseVarKey
    exprs <- parens $ sepBy parseExpr (spacesAround (char ','))
    return $ VarMethod methodName exprs
    
parseVarRef :: Parser' VarNode
parseVarRef = do
    char '$'
    var <- parseVarKey
    return $ VarRef (Var [var])

parseVarRefDeep :: Parser' VarNode
parseVarRefDeep = do
    try $ string "${"
    root <- parseVarKey
    deeper <- many $ char '.' >> (parseVarKey <|> parseVarIndex)
    char '}'
    return $ VarRef (Var (root:deeper))
    
parseVar :: Parser' Var
parseVar = do
    root <- (parseVarMethod <|> parseVarKey)
    deeper <- many $ char '.' >> (parseVarMethod <|> parseVarRefDeep <|> parseVarRef <|> parseVarKey <|> parseVarIndex)
    return $ Var (root:deeper)

parseMethodItem' :: Parser' VarNode
parseMethodItem' = try $ do
    string "item"
    exprs <- parens $ sepBy parseExpr (spacesAround (char ','))
    return $ VarMethod "item" exprs
    
parseStrictVar :: Parser' Var
parseStrictVar = do
    root <- parseVarKey
    deeper <- many $ char '.' >> (parseMethodItem' <|> parseVarRefDeep <|> parseVarRef <|> parseVarKey <|> parseVarIndex)
    return $ Var (root:deeper)
    
parseVal :: Parser' Expr
parseVal = (liftM EVal $ parseHash <|> parseArrayRange <|> parseArray <|> parseString <|> parseInterpolatedString)
            <|> (liftM EVar parseVar)
            <|> (liftM EVal $ parseFloat <|> parseInt)

parseTernary :: Parser' Expr
parseTernary = try $ do
    cond <- parseTerm
    spacesAround $ char '?'
    yes <- parseTerm
    spacesAround $ char ':'
    no <- parseTerm
    return $ ETerOp cond yes no

parseTerm :: Parser' Expr
parseTerm = ((parens parseExpr) <|> parseVal) <* spaces
    
parseExpr = buildExpressionParser table (parseTernary <|> parseTerm)
    where table =   [
                    [Prefix $ try (char '-' >> notFollowedBy (char '%')) >> return (EUnOp Neg),
                    Prefix $ char '+' >> return (EUnOp Pos),
                    Prefix $ ((string "!" >> return ()) <|> try (stringCi "not" >> spaces1)) >> return (EUnOp Not)]
                    ,[Infix (char '*' >> spaces >> return (EBinOp Mul)) AssocLeft,
                    Infix (char '/' >> spaces >> return (EBinOp Div)) AssocLeft,
                    Infix (try (char '%' >> notFollowedBy (char ']') >> spaces) >> return (EBinOp Mod)) AssocLeft,
                    Infix (try (stringCi "mod" >> spaces1) >> return (EBinOp Mod)) AssocLeft]
                    ,[Infix (char '+' >> spaces >> return (EBinOp Add)) AssocLeft,
                    Infix (try (char '-' >> notFollowedBy (char '%') >> spaces) >> return (EBinOp Sub)) AssocLeft]
                    ,[Infix (try (char '_' >> spaces1) >> return (EBinOp Con)) AssocLeft]
                    ,[Infix (try (string ">=" >> spaces) >> return (EBinOp Ge)) AssocLeft,
                    Infix (char '>' >> spaces >> return (EBinOp Gt)) AssocLeft,
                    Infix (try (string "<=" >> spaces) >> return (EBinOp Le)) AssocLeft,
                    Infix (char '<' >> spaces >> return (EBinOp Lt)) AssocLeft,
                    Infix (try (string "==" >> spaces) >> return (EBinOp Eq)) AssocLeft,
                    Infix (string "!=" >> spaces >> return (EBinOp Ne)) AssocLeft]
                    ,[Infix (try (stringCi "gt" >> spaces1) >> return (EBinOp Gt)) AssocLeft,
                    Infix (try (stringCi "ge" >> spaces1) >> return (EBinOp Ge)) AssocLeft,
                    Infix (try (stringCi "lt" >> spaces1) >> return (EBinOp Lt)) AssocLeft,
                    Infix (try (stringCi "le" >> spaces1) >> return (EBinOp Le)) AssocLeft,
                    Infix (try (stringCi "eq" >> spaces1) >> return (EBinOp Eq)) AssocLeft,
                    Infix (try (stringCi "ne" >> spaces1) >> return (EBinOp Ne)) AssocLeft]
                    ,[Infix ((try (stringCi "and" >> spaces1) <|> (spacesAround (string "&&") >> return ())) >> return (EBinOp And)) AssocLeft,
                    Infix ((try (stringCi "or" >> spaces1) <|> (try (spacesAround (string "||")) >> return ())) >> return (EBinOp Or)) AssocLeft]
                    ]

incrementSid :: Parser' StmtId
incrementSid = do
    (tName,(parent,prev,_)) <- getState
    let newId = (parent,prev+1,prev+1)
    putState (tName,newId)
    return newId
    
getLineN :: Parser' LineN
getLineN = liftM sourceLine getPosition

parseComment :: Parser' Stmt
parseComment = do
    char '#'
    lineN <- getLineN
    manyTill anyChar (string "\n" <|> try (string "-%]" <|> string "%]"))
    newId <- incrementSid
    return $ SComment newId lineN
    
parseText :: Parser' Stmt
parseText = do
    spaces
    try $ (string "-%]" <* spaces) <|> string "%]"
    lineN <- getLineN
    s <- manyTill anyChar (try ((try $ spaces >> string "[%-") <|> string "[%"))
    spaces
    newId <- incrementSid
    return $ SText (T.pack s) newId lineN

stmtEnd :: Parser' ()
stmtEnd = do
    spaces
    eof <|> (char ';' >> spaces) <|> (lookAhead (string "-%]" <|> string "%]") >> return ())
    
parsePipeFilter :: Parser' (Maybe (String,[Expr]))
parsePipeFilter =
    do
        try $ char '|' >> spaces1
        (VarKey name) <- parseVarKey
        exprs <- try (parens $ sepBy parseExpr (spacesAround (char ','))) <|> return []
        return (Just (name,exprs))
    <|>
    return Nothing
    
stmtEndFilter :: Parser' (Maybe (String,[Expr]))
stmtEndFilter = do
    spaces
    fltr <- parsePipeFilter
    eof <|> (char ';' >> spaces) <|> (lookAhead (string "-%]" <|> string "%]") >> return ())
    return fltr

parseEnd :: Parser' ()
parseEnd = (try $ string "END") >> stmtEnd

parseMaybeElse :: Parser' (Maybe Else)
parseMaybeElse = ((parseElsif <|> parseElse) >>= \els -> return $ Just els) <|> (parseEnd >> return Nothing)

parseIf :: Parser' Stmt
parseIf = do
    try $ string "IF" >> spaces1
    lineN <- getLineN
    newId <- incrementSid
    cond <- parseExpr
    stmtEnd
    body <- parseStmtSeq
    maybeElse <- parseMaybeElse
    return $ SIf cond body maybeElse newId lineN
    
parseElsif :: Parser' Else
parseElsif = do
    try $ string "ELSIF" >> spaces1
    cond <- parseExpr
    stmtEnd
    body <- parseStmtSeq
    maybeElse <- parseMaybeElse
    return $ Elsif cond body maybeElse

parseElse :: Parser' Else
parseElse = do
    try $ string "ELSE" >> stmtEnd
    body <- parseStmtSeq
    parseEnd
    return $ Else body

parseWhile :: Parser' Stmt
parseWhile = do
    try $ string "WHILE" >> spaces1
    lineN <- getLineN
    (tName,(parent,prev,_)) <- getState
    putState (tName,(prev+1,prev+1,prev+1))
    cond <- parseExpr
    stmtEnd
    body <- parseStmtSeq
    -- body <- liftM SExpr $ parseExpr <* stmtEnd
    parseEnd
    return $ SWhile cond body (parent,prev+1,prev+1) lineN

parseForeach :: Parser' Stmt
parseForeach = do
    try $ string "FOREACH" >> spaces1
    lineN <- getLineN
    (tName,(parent,prev,_)) <- getState
    putState (tName,(prev+1,prev+1,prev+1))
    (SAssign var expr _ _) <- parseAssign
    stmtEnd
    body <- parseStmtSeq
    parseEnd
    return $ SForeach var expr body (parent,prev+1,prev+1) lineN

parseLast :: Parser' Stmt
parseLast = do
    try $ string "LAST" >> stmtEnd
    lineN <- getLineN
    newId <- incrementSid
    return $ SLast newId lineN
    
parseNext :: Parser' Stmt
parseNext = do
    try $ string "NEXT" >> stmtEnd
    lineN <- getLineN
    newId <- incrementSid
    return $ SNext newId lineN
    
parseBlock :: Parser' Stmt
parseBlock = do
    try $ string "BLOCK" >> spaces1
    lineN <- getLineN
    newId <- incrementSid
    (VarKey name) <- parseVarKey
    stmtEnd
    body <- parseStmtSeq
    parseEnd
    return $ SBlock name body newId lineN

parseTmplName :: Parser' IString
parseTmplName = do
    (liftM IVar parseInterpolatedVar) <|> do
        (VarKey s) <- parseVarKey
        ext <- option "" (many $ oneOf (['A'..'Z']++['a'..'z']++['_','.','-']))
        return $ IString (T.pack (s ++ ext))

parseProcess :: Parser' Stmt
parseProcess = do
    try $ string "PROCESS" >> spaces1
    lineN <- getLineN
    newId <- incrementSid
    name <- parseTmplName
    spaces
    assigns <- many parseSimpleAssign
    fltr <- stmtEndFilter
    return $ case fltr of
        Nothing -> SProcess name assigns newId lineN
        Just (fltrName,exprs) -> SFilter fltrName exprs (SProcess name assigns newId lineN) newId lineN

parseWrapper :: Parser' Stmt
parseWrapper = do
    try $ string "WRAPPER" >> spaces1
    lineN <- getLineN
    newId <- incrementSid
    name <- parseTmplName
    spaces
    assigns <- many parseSimpleAssign
    stmtEnd
    body <- parseStmtSeq
    parseEnd
    return $ SWrapper name assigns body newId lineN
        
parseAssign :: Parser' Stmt
parseAssign = do
    lineN <- getLineN
    var <- parseStrictVar
    spacesAround $ char '='
    expr <- parseExpr
    newId <- incrementSid
    return $ SAssign var expr newId lineN
    
parseSimpleAssign :: Parser' Stmt
parseSimpleAssign = do
    lineN <- getLineN
    var <- parseStrictVar
    spacesAround $ char '='
    expr <- parseTernary <|> parseTerm
    newId <- incrementSid
    return $ SAssign var expr newId lineN

parseStmtExpr :: Parser' Stmt
parseStmtExpr = do
    lineN <- getLineN
    expr <- parseExpr
    fltr <- stmtEndFilter
    newId <- incrementSid
    return $ case fltr of
        Nothing -> SExpr expr newId lineN
        Just (fltrName,exprs) -> SFilter fltrName exprs (SExpr expr newId lineN) newId lineN
        
parseFilter :: Parser' Stmt
parseFilter = do
    try $ string "FILTER" >> spaces1
    lineN <- getLineN
    newId <- incrementSid
    (VarKey name) <- parseVarKey
    exprs <- try (parens $ sepBy parseExpr (spacesAround (char ','))) <|> return []
    stmtEnd
    body <- parseStmtSeq
    parseEnd
    return $ SFilter name exprs body newId lineN
    
parseStmtSeq :: Parser' Stmt
parseStmtSeq = do
    newId <- incrementSid
    lineN <- getLineN
    seq <- many1 (
        parseComment <|>
        parseText <|>
        parseIf <|>
        parseWhile <|>
        parseForeach <|>
        parseLast <|>
        parseNext <|>
        parseBlock <|>
        parseProcess <|>
        parseWrapper <|>
        parseFilter <|>
        (try $ parseAssign <* stmtEnd) <|>
        parseStmtExpr
        )
    return $ Seq seq newId lineN
    
parseTemplateWithStmtId :: T.Text -> TName -> StmtId -> Either ParseError Stmt
parseTemplateWithStmtId t tName sId = runParser parseStmtSeq (tName,sId) "" (T.concat [(T.pack "%]"), t, (T.pack "[%")])

parseTemplate tName t = parseTemplateWithStmtId t tName (0,0,0)
