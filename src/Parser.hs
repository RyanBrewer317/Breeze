module Parser where

import qualified Text.Parsec as P
import Text.Parsec ((<|>), (<?>))
import qualified Control.Monad

data Statement = DeclarationStatement P.SourcePos Expression String Expression -- type name value
                  | TypeDeclaration P.SourcePos String Expression -- name value (the value will be evaluated at compile time and used in typechecking)
                  | AssignmentStatement P.SourcePos String Expression -- name new-value
                  | FunctionStatement P.SourcePos Expression String [Expression] Statement -- type name args block
                  | BlockStatement P.SourcePos [Statement] -- [lines]
                  | IfStatement P.SourcePos Expression Statement (Maybe Statement)-- if-condition, if-consequence, Maybe else-consequence
                  | SwitchStatement P.SourcePos Expression [(Expression, Statement)] (Maybe Statement) -- switch-value case-expression-block-pairs default-block
                  | TypeSwitchStatement P.SourcePos Expression [(Expression, Statement)] (Maybe Statement) -- switch-value case-expression-block-pairs default-block
                  | ForLoopStatement P.SourcePos (Maybe Statement) (Maybe Expression) (Maybe Statement) Statement -- init condition iterate block
                  | WhileLoopStatement P.SourcePos Expression Statement -- condition block
                  | ExecutionStatement P.SourcePos Expression -- function-call
                  | ReturnStatement P.SourcePos Expression
                  | UsingStatement P.SourcePos [Expression]
                  | BreakStatement P.SourcePos
                  | DotAssignmentStatement P.SourcePos Expression String Expression -- struct field-name value
                  | IndexAssignmentStatement P.SourcePos Expression Expression Expression -- array index value
                --   | GenericDeclaration P.SourcePos String [String] Expression -- name args value (the value will be evaluated at compile time for each set of arguments and used in typechecking)
                  deriving (Show, Eq)

data Expression = NullLiteral P.SourcePos
                    | IdentLiteral P.SourcePos String -- name
                    | IdentDeclarationLiteral P.SourcePos Expression String -- type name
                    | BoolLiteral P.SourcePos Bool
                    | IntLiteral P.SourcePos Int
                    | FloatLiteral P.SourcePos Float
                    | CharLiteral P.SourcePos Char
                    | StringLiteral P.SourcePos String
                    | ArrayLiteral P.SourcePos Expression [Expression] -- type [vals]
                    | TernaryLiteral P.SourcePos Expression Expression Expression -- condition consequence alternative
                    | StructTypeLiteral P.SourcePos [(Expression, String)] -- [(type, name)]
                    | StructLiteral P.SourcePos Expression [(String, Expression)] -- type [(field-name, field-val)]
                    | DotLiteral P.SourcePos Expression String -- expr.fieldName
                    | InterfaceLiteral P.SourcePos [(Expression, String)] -- [(type, name)]
                    -- | HashLiteral P.SourcePos Expression (Map String Expression) -- type (map string vals)
                    | LambdaLiteral P.SourcePos Expression Expression Statement -- type tuple-args block
                    | TupleLiteral P.SourcePos [Expression] -- [vals]
                    | IndexLiteral P.SourcePos Expression Expression -- array/tuple/hash index
                    | FunctionCallLiteral P.SourcePos Expression Expression -- function tuple-args
                    | InfixLiteral P.SourcePos String Expression Expression -- left op right
                    | PrefixLiteral P.SourcePos String Expression -- op right
                    deriving (Show, Eq)

type Parser = P.Parsec String ()

run :: String -> Parser a -> String -> Either P.ParseError a
run fn p = P.runParser p () fn

nullLiteral = do
    pos <- P.getPosition
    P.try $ P.string "null"
    return $ NullLiteral pos

boolLiteral = do
    pos <- P.getPosition
    b <- P.try $ P.string "true" <|> P.string "false"
    return $ BoolLiteral pos $ b == "true"

identLiteral = do
    pos <- P.getPosition
    first <- P.try P.letter
    rest <- P.many $ P.alphaNum <|> P.char '_'
    return $ IdentLiteral pos $ first:rest

stringToInt :: String -> Int
stringToInt = read

intLiteral = do
    pos <- P.getPosition
    i <- P.try $ P.many1 P.digit
    return $ IntLiteral pos $ stringToInt i

stringToFloat :: String -> Float
stringToFloat = read

floatLiteral = P.try $ do
    pos <- P.getPosition
    whole <- P.many1 P.digit
    P.char '.'
    decimal <- P.many1 P.digit
    return $ FloatLiteral pos $ stringToFloat $ whole ++ "." ++ decimal

charLiteral = do
    pos <- P.getPosition
    P.char '\''
    escaped <- P.option 'r' $ P.char '\\'
    c <- P.anyChar
    P.char '\''
    return $ CharLiteral pos $ if escaped == '\\' then
        case c of
            'n' -> '\n'
            't' -> '\t'
            'r' -> '\r'
            _ -> c
    else c

stringLiteral = do
    pos <- P.getPosition
    P.char '"'
    contents <- P.many chars
    P.char '"' <?> "\"\"\""
    return $ StringLiteral pos contents
    where
        chars = escaped <|> P.noneOf "\"" <?> "string contents"
        escaped = (P.char '\\' <?> "\\") >> P.choice (zipWith escapedChar codes replacements)
        escapedChar :: Char -> Char -> Parser Char
        escapedChar code replacement = P.char code >> return replacement
        codes        = ['n',  'r',  't',  '\\', '\"']
        replacements = ['\n', '\r', '\t', '\\', '\"']

ws = P.skipMany $ (P.char ' ' <|> P.char '\n' <|> P.char '\t' <?> "whitespace") <|> (lineComment <|> multilineComment <?> "comment")

reserved :: Parser a -> Parser a
reserved p = do
    out <- p
    P.notFollowedBy $ P.alphaNum <|> P.char '_'
    return out

commaSep p = P.sepBy p $ ws >> P.char ',' >> ws
semicolonSep p = P.endBy p $ ws >> P.many1 (P.char ';') >> ws

arrayLiteral = do
    P.char '{' <?> "array literal"
    items <- commaSep expression <?> "array elements"
    P.char '}'
    return items

tupleLiteral = do
    pos <- P.getPosition
    P.char '('
    args <- commaSep expression <?> "tuple elements"
    P.char ')'
    return $ case args of
        [x] -> x
        x:xs -> TupleLiteral pos args
        [] -> NullLiteral pos

lambdaLiteral = do
    pos <- P.getPosition
    P.char '\\'
    typ <- typeLiteral
    argsPos <- P.getPosition
    P.char '('
    ws
    args <- commaSep (identDeclarationLiteral <?> "arguments")
    ws
    P.char ')'
    ws
    LambdaLiteral pos typ (TupleLiteral argsPos args) <$> blockStatement

structTypeLiteral = do
    pos <- P.getPosition
    P.try $ reserved $ P.string "struct"
    ws
    P.char '{'
    fields <- commaSep (do
        (IdentDeclarationLiteral _ typ name) <- identDeclarationLiteral
        return (typ, name))
    ws
    P.char '}'
    return $ StructTypeLiteral pos fields

interfaceLiteral = do
    pos <- P.getPosition
    P.try $ reserved $ P.string "interface"
    ws
    P.char '{'
    fields <- commaSep (do
        (IdentDeclarationLiteral _ typ name) <- identDeclarationLiteral
        return (typ, name))
    ws
    P.char '}'
    return $ InterfaceLiteral pos fields

structLiteral = do
    fields <- P.try $ do
        P.char '{' <?> "struct literal"
        ws
        commaSep $ do
            (IdentLiteral _ name) <- identLiteral
            ws
            P.char ':'
            expr <- expression
            return (name, expr)
    P.char '}'
    return fields

identDeclarationLiteral = do
    pos <- P.getPosition
    (typ, name) <- P.try $ do
        typ <- typeLiteral <?> "type"
        (IdentLiteral _ name) <- identLiteral <?> "name"
        return (typ, name)
    return $ IdentDeclarationLiteral pos typ name

mkInfix :: String -> Parser (Expression -> Expression -> Expression)
mkInfix s = do
    pos <- P.getPosition
    P.try (P.string s <?> "operator")
    return $ InfixLiteral pos s

-- precedences
infix0 = P.chainl1 infix1  (mkInfix "||"
                        <|> mkInfix "=>")
infix1 = P.chainl1 infix2  (mkInfix "&&"
                        <|> mkInfix "->")
infix2 = P.chainl1 infix3  (mkInfix "<"
                        <|> mkInfix ">"
                        <|> mkInfix ">="
                        <|> mkInfix "<="
                        <|> mkInfix "!="
                        <|> mkInfix "==")
infix3 = P.chainl1 infix4  (mkInfix "+"
                        <|> mkInfix "-")
infix4 = P.chainl1 infix5  (mkInfix "*"
                        <|> mkInfix "/")
infix5 = P.chainl1 innerExpression (mkInfix "^")

expression = do
    expr <- infix0
    pos <- P.getPosition
    maybeTernary <- P.optionMaybe (P.char '?' <?> "operator")
    case maybeTernary of
        Nothing -> return expr
        Just _ -> do
            cons <- expression
            P.char ':'
            TernaryLiteral pos expr cons <$> expression

innerExpression = do
    ws
    prefixPos <- P.getPosition
    maybePrefix <- P.optionMaybe (P.string "!" <|> P.string "-" <|> P.try (P.string "[]") <?> "prefix operator")
    ws
    expr <- nullLiteral
        <|> boolLiteral
        <|> structTypeLiteral
        <|> interfaceLiteral
        <|> identLiteral
        <|> tupleLiteral
        <|> lambdaLiteral
        <|> floatLiteral
        <|> intLiteral
        <|> charLiteral
        <|> stringLiteral
        <?> "expression literal"
    expr <- return $ case maybePrefix of
        Just op -> PrefixLiteral prefixPos op expr
        Nothing -> expr
    ws
    expr <- fold expr . reverse <$> P.many (tryCall <|> tryStruct <|> tryArray <|> tryIndex <|> tryDot)
    ws
    return expr
    where
        fold :: Expression -> [Expression->Expression]-> Expression
        fold = foldr ($)
        tryCall = do
            pos <- P.getPosition
            call <- tupleLiteral <?> "function call"
            ws
            return $ \expr -> case call of
                TupleLiteral {} -> FunctionCallLiteral pos expr call
                NullLiteral {} -> FunctionCallLiteral pos expr $ TupleLiteral pos []
                _ -> FunctionCallLiteral pos expr $ TupleLiteral pos [call]
        tryArray = do
            pos <- P.getPosition
            array <- arrayLiteral
            ws
            return $ \expr -> ArrayLiteral pos expr array
        tryStruct = do
            pos <- P.getPosition
            fields <- structLiteral
            ws
            return $ \expr -> StructLiteral pos expr fields
        tryIndex = do
            pos <- P.getPosition
            index <- (P.char '[' *> (expression <?> "expression") <* P.char ']') <?> "index"
            ws
            return $ \expr -> IndexLiteral pos expr index
        tryDot = do
            pos <- P.getPosition
            P.char '.'
            (IdentLiteral _ name) <- identLiteral
            ws
            return $ \expr -> DotLiteral pos expr name

declarationStatement = do
    (pos, typ, name) <- P.try $ do
        (IdentDeclarationLiteral pos typ name) <- identDeclarationLiteral
        ws
        P.char '='
        return (pos, typ, name)
    DeclarationStatement pos typ name <$> expression

typeDeclarationStatement = do
    pos <- P.getPosition
    P.try $ reserved $ P.string "typedef"
    ws
    (IdentLiteral _ name) <- identLiteral <?> "type name"
    ws
    P.char '='
    TypeDeclaration pos name <$> expression <?> "type expression"

assignmentStatement = do
    (pos, name) <- P.try $ do
        (IdentLiteral pos name) <- identLiteral <?> "identifier"
        ws
        P.char '='
        return (pos, name)
    AssignmentStatement pos name <$> expression <?> "expression"

blockStatement = do
    pos <- P.getPosition
    P.char '{'
    ws
    mbUsing <- P.optionMaybe (semicolon usingStatement <?> "using statement")
    let first = case mbUsing of
            Nothing -> []
            Just using -> [using]
    lines <- P.many statement <?> "statements"
    P.char '}'
    return $ BlockStatement pos $ first ++ lines

functionStatement = do
    (pos, typ, name) <- P.try $ do
        (IdentDeclarationLiteral pos typ name) <- identDeclarationLiteral
        ws
        P.char '('
        return (pos, typ, name)
    ws
    args <- commaSep identDeclarationLiteral <?> "parameters"
    ws
    P.char ')'
    ws
    FunctionStatement pos typ name args <$> blockStatement <?> "block"

ifStatement = do
    pos <- P.getPosition
    reserved $ P.try $ P.string "if"
    ws
    P.char '('
    cond <- expression <?> "condition"
    P.char ')'
    block <- statement <?> "consequence"
    maybeElse <- P.optionMaybe $ reserved $ P.string "else" >> (statement <?> "alternative")
    return $ IfStatement pos cond block maybeElse

forLoopStatement = do
    pos <- P.getPosition
    reserved $ P.try $ P.string "for"
    ws
    P.char '('
    ws
    init <- P.optionMaybe statementNoSemicolon <?> "initializer"
    P.char ';'
    ws
    cond <- P.optionMaybe expression <?> "condition"
    P.char ';'
    ws
    iter <- P.optionMaybe statementNoSemicolon <?> "iterator"
    P.char ')'
    ws
    ForLoopStatement pos init cond iter <$> statement <?> "block"

whileLoopStatement = do
    pos <- P.getPosition
    reserved $ P.try $ P.string "while"
    ws
    P.char '('
    cond <- expression <?> "condition"
    P.char ')'
    WhileLoopStatement pos cond <$> statement

switchStatement = do
    pos <- P.getPosition
    P.try $ reserved $ P.string "switch"
    ws
    P.char '('
    expr <- constantLiteral <?> "constant expression"
    P.char ')'
    ws
    P.char '{'
    ws
    cases <- P.many (do
        reserved $ P.string "case"
        ws
        P.char '('
        val <- constantLiteral
        P.char ')'
        block <- statement
        return (val, block))
    defalt <- P.optionMaybe $ reserved $ P.string "default" >> statement
    P.char '}'
    return $ SwitchStatement pos expr cases defalt

typeSwitchStatement = do
    pos <- P.getPosition
    P.try $ reserved $ P.string "typeswitch"
    ws
    P.char '('
    expr <- identLiteral <?> "identifier"
    P.char ')'
    ws
    P.char '{'
    ws
    cases <- P.many (do
        reserved $ P.string "case"
        ws
        P.char '('
        val <- constantLiteral
        P.char ')'
        block <- statement
        return (val, block)
        )
    defalt <- P.optionMaybe $ reserved $ P.string "default" >> statement
    P.char '}'
    return $ TypeSwitchStatement pos expr cases defalt

typeLiteral = P.chainl1 typeLiteral2 $ mkInfix "+"
typeLiteral2 = P.chainl1 typeLiteral3 $ mkInfix "*"
typeLiteral3 = P.chainl1 innerTypeLiteral $ mkInfix "->"

innerTypeLiteral = do
    ws
    pos <- P.getPosition
    maybeArray <- P.optionMaybe $ P.try (P.string "[]" <?> "operator")
    ws
    lit <- parentheticalLiteral typeLiteral <|> (identLiteral <?> "type name") <|> structTypeLiteral <|> interfaceLiteral
    lit <- return $ case maybeArray of
        Nothing -> lit
        Just op -> PrefixLiteral pos op lit
    ws
    return lit

constantLiteral = do
    ws
    prefixPos <- P.getPosition
    maybeMinus <- P.optionMaybe (P.string "-" <?> "operator")
    ws
    lit <- P.choice [nullLiteral, boolLiteral, identLiteral, charLiteral, intLiteral, floatLiteral, parentheticalLiteral constantLiteral, typeLiteral] <?> "expression literal"
    lit <- case maybeMinus of
        Nothing -> return lit
        Just minus -> case lit of
            IntLiteral pos n -> return $ IntLiteral pos (-n)
            FloatLiteral pos n -> return $ FloatLiteral pos (-n)
            _ -> P.unexpected "minus sign"
    ws
    return lit

parentheticalLiteral :: Parser a -> Parser a
parentheticalLiteral p = P.char '(' *> ws *> p <* ws <* P.char ')'

expressionStatement = P.try (do
    expr <- expression
    case expr of
        FunctionCallLiteral pos _ _ -> return $ ExecutionStatement pos expr
        DotLiteral pos struct field -> do
            P.char '='
            val <- expression <?> "expression"
            return $ DotAssignmentStatement pos struct field val
        IndexLiteral pos array idx -> do
            P.char '='
            val <- expression <?> "expression"
            return $ IndexAssignmentStatement pos array idx val
        _ -> P.unexpected "expression") <?> "function call"

returnStatement = do
    pos <- P.getPosition
    P.try $ reserved $ P.string "return"
    ReturnStatement pos <$> expression <?> "expression"

usingStatement = do
    pos <- P.getPosition
    reserved $ P.try $ P.string "using"
    ws
    UsingStatement pos <$> (P.sepBy1 identLiteral (ws >> P.char ',' >> ws) <?> "identifiers")

breakStatement :: Parser Statement
breakStatement = do
    pos <- P.getPosition
    P.string "break"
    return $ BreakStatement pos

semicolon :: Parser Statement -> Parser Statement
semicolon p = p >>= \out -> ws >> (P.char ';' <?> "semicolon") >> return out

statement :: Parser Statement
statement = do
    ws
    stmt <- semicolon typeDeclarationStatement <|> switchStatement <|> typeSwitchStatement <|> forLoopStatement <|> ifStatement <|> whileLoopStatement <|> semicolon returnStatement <|> semicolon breakStatement <|> semicolon declarationStatement <|> semicolon assignmentStatement <|> functionStatement <|> blockStatement <|> semicolon expressionStatement <?> "statement"
    ws
    return stmt

statementNoSemicolon = do
    ws
    stmt <- typeDeclarationStatement <|> switchStatement <|> typeSwitchStatement <|> forLoopStatement <|> ifStatement <|> whileLoopStatement <|> returnStatement <|> breakStatement <|> declarationStatement <|> assignmentStatement <|> functionStatement <|> blockStatement <|> expressionStatement <?> "statement"
    ws
    return stmt

lineComment :: Parser Char
lineComment = do
    P.try $ P.string "//"
    P.manyTill P.anyChar (Control.Monad.void (P.char '\n') <|> P.eof) <?> "line comment"
    return ' '

multilineComment :: Parser Char
multilineComment = do
    P.try $ P.string "/*"
    P.manyTill P.anyChar (P.string "*/") <?> "multiline comment"
    return ' '

parse = P.many1 statement <* P.eof



posOf :: Expression -> P.SourcePos
posOf expr = case expr of
    NullLiteral pos -> pos
    IdentLiteral pos _ -> pos
    IdentDeclarationLiteral pos _ _ -> pos
    BoolLiteral pos _ -> pos
    IntLiteral pos _ -> pos
    FloatLiteral pos _ -> pos
    CharLiteral pos _ -> pos
    StringLiteral pos _ -> pos
    IndexLiteral pos _ _ -> pos
    ArrayLiteral pos _ _ -> pos
    TernaryLiteral pos _ _ _ -> pos
    LambdaLiteral pos _ _ _ -> pos
    StructTypeLiteral pos _ -> pos
    StructLiteral pos _ _ -> pos
    DotLiteral pos _ _ -> pos
    InterfaceLiteral pos _ -> pos
    TupleLiteral pos _ -> pos
    FunctionCallLiteral pos _ _ -> pos
    InfixLiteral pos _ _ _ -> pos
    PrefixLiteral pos _ _ -> pos

posOfStatement :: Statement -> P.SourcePos
posOfStatement stmt = case stmt of
    DeclarationStatement pos _ _ _ -> pos
    TypeDeclaration pos _ _ -> pos
    AssignmentStatement pos _ _ -> pos
    FunctionStatement pos _ _ _ _ -> pos
    BlockStatement pos _ -> pos
    IfStatement pos _ _ _ -> pos
    SwitchStatement pos _ _ _ -> pos
    TypeSwitchStatement pos _ _ _ -> pos
    ForLoopStatement pos _ _ _ _ -> pos
    WhileLoopStatement pos _ _ -> pos
    ExecutionStatement pos _ -> pos
    ReturnStatement pos _ -> pos
    UsingStatement pos _ -> pos
    BreakStatement pos -> pos
    DotAssignmentStatement pos _ _ _ -> pos
    IndexAssignmentStatement pos _ _ _ -> pos