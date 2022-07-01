module Intermediary where

import qualified Parser
import qualified TypeChecker
import qualified TypeAnnotations
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Data.List (intercalate, elemIndex)
import Data.Maybe (fromJust)

statementToIntermediary :: [TypeChecker.Type] -> TypeAnnotations.AnnotatedStatement -> String
statementToIntermediary typesList stmt = case stmt of
    TypeAnnotations.Assignment _ name val ->                        "=" ++ name ++ " " ++ expressionToIntermediary typesList val
    TypeAnnotations.DotAssignment _ struct field val ->             "." ++ expressionToIntermediary typesList struct ++ " " ++ field ++ " " ++ expressionToIntermediary typesList val
    TypeAnnotations.IndexAssignment _ array idx val ->              "[" ++ expressionToIntermediary typesList array ++ " " ++ expressionToIntermediary typesList idx ++ " " ++ expressionToIntermediary typesList val
    TypeAnnotations.FunctionDec _ name args block ->                "\\" ++ name ++ " " ++ show (length args) ++ " " ++ unwords (map (expressionToIntermediary typesList) args) ++ " " ++ statementToIntermediary typesList block
    TypeAnnotations.Block _ stmts ->                                "{" ++ show (length stmts) ++ " " ++ unwords (map (statementToIntermediary typesList) stmts)
    TypeAnnotations.If _ cond cons ->                               "?" ++ expressionToIntermediary typesList cond ++ " " ++ statementToIntermediary typesList cons
    TypeAnnotations.IfElse _ cond cons els ->                       ":" ++ expressionToIntermediary typesList cond ++ " " ++ statementToIntermediary typesList cons ++ " " ++ statementToIntermediary typesList els
    TypeAnnotations.Switch _ expr cases maybeDefault ->             "|" ++ show (length cases) ++ " " ++ expressionToIntermediary typesList expr ++ " " ++ unwords (map (\(val, block) -> expressionToIntermediary typesList val ++ " " ++ statementToIntermediary typesList block) cases) ++ " " ++ maybe "" (statementToIntermediary typesList) maybeDefault
    TypeAnnotations.TypeSwitch _ expr cases maybeDefault ->         "T" ++ show (length cases) ++ " " ++ expressionToIntermediary typesList expr ++ " " ++ unwords (map (\(val, block) -> expressionToIntermediary typesList val ++ " " ++ statementToIntermediary typesList block) cases) ++ " " ++ maybe "" (statementToIntermediary typesList) maybeDefault
    TypeAnnotations.For _ maybeInit maybeCond maybeIter block ->    "#" ++ maybe "" (statementToIntermediary typesList) maybeInit ++ " " ++ maybe "" (expressionToIntermediary typesList) maybeCond ++ " " ++ maybe "" (statementToIntermediary typesList) maybeIter ++ " " ++ statementToIntermediary typesList block
    TypeAnnotations.While _ cond block ->                           "@" ++ expressionToIntermediary typesList cond ++ " " ++ statementToIntermediary typesList block
    TypeAnnotations.Execution _ expr ->                             "!" ++ expressionToIntermediary typesList expr
    TypeAnnotations.Return _ expr ->                                "^" ++ expressionToIntermediary typesList expr
    TypeAnnotations.Using _ idents ->                               "u" ++ show (length idents) ++ " " ++ unwords (map (\(TypeAnnotations.Ident _ name typ) -> name) idents)
    TypeAnnotations.Break _ ->                                      ";"

expressionToIntermediary :: [TypeChecker.Type] -> TypeAnnotations.AnnotatedExpression -> String
expressionToIntermediary typesList expr = case expr of
    TypeAnnotations.Primitive expr ->               primitiveToIntermediary expr
    TypeAnnotations.Ident expr name typ ->          "(" ++ name ++ " " ++ sho typ
    TypeAnnotations.Array _ typ exprs ->            ":" ++ sho typ ++ " " ++ show (length exprs) ++ " " ++ unwords (map (expressionToIntermediary typesList) exprs)
    TypeAnnotations.Tuple _ exprs ->                "*" ++ show (length exprs) ++ " " ++ unwords (map (expressionToIntermediary typesList) exprs)
    TypeAnnotations.Ternary _ typ cond cons alt ->  "?" ++ sho typ ++ " " ++ expressionToIntermediary typesList cond ++ " " ++ expressionToIntermediary typesList cons ++ " " ++ expressionToIntermediary typesList alt
    TypeAnnotations.Call _ typ func args ->         "\\" ++ sho typ ++ " " ++ expressionToIntermediary typesList func ++ " " ++ expressionToIntermediary typesList args
    TypeAnnotations.Index _ typ coll idx ->         "[" ++ sho typ ++ " " ++ expressionToIntermediary typesList coll ++ " " ++ expressionToIntermediary typesList idx
    TypeAnnotations.Infix _ typ op left right ->    "-" ++ sho typ ++ " " ++ op ++ " " ++ expressionToIntermediary typesList left ++ " " ++ expressionToIntermediary typesList right
    TypeAnnotations.Prefix _ typ op right ->        "~" ++ sho typ ++ " " ++ op ++ " " ++ expressionToIntermediary typesList right
    TypeAnnotations.StructType _ fields ->          "$" ++ show (length fields) ++ " " ++ unwords (map (\(a, name)->expressionToIntermediary typesList a ++ " " ++ name) fields)
    TypeAnnotations.Struct _ typ fields ->          "{" ++ sho typ ++ " " ++ show (length fields) ++ " " ++ unwords (map (\(name, val)->name ++ " " ++ expressionToIntermediary typesList val) fields)
    TypeAnnotations.Dot _ typ expr fieldName ->     "." ++ sho typ ++ " " ++ expressionToIntermediary typesList expr ++ " " ++ fieldName
    TypeAnnotations.InterfaceType _ fields ->       "}" ++ show (length fields) ++ " " ++ unwords (map (\(a, name)->expressionToIntermediary typesList a ++ " " ++ name) fields)
    TypeAnnotations.Lambda _ typ args block ->      "l" ++ sho typ ++ " " ++ expressionToIntermediary typesList args ++ " " ++ statementToIntermediary typesList block
    where sho typ = typeToStringInt typ typesList

primitiveToIntermediary :: Parser.Expression -> String
primitiveToIntermediary expr = case expr of
    Parser.NullLiteral _ ->     "null"
    Parser.BoolLiteral _ b ->   if b then "true" else "false"
    Parser.IntLiteral _ i ->    '#':show i
    Parser.FloatLiteral _ f ->  '/':show f
    Parser.CharLiteral _ c ->   show c
    Parser.StringLiteral _ s -> primitiveToIntermediary $ Parser.ArrayLiteral undefined undefined (map (Parser.CharLiteral undefined) s)
    _ -> error ""

encode ast types = show (length ast) ++ " " ++ unwords (map (statementToIntermediary types) ast) ++ " " ++ show (length types) ++ " " ++ unlines (map show types)

typeToStringInt typ typesList = case typ `elemIndex` typesList of
    Nothing -> error $ show typesList ++ " " ++ show typ
    Just i -> show i




decode fn = Parser.run fn $ do
    l <- Parser.stringToInt <$> P.many1 P.digit
    P.count l $ P.char ' ' >> nStmt

data NExpression = NNull
                     | NIdent String
                     | NBool Bool
                     | NInt Int
                     | NFloat Float
                     | NChar Char
                     | NString String
                     | NTernary NExpression NExpression NExpression
                     | NIndex NExpression NExpression
                     | NTuple [NExpression]
                     | NArray [NExpression]
                     | NCall NExpression NExpression
                     | NInfix NExpression String NExpression
                     | NPrefix String NExpression
                     deriving (Eq)

instance Show NExpression where
    show NNull = "null"
    show (NIdent name) = name
    show (NBool b) = if b then "true" else "false"
    show (NInt i) = show i
    show (NFloat f) = show f
    show (NChar c) = show c
    show (NString s) = show s
    show (NTernary cond cons alt) = show cond ++ " ? " ++ show cons ++ " : " ++ show alt
    show (NIndex coll idx) = show coll ++ "[" ++ show idx ++ "]"
    show (NTuple exprs) = "(" ++ intercalate ", " (map show exprs) ++ ")"
    show (NArray exprs) = "[" ++ intercalate ", " (map show exprs) ++ "]"
    show (NCall func arg) = show func ++ show arg -- arg is a tuple, so the () are included already
    show (NInfix left op right) = show left ++ " " ++ op ++ " " ++ show right
    show (NPrefix op right) = op ++ show right

data NStatement = NAssignment String NExpression
                | NFunction String [String] NStatement
                | NBlock [NStatement]
                | NIf NExpression NStatement
                | NIfElse NExpression NStatement NStatement
                | NFor (Maybe NStatement) (Maybe NExpression) (Maybe NStatement) NStatement
                | NWhile NExpression NStatement
                | NExecution NExpression
                | NReturn NExpression
                deriving (Eq)

instance Show NStatement where
    show (NAssignment name val) = name ++ " = " ++ show val
    show (NFunction name args block) = name ++ "(" ++ intercalate ", " args ++ ") " ++ show block -- block is a NBlock so the {} are included
    show (NBlock stmts) = "{\n" ++ intercalate ";\n" (map show stmts) ++ ";\n}"
    show (NIf cond cons) = "if (" ++ show cond ++ ") " ++ show cons
    show (NIfElse cond cons alt) = "if (" ++ show cond ++ ") " ++ show cons ++ " else " ++ show alt
    show (NFor mbInit mbCond mbIter block) = "for (" ++ maybe " " show mbInit ++ "; " ++ maybe "" show mbCond ++ "; " ++ maybe "" show mbIter ++ ") " ++ show block
    show (NWhile cond block) = "while (" ++ show cond ++ ") " ++ show block
    show (NExecution expr) = show expr
    show (NReturn expr) = "return " ++ show expr

nNull = Parser.nullLiteral >> return NNull

nIdent = P.char '(' >> Parser.identLiteral >>= \(Parser.IdentLiteral _ name) -> return $ NIdent name

nBool = Parser.boolLiteral >>= \(Parser.BoolLiteral _ b) -> return $ NBool b

nInt = P.char '#' >> Parser.stringToInt <$> P.many1 P.digit >>= \i -> return $ NInt i

nFloat = P.char '/' >> P.many1 P.digit >>= \whole -> P.char '.' >> P.many1 P.digit >>= \rest -> return $ NFloat $ Parser.stringToFloat $ whole ++ '.':rest

nChar = Parser.charLiteral >>= \(Parser.CharLiteral _ c) -> return $ NChar c

nString = Parser.stringLiteral >>= \(Parser.StringLiteral _ s) -> return $ NString s

nIndex = P.try $ do
    P.char '['
    coll <- nExpr
    P.char ' '
    NIndex coll <$> nExpr

nArray = P.try $ do
    P.char ':'
    len <- Parser.stringToInt <$> P.many1 P.digit
    fmap NArray $ P.count len $ Parser.ws >> nExpr

nTuple = P.try $ do
    P.char '*'
    len <- Parser.stringToInt <$> P.many1 P.digit
    fmap NTuple $ P.count len $ Parser.ws >> nExpr

nTernary = P.try $ do
    P.char '?'
    cond <- nExpr
    P.char ' '
    cons <- nExpr
    P.char ' '
    NTernary cond cons <$> nExpr

nCall = P.try $ do
    P.char '\\'
    func <- nExpr
    P.char ' '
    NCall func <$> nExpr

nInfix = P.try $ do
    P.char '-'
    op <- P.manyTill P.anyChar (P.char ' ')
    left <- nExpr
    P.char ' '
    NInfix left op <$> nExpr

nPrefix = P.try $ do
    P.char '~'
    op <- P.manyTill P.anyChar (P.char ' ')
    NPrefix op <$> nExpr

nExpr = nNull <|> nBool <|> nIdent <|> nInt <|> nFloat <|> nChar <|> nString <|> nIndex <|> nArray <|> nTuple <|> nTernary <|> nCall <|> nInfix <|> nPrefix

nAssignment = P.try $ do
    P.char '='
    (Parser.IdentLiteral _ name) <- Parser.identLiteral
    P.char ' '
    NAssignment name <$> nExpr

nFunction = P.try $ do
    P.char '\\'
    (Parser.IdentLiteral _ name) <- Parser.identLiteral
    P.char ' '
    argsLen <- Parser.stringToInt <$> P.many1 P.digit
    argIdents <-
        if argsLen > 0 then
            P.count argsLen (P.char ' ' >> Parser.identLiteral)
        else
            P.char ' ' >> return []
    let args = map (\(Parser.IdentLiteral _ n) -> n) argIdents
    P.char ' '
    NFunction name args <$> nBlock

nBlock = P.try $ do
    P.char '{'
    len <- Parser.stringToInt <$> P.many1 P.digit
    NBlock <$>
        if len > 0 then
            P.count len (P.char ' ' >> nStmt)
        else
            P.char ' ' >> return []

nIf = P.try $ do
    P.char '?'
    cond <- nExpr
    P.char ' '
    NIf cond <$> nStmt

nIfElse = P.try $ do
    P.char ':'
    cond <- nExpr
    P.char ' '
    cons <- nStmt
    P.char ' '
    NIfElse cond cons <$> nStmt

nFor = P.try $ do
    P.char '#'
    mbInit <- P.optionMaybe nStmt
    P.char ' '
    mbCond <- P.optionMaybe nExpr
    P.char ' '
    mbIter <- P.optionMaybe nStmt
    P.char ' '
    NFor mbInit mbCond mbIter <$> nStmt

nWhile = P.try $ do
    P.char '@'
    cond <- nExpr
    P.char ' '
    NWhile cond <$> nStmt

nExecution = P.try $ do
    P.char '!'
    NExecution <$> nExpr

nReturn = P.try $ do
    P.char '^'
    NReturn <$> nExpr

nStmt = nAssignment <|> nFunction <|> nBlock <|> nIf <|> nIfElse <|> nFor <|> nWhile <|> nExecution <|> nReturn

display :: [NStatement] -> [Char]
display a = intercalate ";\n" (map show a) ++ ";"