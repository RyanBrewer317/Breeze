module TypeChecker where

import qualified Parser
import qualified Text.Parsec as P
import Data.List (intersect, intercalate, delete)
import Data.Map (Map, (!), insert, union, fromList, empty, member, partitionWithKey, toAscList, intersection)
import Data.Maybe (fromMaybe)
import qualified Data.List as L

data Type = IntType
          | FloatType
          | BoolType
          | VoidType
          | NoType
          | CharType
          | ArrayType Type
          | TypeType
          | TupleType Type Type -- a product type represented as a linked list
          | FunctionType Type Type -- args-tuple return
          | UnionType Type Type -- a sum type represented as a linked list
          | StructType [(Type, String)] -- a struct with fields
          | InterfaceType [(Type, String)] -- an interface with fields

instance Eq Type where
    (InterfaceType fields1) == (StructType fields2)     = (length fields1 <= length fields2) && all (\((type1, name1), (type2, name2))->type1==type2 && name1==name2) (zip fields1 fields2)
    (StructType fields2) == (InterfaceType fields1)     = (length fields1 <= length fields2) && all (\((type1, name1), (type2, name2))->type1==type2 && name1==name2) (zip fields1 fields2)
    IntType == IntType                                  = True
    FloatType == FloatType                              = True
    BoolType == BoolType                                = True
    VoidType == VoidType                                = True
    NoType == NoType                                    = True
    CharType == CharType                                = True
    (ArrayType a) == (ArrayType b)                      = a == b
    TypeType == TypeType                                = True
    (TupleType a b) == (TupleType c d)                  = a == c && b == d
    (FunctionType a b) == (FunctionType c d)            = a == c && b == d
    (UnionType a b) == (UnionType c d)                  = a == c && b == d
    (StructType fields1) == (StructType fields2)        = (length fields1 == length fields2) && all (\((type1, name1), (type2, name2))->type1==type2 && name1==name2) (zip fields1 fields2)
    (InterfaceType fields1) == (InterfaceType fields2)  = all (\((type1, name1), (type2, name2))->type1==type2 && name1==name2) (zip fields1 fields2)
    _ == _ = False

instance Show Type where
    show IntType = "Int"
    show FloatType = "Float"
    show BoolType = "Bool"
    show VoidType = "Void"
    show NoType = "NoType"
    show CharType = "Char"
    show (ArrayType t) = "[]" ++ show t
    show TypeType = "Type"
    show (TupleType left right) = intercalate "*" $ map show $ linkedTupleTypeListToList (TupleType left right)
    show (FunctionType args ret) = show args ++ "->" ++ show ret
    show (UnionType left right) = intercalate "+" $ map show $ linkedUnionTypeListToList (UnionType left right)
    show (StructType fields) = "struct{" ++ intercalate ", " (map (\(typ, name)->show typ ++ " " ++ name) fields) ++ "}"
    show (InterfaceType fields) = "interface{" ++ intercalate ", " (map (\(typ, name)->show typ ++ " " ++ name) fields) ++ "}"

linkedTupleTypeListToList (TupleType left right) = left:linkedTupleTypeListToList right
linkedTupleTypeListToList t = [t]
linkedUnionTypeListToList (UnionType left right) = left:linkedUnionTypeListToList right
linkedUnionTypeListToList t = [t]
listToLinkedUnionTypeList [x] = x
listToLinkedUnionTypeList (x:xs) = combineTypes x $ listToLinkedUnionTypeList xs
listToLinkedUnionTypeList _ = NoType

type Scope = Map String Type -- map of types
type TypeScope = Map String Type -- map of values, which are types

stringErr :: String -> P.SourcePos -> String -> String
stringErr source pos msg = show pos ++ ":\n" ++ msg ++ "\n" ++ lines source !! (P.sourceLine pos - 1) ++ "\n" ++ replicate (P.sourceColumn pos - 1) ' ' ++ "^^"
typeErr :: String -> P.SourcePos -> Type -> Type -> String
typeErr source pos expected got = stringErr source pos $ "TypeError: expected " ++ show expected ++ ", got " ++ show got
nameErr :: String -> P.SourcePos -> [Char] -> String
nameErr source pos name = stringErr source pos $ "NameError: unknown identifier " ++ name

startingScope = fromList [
            ("print", FunctionType (ArrayType CharType) VoidType),
            ("intToString", FunctionType IntType (ArrayType CharType)),
            ("len", FunctionType (ArrayType CharType) IntType)
        ]

startingTypeScope = fromList [
            ("Int", IntType),
            ("String", ArrayType CharType),
            ("Void", VoidType),
            ("Float", FloatType),
            ("Char", CharType),
            ("Bool", BoolType),
            ("Type", TypeType)
        ]

typecheck source stmts = case stmts of
    [] -> Left "Error: no valid statements."
    _ -> typecheckStatements source (Parser.posOfStatement $ head stmts) stmts startingScope startingTypeScope NoType NoType

typecheckStatements :: String -> P.SourcePos -> [Parser.Statement] -> Scope -> TypeScope -> Type -> Type -> Either String Type
typecheckStatements source pos stmts scope typeScope expectedType typeSoFar
    | not $ null stmts = do
        let stmt = head stmts
        let rest = tail stmts
        typ <- typecheckStatement source stmt scope typeScope expectedType
        let newTypeSoFar = combineTypes typeSoFar typ
        case stmt of
            Parser.DeclarationStatement _ typExpr name _ -> evalType source typeScope typExpr >>= \typ -> typecheckStatements source pos rest (insert name typ scope) typeScope expectedType newTypeSoFar
            Parser.FunctionStatement _ retExpr name args block -> do
                returnType <- evalType source typeScope retExpr
                argType <- argsToTupleType source typeScope args
                typecheckStatements source pos rest (insert name (FunctionType argType returnType) scope) typeScope expectedType newTypeSoFar
            Parser.TypeDeclaration _ name typExpr -> evalType source typeScope typExpr >>= \typ -> typecheckStatements source pos rest scope (insert name typ typeScope) expectedType newTypeSoFar
            Parser.ReturnStatement _ _ -> Right expectedType -- the validity of the return statement has already been checked, this just stops typechecking from going past a return
            Parser.IfStatement _ _ _ mbElse -> maybe (tc rest newTypeSoFar) (\_->if NoType `notElem` linkedUnionTypeListToList typ then Right expectedType else tc rest newTypeSoFar) mbElse
            Parser.BreakStatement _ -> Right newTypeSoFar
            _ -> tc rest newTypeSoFar
    | expectedType == listToLinkedUnionTypeList ((VoidType:) $ delete NoType $ linkedUnionTypeListToList typeSoFar) =
        Right expectedType
    | expectedType == NoType =
        Right NoType
    | otherwise =
        Left $ stringErr source pos $ "TypeError: expected " ++ show expectedType ++ ", might return Void"
    where tc rest = typecheckStatements source pos rest scope typeScope expectedType

argsToTupleType :: String -> TypeScope -> [Parser.Expression] -> Either String Type
argsToTupleType source typeScope = f where
    f [] = Right VoidType
    f [Parser.IdentDeclarationLiteral _ typ _] = evalType source typeScope typ
    f ((Parser.IdentDeclarationLiteral _ typ1 _):xs) = do
        left <- evalType source typeScope typ1
        right <- argsToTupleType source typeScope xs
        return $ TupleType left right
    f (_:_) = error ""

typecheckBlock :: String -> P.SourcePos -> Scope -> TypeScope -> Scope -> TypeScope -> Type -> Type -> [Parser.Statement] -> Either String Type
typecheckBlock source pos scope typeScope insertedScope insertedTypeScope expectedType typeSoFar statements = case statements of
    [] -> Right NoType
    first:rest -> case first of
        (Parser.UsingStatement _ idents) ->
            case mapM (typeOf source scope typeScope) idents of
                Left s -> Left s
                Right types ->
                    let names = map (\(Parser.IdentLiteral _ name) -> name) idents in
                        let (varIdents, typeNames) = partitionWithKey (\k a -> member k scope) $ fromList (zip names types) in
                            let newScope = insertedScope `union` varIdents in
                                let newTypeScope = insertedTypeScope `intersection` fromList (zip (map fst $ toAscList typeNames) $ repeat VoidType) in
                                    typecheckStatements source pos rest newScope newTypeScope expectedType typeSoFar
        _ -> typecheckStatements source pos statements insertedScope insertedTypeScope expectedType typeSoFar

typecheckStatement :: String -> Parser.Statement -> Scope -> TypeScope -> Type -> Either String Type
typecheckStatement source stmt scope typeScope expectedType =
    case stmt of
        Parser.DeclarationStatement _ typ _ val -> do
            identType <- evalType source typeScope typ
            typecheckExpression source scope typeScope val identType
            return NoType
        Parser.TypeDeclaration _ name val -> evalType_ source typeScope val >> return NoType
        Parser.FunctionStatement _ typ name args block -> do
            returnType <- evalType source typeScope typ
            argType <- argsToTupleType source typeScope args
            let recursiveScope = insert name (FunctionType argType returnType) scope
            case mapM (typeOf source recursiveScope typeScope) args of -- this just makes sure they're written validly, no undefined type constants etc
                Left s -> Left s
                Right _ -> return ()
            case mapM (\ (Parser.IdentDeclarationLiteral _ te n) -> evalType source typeScope te >>= \t -> return (n, t)) args of
                    Left s -> Left s
                    Right tns ->
                        let argScope = fromList tns in
                            case block of
                                Parser.BlockStatement pos statements -> typecheckBlock source pos (argScope `union` recursiveScope) typeScope (insert name (FunctionType argType returnType) argScope) startingTypeScope returnType NoType statements
                                _ -> error "" -- the statement of a function statement can only be a BlockStatement
            return NoType
        Parser.AssignmentStatement pos name val -> do
            typ <- if name `member` scope then return $ scope ! name else if name `member` typeScope then return TypeType else Left $ nameErr source pos name
            typecheckExpression source scope typeScope val typ
            return NoType
        Parser.DotAssignmentStatement pos struct field val -> do
            structType <- typeOf source scope typeScope struct
            let (StructType fields) = structType
            let typ = fromList (map (\(a, b)->(b, a)) fields) ! field
            typecheckExpression source scope typeScope val typ
            return NoType
        Parser.IndexAssignmentStatement pos array idx val -> do
            arrayType <- typeOf source scope typeScope array
            let (ArrayType innerType) = arrayType
            typecheckExpression source scope typeScope val innerType
            return NoType
        Parser.BlockStatement pos statements ->
            typecheckBlock source pos scope typeScope startingScope startingTypeScope expectedType expectedType statements
        Parser.IfStatement pos cond cons maybeElse -> do
            typecheckExpression source scope typeScope cond BoolType
            typ <- typecheckStatement source cons scope typeScope expectedType
            case maybeElse of
                Nothing -> return typ
                Just elseStatement -> do
                    typ2 <- typecheckStatement source elseStatement scope typeScope expectedType
                    return $ combineTypes typ typ2
        Parser.SwitchStatement pos expr cases mbDefault -> do
            typ <- typeOf source scope typeScope expr
            caseTypes <- mapM (\(val, block) -> typecheckExpression source scope typeScope val typ >> typecheckStatement source block scope typeScope expectedType) cases
            defType <- case mbDefault of
                Nothing -> return []
                Just d -> (:[]) <$> typecheckStatement source d scope typeScope expectedType
            let types = caseTypes ++ defType
            return $ listToLinkedUnionTypeList types
        Parser.TypeSwitchStatement pos ident cases mbDefault -> do
            typeOf source scope typeScope ident
            let (Parser.IdentLiteral _ name) = ident
            caseTypes <- mapM (\(typeExpr, block) -> evalType source typeScope typeExpr >>= \typ -> typecheckStatement source block (insert name typ scope) typeScope expectedType) cases
            defType <- case mbDefault of
                Nothing -> return []
                Just d -> (:[]) <$> typecheckStatement source d scope typeScope expectedType
            let types = caseTypes++defType
            return $ listToLinkedUnionTypeList types
        Parser.ExecutionStatement _ expr -> do
            typeOf source scope typeScope expr
            return NoType
        Parser.ForLoopStatement _ maybeInit maybeCond maybeIter block -> do
            case maybeInit of
                Nothing -> return NoType
                Just init -> typecheckStatement source init scope typeScope expectedType
            let init = fromMaybe (Parser.BlockStatement undefined []) maybeInit
            let newScope = case (case init of
                    (Parser.DeclarationStatement _ typeExpr name _) -> do
                        typ <- evalType source typeScope typeExpr
                        return $ insert name typ scope
                    _ -> return scope) of
                            Left s -> error ""
                            Right s -> s
            case maybeCond of
                Nothing -> return NoType
                Just cond -> typecheckExpression source newScope typeScope cond BoolType >> return NoType
            case maybeIter of
                Nothing -> return NoType
                Just iter -> typecheckStatement source iter newScope typeScope expectedType
            typecheckStatement source block newScope typeScope expectedType
        Parser.WhileLoopStatement _ cond block -> do
            typecheckExpression source scope typeScope cond BoolType
            typecheckStatement source block scope typeScope expectedType
        Parser.ReturnStatement _ expr -> typecheckExpression source scope typeScope expr expectedType >> return expectedType
        Parser.BreakStatement _ -> return NoType
        _ -> error ""
    where
        isReturn (Parser.ReturnStatement _ _) = True
        isReturn _ = False

combineTypes t1 t2
    | intersect union1 union2 == union1 = t2
    | intersect union2 union1 == union2 = t1
    | otherwise = UnionType t1 t2
    where
        union1 = linkedUnionTypeListToList t1
        union2 = linkedUnionTypeListToList t2

typeOf :: String -> Scope -> TypeScope -> Parser.Expression -> Either String Type
typeOf source scope typeScope expr = case expr of
    Parser.NullLiteral _ -> Right VoidType
    Parser.IdentLiteral pos name ->
        if member name scope then
            Right $ scope ! name
        else
            if member name typeScope then
                Right TypeType
            else
                Left $ nameErr source pos name
    Parser.IdentDeclarationLiteral _ typeExpr _ -> do
        evalType source typeScope typeExpr
        return VoidType
    Parser.BoolLiteral _ _ -> Right BoolType
    Parser.IntLiteral _ _ -> Right IntType
    Parser.FloatLiteral _ _ -> Right FloatType
    Parser.CharLiteral _ _ -> Right CharType
    Parser.StringLiteral _ _ -> Right $ ArrayType CharType
    Parser.IndexLiteral pos coll idx -> do
        case typeOf source scope typeScope coll of
            Left e -> Left e
            Right (ArrayType t) ->          typecheckExpression source scope typeScope idx IntType >> return t
            Right (TupleType left right) -> typecheckExpression source scope typeScope idx IntType >> return (combineTypes left right)
            Right t -> Left $ stringErr source pos $ "TypeError: cannot take index of " ++ show t
    Parser.ArrayLiteral _ typeExpr vals -> do
        typ <- evalType source typeScope typeExpr
        let (ArrayType innerType) = typ
        mapM_ (\val -> typecheckExpression source scope typeScope val innerType) vals
        return $ ArrayType innerType
    Parser.TernaryLiteral _ cond cons alt -> do
        typecheckExpression source scope typeScope cond BoolType
        expectedType <- typeOf source scope typeScope cons
        typecheckExpression source scope typeScope alt expectedType
        return expectedType
    -- Parser.CastLiteral pos typeExpr expr -> do
    --     newType <- evalType source typeScope typeExpr
    --     oldType <- typeOf source scope typeScope expr
    --     if combineTypes oldType newType == oldType then return newType else Left $ stringErr source pos "TypeError: cannot cast from " ++ show oldType ++ " to " ++ show newType
    Parser.StructTypeLiteral _ fields -> mapM_ (\(typeExpr, _)->evalType source typeScope typeExpr) fields >> return TypeType
    Parser.StructLiteral pos typeExpr fields -> do
        typ <- evalType source typeScope typeExpr
        newFields <- mapM (\(name, val)->typeOf source scope typeScope val >>= \fieldType -> return (fieldType, name)) fields
        let newType = StructType newFields
        case typ of
            StructType typeFields -> if typ == newType then return typ else Left $ typeErr source pos typ newType
            _ -> Left $ typeErr source pos typ newType
    Parser.DotLiteral _ expr fieldName -> typeOf source scope typeScope expr >>= \(StructType fields) -> return $ fromList (map (\(a, b)->(b, a)) fields) ! fieldName
    Parser.InterfaceLiteral _ fields -> mapM_ (\(typeExpr, _)->evalType source typeScope typeExpr) fields >> return TypeType
    Parser.TupleLiteral _ vals -> fillTupleType source scope typeScope vals
    Parser.FunctionCallLiteral pos func args -> do
        funcType <- typeOf source scope typeScope func
        case funcType of
            FunctionType argsTuple returnType -> typecheckExpression source scope typeScope args argsTuple >> return returnType
            _ -> Left $ stringErr source pos "TypeError: non-function called like function."
    Parser.InfixLiteral _ op left right -> typeOfOperation source scope typeScope left op right
    Parser.PrefixLiteral pos op right -> do
        rtype <- typeOf source scope typeScope right
        case op of
            "!" -> case rtype of
                BoolType -> Right BoolType
                _ -> Left $ typeErr source pos BoolType rtype
            "-" -> case rtype of
                IntType -> Right IntType
                FloatType -> Right FloatType
                _ -> Left $ stringErr source pos "TypeError: invalid prefix operation " ++ op ++ " " ++ show rtype
            "[]" -> case rtype of
                TypeType -> Right TypeType
                _ -> Left $ typeErr source pos TypeType rtype
            _ -> Left $ stringErr source pos "NameError: unknown prefix operator " ++ op

fillTupleType :: String -> Scope -> TypeScope -> [Parser.Expression] -> Either String Type
fillTupleType source scope typeScope = f where
    f [x] = typeOf source scope typeScope x
    f (x:xs) = do
        left <- typeOf source scope typeScope x
        right <- fillTupleType source scope typeScope xs
        return $ TupleType left right
    f [] = Right VoidType -- "()", *:Unit ~= null:Void

checkEq ltype op rtype
    | op `notElem` ["==", "!="] || ltype /= rtype =
        Right ()
    | otherwise =
        Left BoolType

checkArith ltype op rtype
    | op `notElem` ["+", "-", "/", "*", "^", ">", "<", "<=", ">="] || (ltype /= IntType && ltype /= FloatType) || (rtype /= IntType && ltype /= FloatType) =
        Right ()
    | op `elem` [">", "<", "<=", ">="] =
        Left BoolType
    | (ltype == IntType) && (rtype == IntType) =
        Left IntType
    | otherwise =
        Left FloatType

checkTypeOp ltype op rtype
    | op `notElem` ["+", "*", "->", "<="] || ltype /= TypeType || rtype /= TypeType =
        Right ()
    | op == "<=" =
        Left BoolType
    | otherwise =
        Left TypeType

checkConcat ltype op rtype
    | op /= "+" || isntArray ltype || isntArray rtype || ltype /= rtype =
        Right ()
    | otherwise =
        Left ltype
    where
        isntArray (ArrayType _) = False
        isntArray _ = True

typeOfOperation source scope typeScope left op right = do
    ltype <- typeOf source scope typeScope left
    rtype <- typeOf source scope typeScope right
    let attempts = (do
            checkEq ltype op rtype
            checkArith ltype op rtype
            checkTypeOp ltype op rtype
            checkConcat ltype op rtype)
    case attempts of
        Right () -> Left $ stringErr source (Parser.posOf right) $ "TypeError: invalid operation "++ show ltype ++ " " ++ op ++ " " ++ show rtype
        Left t -> Right t

typecheckExpression :: String -> Scope -> TypeScope -> Parser.Expression -> Type -> Either String ()
typecheckExpression source scope typeScope expr expectedType = do
    typ <- typeOf source scope typeScope expr
    if combineTypes expectedType typ == expectedType then
        Right ()
    else
        Left $ typeErr source (Parser.posOf expr) expectedType typ

evalType :: String -> TypeScope -> Parser.Expression -> Either String Type
evalType source scope expr = case expr of
    Parser.IdentLiteral pos name ->
        if member name scope then
            Right $ scope ! name
        else
            Left $ nameErr source pos name
    Parser.InfixLiteral pos op left right -> do
        leftType <- evalType source scope left
        rightType <- evalType source scope right
        case op of
            "+" -> Right $ UnionType leftType rightType
            "*" -> Right $ TupleType leftType rightType
            _ -> Left $ stringErr source pos "TypeError: operation does not return a type"
    Parser.PrefixLiteral pos op right -> do
        rightType <- evalType source scope right
        case op of
            "[]" -> Right $ ArrayType rightType
            _ -> Left $ stringErr source pos "TypeError: operation does not return a type"
    Parser.StructTypeLiteral _ fields ->
        StructType <$> mapM (\(typeExpr, name)->evalType source scope typeExpr >>= \typ -> return (typ, name)) fields
    Parser.InterfaceLiteral _ fields ->
        InterfaceType <$> mapM (\(typeExpr, name)->evalType source scope typeExpr >>= \typ -> return (typ, name)) fields
    _ -> Left $ stringErr source (Parser.posOf expr) "TypeError: not a type." -- TODO better errors later
evalType_ :: String -> TypeScope -> Parser.Expression -> Either String ()
evalType_ source scope expr = case evalType source scope expr of
    Left err -> Left err
    Right _ -> Right ()
