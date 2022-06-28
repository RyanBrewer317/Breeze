module TypeAnnotations where

import qualified Parser
import qualified TypeChecker as TC
import Data.Map ((!), insert, union, fromList, member, mergeWithKey, intersection)
import Data.List (partition, sort, nub)
import Data.Bifunctor ( Bifunctor(bimap, first) )

data AnnotatedExpression = Primitive Parser.Expression
                         | Ident Parser.Expression String TC.Type
                         | Array Parser.Expression TC.Type [AnnotatedExpression]
                         | Tuple Parser.Expression [AnnotatedExpression]
                         | Ternary Parser.Expression TC.Type AnnotatedExpression AnnotatedExpression AnnotatedExpression
                         | Call Parser.Expression TC.Type AnnotatedExpression AnnotatedExpression
                         | Index Parser.Expression TC.Type AnnotatedExpression AnnotatedExpression
                         | Infix Parser.Expression TC.Type String AnnotatedExpression AnnotatedExpression
                         | Prefix Parser.Expression TC.Type String AnnotatedExpression
                         | StructType Parser.Expression [(AnnotatedExpression, String)]
                         | Struct Parser.Expression TC.Type [(String, AnnotatedExpression)]
                         | Dot Parser.Expression TC.Type AnnotatedExpression String
                         | InterfaceType Parser.Expression [(AnnotatedExpression, String)]
                         deriving Show

data AnnotatedStatement = Assignment Parser.Statement String AnnotatedExpression
                        | DotAssignment Parser.Statement AnnotatedExpression String AnnotatedExpression
                        | IndexAssignment Parser.Statement AnnotatedExpression AnnotatedExpression AnnotatedExpression
                        | FunctionDec Parser.Statement String [AnnotatedExpression] AnnotatedStatement
                        | Block Parser.Statement [AnnotatedStatement]
                        | If Parser.Statement AnnotatedExpression AnnotatedStatement
                        | IfElse Parser.Statement AnnotatedExpression AnnotatedStatement AnnotatedStatement
                        | Switch Parser.Statement AnnotatedExpression [(AnnotatedExpression, AnnotatedStatement)] (Maybe AnnotatedStatement)
                        | TypeSwitch Parser.Statement AnnotatedExpression [(AnnotatedExpression, AnnotatedStatement)] (Maybe AnnotatedStatement)
                        | For Parser.Statement (Maybe AnnotatedStatement) (Maybe AnnotatedExpression) (Maybe AnnotatedStatement) AnnotatedStatement
                        | While Parser.Statement AnnotatedExpression AnnotatedStatement
                        | Execution Parser.Statement AnnotatedExpression
                        | Return Parser.Statement AnnotatedExpression
                        | Using Parser.Statement [AnnotatedExpression]
                        | Break Parser.Statement
                        deriving Show

annotate :: [Parser.Statement] -> ([AnnotatedStatement], [TC.Type])
annotate ast =
    let (annStmts, types) = annotateStatements ast TC.startingScope TC.startingTypeScope in
        (annStmts, nub types)

annotateStatements :: [Parser.Statement] -> TC.Scope -> TC.TypeScope -> ([AnnotatedStatement], [TC.Type])
annotateStatements stmts scope typeScope
    | not $ null stmts =
        let stmt = head stmts in
        let rest = tail stmts in
        let (annotated, types) = annotateStatement scope typeScope stmt in
            case stmt of
                Parser.DeclarationStatement _ typ name val ->
                    let (annRest, typesRest) = annotateStatements rest (insert name (evalType typeScope typ) scope) typeScope in
                        (annotated:annRest, types++typesRest)
                Parser.TypeDeclaration _ name val ->
                    let (annRest, typesRest) = annotateStatements rest scope (insert name (evalType typeScope val) typeScope) in
                        (annotated:annRest, types++typesRest)
                Parser.FunctionStatement _ retType name args block ->
                    let (annRest, typesRest) = annotateStatements rest (insert name (TC.FunctionType (argsToTupleType typeScope args) (evalType typeScope retType)) scope) typeScope in
                        (annotated:annRest, types++typesRest)
                _ ->
                    let (annRest, typesRest) = annotateStatements rest scope typeScope in
                        (annotated:annRest, types++typesRest)
    | otherwise = ([], [])

annotateBlock :: TC.Scope -> TC.TypeScope -> TC.Scope -> TC.TypeScope -> [Parser.Statement] -> ([AnnotatedStatement], [TC.Type])
annotateBlock scope typeScope insertedScope insertedTypeScope stmts = case stmts of
    [] -> ([], [])
    first:rest -> case first of
        (Parser.UsingStatement _ idents) ->
            let (varIdents, typeIdents) = partition (`member` scope) $ map (\(Parser.IdentLiteral _ name)->name) idents in
            let newScope = insertedScope `union` fromList (map (\name->(name, scope ! name)) varIdents) in
            let newTypeScope = insertedTypeScope `union` fromList (map (\name->(name, typeScope ! name)) typeIdents) in
                annotateStatements rest newScope newTypeScope
        _ -> annotateStatements stmts insertedScope insertedTypeScope

annotateStatement :: TC.Scope -> TC.TypeScope -> Parser.Statement -> (AnnotatedStatement, [TC.Type])
annotateStatement scope typeScope stmt = case stmt of
    Parser.DeclarationStatement _ _ name val ->
        let (expr, types) = annotateExpression scope typeScope val in
            (Assignment stmt name expr, types)
    Parser.TypeDeclaration _ name val ->
        let (expr, types) = annotateExpression scope typeScope val in
            (Assignment stmt name expr, types)
    Parser.AssignmentStatement _ name val ->
        let (expr, types) = annotateExpression scope typeScope val in
            (Assignment stmt name expr, types)
    Parser.DotAssignmentStatement _ struct field val ->
        let (annStruct, structTypes) = annotateExpression scope typeScope struct in
        let (expr, types) = annotateExpression scope typeScope val in
            (DotAssignment stmt annStruct field expr, structTypes++types)
    Parser.IndexAssignmentStatement _ array index value -> 
        let (annArray, arrayTypes) = annotateExpression scope typeScope array in
        let (annIndex, indexTypes) = annotateExpression scope typeScope index in
        let (annValue, valueTypes) = annotateExpression scope typeScope value in
            (IndexAssignment stmt annArray annIndex annValue, arrayTypes++indexTypes++valueTypes)
    Parser.FunctionStatement _ retType name args block ->
        let tns = map (\(Parser.IdentDeclarationLiteral _ typeExpr name)->(name, evalType typeScope typeExpr)) args in
        let argScope = fromList tns in
        let innerScope = insert name (TC.FunctionType (argsToTupleType typeScope args) (evalType typeScope retType)) argScope in
        case block of
            Parser.BlockStatement _ stmts ->
                let (annArgs, argsTypesTall) = unzip $ map (annotateExpression scope typeScope) args in
                let argsTypes = concat argsTypesTall in
                let (annStmts, blockTypes) = annotateBlock scope typeScope (innerScope `union` TC.startingScope) TC.startingTypeScope stmts in
                    (FunctionDec stmt name annArgs (Block block annStmts), argsTypes++blockTypes)
            _ -> error ""
    Parser.BlockStatement _ stmts ->
        let (annStmts, blockTypes) = annotateBlock scope typeScope TC.startingScope TC.startingTypeScope stmts in
            (Block stmt annStmts, blockTypes)
    Parser.IfStatement _ cond cons mbAlt ->
        let (annCond, condTypes) = annotateExpression scope typeScope cond in
        let (annCons, consTypes) = annotateStatement scope typeScope cons in
        case mbAlt of
            Nothing -> (If stmt annCond annCons, condTypes++consTypes)
            Just alt ->
                let (annAlt, altTypes) = annotateStatement scope typeScope alt in
                    (IfElse stmt annCond annCons annAlt, condTypes++consTypes++altTypes)
    Parser.SwitchStatement _ expr cases mbDefault ->
        let (annExpr, exprTypes) = annotateExpression scope typeScope expr in
        let (annCases, casesTypesTall) = unzip $ map (\(cas, block) -> let (annVal, valTypes) = annotateExpression scope typeScope cas in let (annBlock, blockTypes) = annotateStatement scope typeScope block in ((annVal, annBlock), valTypes++blockTypes)) cases in
        let (annMaybeDef, defTypes) = maybe (Nothing, []) (Data.Bifunctor.first Just) $ mbDefault >>= \def -> return $ annotateStatement scope typeScope def in
            (Switch stmt annExpr annCases annMaybeDef, exprTypes++concat casesTypesTall++defTypes)
    Parser.TypeSwitchStatement _ ident cases mbDefault ->
        let (annIdent, identTypes) = annotateExpression scope typeScope ident in
        let (Parser.IdentLiteral _ name) = ident in
        let (annCases, casesTypesTall) = unzip $ map (\(cas, block) -> let (annVal, valTypes) = annotateExpression scope typeScope cas in let (annBlock, blockTypes) = annotateStatement (insert name (evalType typeScope cas) scope) typeScope block in ((annVal, annBlock), valTypes++blockTypes)) cases in
        let (annMaybeDef, defTypes) = maybe (Nothing, []) (Data.Bifunctor.first Just) $ mbDefault >>= \def -> return $ annotateStatement scope typeScope def in
            (TypeSwitch stmt annIdent annCases annMaybeDef, identTypes++concat casesTypesTall++defTypes)
    Parser.ForLoopStatement _ mbInit mbCond mbIter block ->
        let (annMaybeInit, initTypes) = maybe (Nothing, []) (Data.Bifunctor.first Just) $ mbInit >>= \init -> return $ annotateStatement scope typeScope init in
        let forScope = (case annMaybeInit of
                            Just (Assignment (Parser.DeclarationStatement _ typeExpr _ _) name val) -> insert name (evalType typeScope typeExpr) scope
                            Just (Assignment _ name val) -> insert name (typeOf val) scope
                            _ -> scope) in
        let (annMaybeCond, condTypes) = maybe (Nothing, []) (Data.Bifunctor.first Just) $ mbCond >>= \cond -> return $ annotateExpression forScope typeScope cond in
        let (annMaybeIter, iterTypes) = maybe (Nothing, []) (Data.Bifunctor.first Just) $ mbIter >>= \iter -> return $ annotateStatement forScope typeScope iter in
        let (annBlock, blockTypes) = annotateStatement forScope typeScope block in
            (For stmt annMaybeInit annMaybeCond annMaybeIter annBlock, initTypes++condTypes++iterTypes++blockTypes)
    Parser.WhileLoopStatement _ cond block ->
        let (annCond, condTypes) = annotateExpression scope typeScope cond in
        let (annBlock, blockTypes) = annotateStatement scope typeScope block in
            (While stmt annCond annBlock, condTypes++blockTypes)
    Parser.ExecutionStatement _ expr ->
        let (annExpr, exprTypes) = annotateExpression scope typeScope expr in
            (Execution stmt annExpr, exprTypes)
    Parser.ReturnStatement _ expr ->
        let (annExpr, exprTypes) = annotateExpression scope typeScope expr in
            (Return stmt annExpr, exprTypes)
    Parser.BreakStatement _ -> (Break stmt, [])
    _ -> error ""

annotateExpression :: TC.Scope -> TC.TypeScope -> Parser.Expression -> (AnnotatedExpression, [TC.Type])
annotateExpression scope typeScope expr = case expr of
    Parser.NullLiteral _ ->
        let annExpr = Primitive expr in
            (annExpr, [typeOf annExpr])
    Parser.IdentLiteral _ name ->
        if name `member` scope then
            let typ = scope ! name in
            (Ident expr name typ, [typ])
        else
            (Ident expr name TC.TypeType, [TC.TypeType])
    Parser.IdentDeclarationLiteral _ typeExpr name ->
        let typ = evalType typeScope typeExpr in
            (Ident expr name typ, [typ])
    Parser.BoolLiteral _ _ ->
        let annExpr = Primitive expr in
            (annExpr, [typeOf annExpr])
    Parser.IntLiteral _ _ ->
        let annExpr = Primitive expr in
            (annExpr, [typeOf annExpr])
    Parser.FloatLiteral _ _ ->
        let annExpr = Primitive expr in
            (annExpr, [typeOf annExpr])
    Parser.CharLiteral _ _ ->
        let annExpr = Primitive expr in
            (annExpr, [typeOf annExpr])
    Parser.StringLiteral pos s ->
        let (annExprs, exprTypesTall) = unzip $ map (annotateExpression scope typeScope . Parser.CharLiteral pos) s in
            (Array expr (TC.ArrayType TC.CharType) annExprs, TC.ArrayType TC.CharType:concat exprTypesTall)
    Parser.ArrayLiteral _ typeExpr exprs ->
        let (annExprs, exprTypesTall) = unzip $ map (annotateExpression scope typeScope) exprs in
            let typ = evalType typeScope typeExpr in
                (Array expr typ annExprs, typ:concat exprTypesTall)
    Parser.TernaryLiteral _ cond cons alt ->
        let (annCond, condTypes) = annotateExpression scope typeScope cond in
        let (annCons, consTypes) = annotateExpression scope typeScope cons in
        let (annAlt, altTypes) = annotateExpression scope typeScope alt in
        let fullType = TC.combineTypes (typeOf annCons) (typeOf annAlt) in
            (Ternary expr fullType annCond annCons annAlt, fullType:condTypes++consTypes++altTypes)
    Parser.TupleLiteral _ exprs ->
        let (annExprs, exprTypesTall) = unzip $ map (annotateExpression scope typeScope) exprs in
            (Tuple expr annExprs, fillTupleType annExprs : concat exprTypesTall)
    Parser.IndexLiteral _ coll idx ->
        let (annColl, collTypes) = annotateExpression scope typeScope coll in
        let (annIndex, indexTypes) = annotateExpression scope typeScope idx in
        let fullType = (case typeOf annColl of
                TC.ArrayType typ -> typ
                TC.TupleType _ _ -> (listToLinkedUnionTypeList . linkedTupleTypeListToList . typeOf) annColl
                _ -> undefined) in
            (Index expr fullType annColl annIndex, fullType:collTypes++indexTypes)
    Parser.FunctionCallLiteral _ func args ->
        let (annFunc, funcTypes) = annotateExpression scope typeScope func in
        let (annArgs, argsTypes) = annotateExpression scope typeScope args in
        let (TC.FunctionType _ retType) = typeOf annFunc in
            (Call expr retType annFunc annArgs, retType : funcTypes++argsTypes)
    Parser.InfixLiteral _ op left right ->
        let (annLeft, leftTypes) = annotateExpression scope typeScope left in
        let (annRight, rightTypes) = annotateExpression scope typeScope right in
        let typ = infixType op (typeOf annLeft) (typeOf annRight) in
            (Infix expr typ op annLeft annRight, typ:leftTypes++rightTypes)
    Parser.PrefixLiteral _ op right ->
        let (annRight, rightTypes) = annotateExpression scope typeScope right in
        let typ = prefixType op (typeOf annRight) in
            (Prefix expr typ op annRight, typ : rightTypes)
    Parser.StructTypeLiteral _ fields -> 
        let typeExprs = map fst fields in
        let (annTypes, typesTall) = unzip $ map (annotateExpression scope typeScope) typeExprs in
        let names = map snd fields in
            (StructType expr (zip annTypes names), TC.StructType (zip (map (evalType typeScope) typeExprs) names) : concat typesTall)
    Parser.StructLiteral _ typeExpr fields ->
        let (_, typeTypes) = annotateExpression scope typeScope typeExpr in
        let (annVals, valTypesTall) = unzip $ map (annotateExpression scope typeScope . snd) fields in
        let names = map fst fields in
            (Struct expr (evalType typeScope typeExpr) (zip names annVals), typeTypes++concat valTypesTall)
    Parser.DotLiteral _ expr fieldName ->
        let (annExpr, exprTypes) = annotateExpression scope typeScope expr in
        let (TC.StructType fields) = typeOf annExpr in
        let typ = fromList (map (\(a, b)->(b, a)) fields) ! fieldName in
            (Dot expr typ annExpr fieldName, exprTypes)
    Parser.InterfaceLiteral _ fields ->
        let typeExprs = map fst fields in
        let (annTypes, typesTall) = unzip $ map (annotateExpression scope typeScope) typeExprs in
        let names = map snd fields in
            (InterfaceType expr (zip annTypes names), TC.InterfaceType (zip (map (evalType typeScope) typeExprs) names) : concat typesTall)
    -- _ -> error $ show expr

typeOf :: AnnotatedExpression -> TC.Type
typeOf expr = case expr of
    Primitive expr -> case expr of
        Parser.NullLiteral _ -> TC.VoidType
        Parser.BoolLiteral _ _ -> TC.BoolType
        Parser.IntLiteral _ _ -> TC.IntType
        Parser.FloatLiteral _ _ -> TC.FloatType
        Parser.CharLiteral _ _ -> TC.CharType
        Parser.StringLiteral _ _ -> TC.ArrayType TC.CharType
        _ -> undefined
    Ident _ _ typ -> typ
    Array _ typ _ -> TC.ArrayType typ
    Tuple _ exprs -> fillTupleType exprs
    Ternary _ typ _ _ _ -> typ
    Call _ typ _ _ -> typ
    Index _ typ _ _ -> typ
    -- Index _ coll idx -> case typeOf coll of
    --     TC.ArrayType typ -> typ
    --     TC.TupleType _ _ -> (listToLinkedUnionTypeList . linkedTupleTypeListToList . typeOf) coll
    --     _ -> undefined
    Infix _ typ _ _ _ -> typ
    Prefix _ typ _ _ -> typ
    StructType _ _ -> TC.TypeType
    InterfaceType _ _ -> TC.TypeType
    Struct _ typ _ -> typ
    Dot _ typ _ _ -> typ

fillTupleType exprs = case exprs of
    [] -> undefined
    [x] -> typeOf x
    [x, x2] -> TC.TupleType (typeOf x) (typeOf x2)
    (x:xs) -> TC.TupleType (typeOf x) $ fillTupleType xs

linkedTupleTypeListToList typ = case typ of
    TC.TupleType left right -> left : linkedTupleTypeListToList right
    _ -> [typ]

listToLinkedUnionTypeList types = case types of
    [] -> undefined
    [x, x2] -> TC.UnionType x x2
    (x:xs) -> TC.UnionType x $ listToLinkedUnionTypeList xs

infixType op left right = case op of
    "+" -> case left of
        TC.IntType -> right
        TC.FloatType -> left
        TC.ArrayType _ -> left
        TC.TypeType -> left
        _ -> error $ show left
    "-" -> case left of
        TC.IntType -> right
        TC.FloatType -> left
        _ -> undefined
    "*" -> case left of
        TC.IntType -> right
        TC.FloatType -> left
        TC.TypeType -> left
        _ -> undefined
    "/" -> case left of
        TC.IntType -> right
        TC.FloatType -> left
        _ -> undefined
    "==" -> TC.BoolType
    "<=" -> TC.BoolType
    ">=" -> TC.BoolType
    "<" -> TC.BoolType
    ">" -> TC.BoolType
    "&&" -> TC.BoolType
    "||" -> TC.BoolType
    "=>" -> TC.BoolType
    _ -> undefined

prefixType op right = case op of
    "!" -> TC.BoolType
    "-" -> right
    _ -> undefined

evalType scope expr = case expr of
    Parser.IdentLiteral _ name -> scope ! name
    Parser.InfixLiteral _ op left right ->
        let leftType = evalType scope left in
            let rightType = evalType scope right in
                case op of
                    "+" -> TC.UnionType leftType rightType
                    "*" -> TC.TupleType leftType rightType
                    "->" -> TC.FunctionType leftType rightType
                    _ -> error ""
    Parser.PrefixLiteral _ op right ->
        let rightType = evalType scope right in
            case op of
                "[]" -> TC.ArrayType rightType
                _ -> error ""
    Parser.StructTypeLiteral _ fields ->
        TC.StructType $ map (first (evalType scope)) fields
    Parser.InterfaceLiteral _ fields ->
        TC.InterfaceType $ map (first (evalType scope)) fields
    _ -> error ""

argsToTupleType :: TC.TypeScope -> [Parser.Expression] -> TC.Type
argsToTupleType typeScope = f where
    f [] = TC.VoidType
    f [Parser.IdentDeclarationLiteral _ typ _] = evalType typeScope typ
    f ((Parser.IdentDeclarationLiteral _ typ _):xs) = TC.TupleType (evalType typeScope typ) (argsToTupleType typeScope xs)
    f (_:_) = error ""
