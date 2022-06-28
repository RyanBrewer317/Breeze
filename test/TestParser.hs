module TestParser ( tests ) where

import Distribution.TestSuite.QuickCheck
import Parser

tests :: IO [ Test ]
tests = return [ testProperty "Parser.nullLiteral parses null" doesParseNull
               , testProperty "Parser.boolLiteral parses true and false" doesParseBool
               , testProperty "Parser.identLiteral parses identifiers" doesParseIdent
               , testProperty "Parser.intLiteral parses integers" doesParseInt
               , testProperty "Parser.floatLiteral parses floats" doesParseFloat
               , testProperty "Parser.charLiteral parses characters" doesParseChar
               , testProperty "Parser.stringLiteral parses strings" doesParseString
               , testProperty "Parser.expression parses arrays" doesParseArray
               , testProperty "Parser.tupleLiteral parses tuples" doesParseTuple
               , testProperty "Parser.expression parses ternary operators" doesParseTernary
               , testProperty "Parser.expression parses array and tuple indices" doesParseIndex
               ]

testParse :: (Eq a) => Parser Expression -> [(String, a)] -> (Expression -> a) -> Bool
testParse parser cases converter = all ((==True) . f) cases where
    f (text, value) = case p text value of Left pe -> False; Right b -> b
    p text value = do
        e <- run parser text
        let val = converter e
        return $ value == val

getNull (NullLiteral _) = Just ()
getNull _ = Nothing

getBool (BoolLiteral _ b) = Just b
getBool _ = Nothing

getIdent (IdentLiteral _ name) = Just name
getIdent _ = Nothing

getInt (IntLiteral _ i) = Just i
getInt _ = Nothing

getFloat (FloatLiteral _ f) = Just f
getFloat _ = Nothing

getChar (CharLiteral _ c) = Just c
getChar _ = Nothing

getString (StringLiteral _ s) = Just s
getString _ = Nothing

getArray (ArrayLiteral _ typ items) = Just (typ, items)
getArray _ = Nothing

getTuple (TupleLiteral _ items) = Just items
getTuple _ = Nothing

getTernary (TernaryLiteral _ cond cons alt) = Just (cond, cons, alt)
getTernary _ = Nothing

getIndex (IndexLiteral _ _ idx) = Just idx
getIndex _ = Nothing


doesParseNull = testParse nullLiteral [("null", Just ())] getNull

doesParseBool = testParse boolLiteral [("true", Just True), ("false", Just False)] getBool

doesParseIdent = testParse identLiteral [("f", Just "f"), ("foo", Just "foo"), ("foo_bar", Just "foo_bar"), ("foo_", Just "foo_"), ("f1", Just "f1"), ("f_1", Just "f_1")] getIdent

doesParseInt = testParse intLiteral [("1", Just 1), ("069", Just 69)] getInt

doesParseFloat = testParse floatLiteral [("0.1", Just 0.1), ("069.420", Just 69.42)] getFloat

doesParseChar = testParse charLiteral [("'c'", Just 'c'), ("'\\\\'", Just '\\'), ("'\\n'", Just '\n')] TestParser.getChar

doesParseString = testParse stringLiteral [("\"\"", Just ""), ("\"\\\\\"", Just "\\"), ("\"Breeze is cool\"", Just "Breeze is cool")] getString

doesParseArray = testParse expression [("Int{1, 2}", Just (Just "Int", [Just 1, Just 2]))] $ \a -> f (getArray a) where
    f :: Maybe (Expression, [Expression]) -> Maybe (Maybe String, [Maybe Int])
    f (Just (typ, ints)) = Just (getIdent typ, map getInt ints)
    f Nothing = Nothing

doesParseTuple = testParse tupleLiteral [("('c', true,7)", Just (Just 'c', Just True, Just 7))] $ \a -> f (getTuple a) where
    f :: Maybe [Expression] -> Maybe (Maybe Char, Maybe Bool, Maybe Int)
    f (Just [a, b, c]) = Just (TestParser.getChar a, getBool b, getInt c)
    f _ = Nothing

doesParseTernary = testParse expression [("a ? b : c", Just (Just "a", Just "b", Just "c"))] $ \a -> f (getTernary a) where
    f :: Maybe (Expression, Expression, Expression) -> Maybe (Maybe String, Maybe String, Maybe String)
    f (Just (a, b, c)) = Just (getIdent a, getIdent b, getIdent c)
    f _ = Nothing

doesParseIndex = testParse expression [("Int{1, 2}[1]", Just 2), ("(3, true)[0]", Just 3)] $ \a -> f (getIndex a) where
    f :: Maybe Expression -> Maybe Int
    f (Just a) = getInt a
    f _ = Nothing