module PLexer where

import      Data.Char

type VariableName = String
data Token = TVariable VariableName
            | TConjuction
            | TConditional
            | TEquality
            | TInequalty
            | TDisjunction
            | TOpenParenthesis
            | TClosingParenthesis
            | TNegation
            | EOI deriving (Show, Eq)
type TokenSequence = [Token]

tokenize :: String -> TokenSequence
tokenize [] = [EOI]
tokenize (' ':xs) = tokenize xs
tokenize ('(':xs) = TOpenParenthesis:tokenize xs
tokenize (')':xs) = TClosingParenthesis:tokenize xs
tokenize ('v':xs) = TDisjunction:tokenize xs
tokenize ('^':xs) = TConjuction:tokenize xs
tokenize ('=':xs) = TEquality:tokenize xs
tokenize ('!':y:xs) = if y == '=' then
                        TInequalty : tokenize xs
                      else
                        TNegation : tokenize (y:xs)
tokenize ('-':y:xs) = if y == '>' then
                        TConditional : tokenize xs
                      else
                        error ("Expected '>' but got " ++ show y)
tokenize str@(x:xs)
  | isUpper x = let (xs', vname) = readVariableName (str, "") in TVariable vname : tokenize xs'
  | otherwise = error ("Unexpected Character encountered " ++ show x ++ " (Hint: variable names should start with an upper case letter).")

readVariableName :: (String,  VariableName) -> (String, VariableName)
readVariableName ("", variableName) = ("", variableName)
readVariableName result@(x:xs, variableName)
  | isLetter x = readVariableName (xs, variableName ++ [x])
  | otherwise = result
