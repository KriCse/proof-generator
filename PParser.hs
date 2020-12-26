module PParser where

import PLexer (Token(..), TokenSequence, VariableName)

data BinaryOperator = Conjuction
                    | Disjunction
                    | Conditional
                    | Equality
                    | Inequalty deriving (Eq)

instance Show BinaryOperator where
  show Conjuction = "^"
  show Disjunction = "v"
  show Conditional = "->"
  show Equality = "="
  show Inequalty = "!="


data UnaryOperator = Negation deriving (Eq)
instance Show UnaryOperator where
  show Negation = "!"

data Expression  = AtomicFormula VariableName |
                  BinaryExpression BinaryOperator Expression Expression |
                  UnaryExpression UnaryOperator Expression deriving (Eq)

instance Show Expression where
  show (AtomicFormula name) = name
  show (BinaryExpression o l r) = show l ++ show o ++ show r
  show (UnaryExpression o e) = show o ++ show e

type PartialParseResult = (TokenSequence, Expression)
type Parser = TokenSequence -> PartialParseResult

parseEquality :: Parser
parseEquality [EOI] = error "Unexpected End of Input"
parseEquality ts
  | follow == TEquality || follow == TInequalty = (nextTs, BinaryExpression operator left right)
  | otherwise = (ts', left)
  where
    ts' :: TokenSequence
    left :: Expression
    follow :: Token
    (ts'@(follow : _), left) = parseTerm ts
    nextTs :: TokenSequence
    operator :: BinaryOperator
    right :: Expression
    (nextTs, operator, right) = parseEquality' ts'

getBinaryEqualityOperator :: Token -> BinaryOperator
getBinaryEqualityOperator TEquality = Equality
getBinaryEqualityOperator TInequalty = Inequalty
getBinaryEqualityOperator t = error ("Expected Equality operator but got " ++ show t)

parseEquality' :: TokenSequence -> (TokenSequence, BinaryOperator, Expression)
parseEquality' (operatorToken : ts)
  | operatorToken' == TEquality || operatorToken' == TInequalty = (nextTs, operator, BinaryExpression operator' expression rightExpression)
  | otherwise = (ts', operator, expression)
  where
    operator :: BinaryOperator
    operator = getBinaryEqualityOperator operatorToken
    operatorToken' :: Token
    ts' :: TokenSequence
    expression :: Expression
    nextTs :: TokenSequence
    (ts'@(operatorToken':_), expression) = parseTerm ts
    rightExpression :: Expression
    operator' :: BinaryOperator
    (nextTs, operator', rightExpression) = parseEquality' ts'

parseEquality' _ = error "Unexpected Token"

parseBinaryOperator' :: Token -> BinaryOperator -> Parser -> TokenSequence -> (TokenSequence, BinaryOperator, Expression)
parseBinaryOperator' operatorToken binaryOperator nextParser (operator : ts)
  | operator /= operatorToken = error ("Unexpected Token: Expected " ++ show operatorToken ++ " but got " ++  show operator)
  | operatorToken == nextOperator = (nextTs, binaryOperator, BinaryExpression binaryOperator' rightExpression nextExpression)
  | otherwise = (ts', binaryOperator, rightExpression)
  where
    rightExpression :: Expression
    nextOperator :: Token
    ts' :: TokenSequence
    (ts'@(nextOperator:_), rightExpression) = nextParser ts
    nextTs :: TokenSequence
    binaryOperator' :: BinaryOperator
    nextExpression :: Expression
    (nextTs, binaryOperator', nextExpression) = parseBinaryOperator' operatorToken binaryOperator nextParser ts'

parseBinaryOperator :: Token -> BinaryOperator -> Parser -> TokenSequence -> PartialParseResult
parseBinaryOperator operatorToken binaryOperator nextParser ts
  | operator == operatorToken = (nextTs, BinaryExpression nextBinaryOperator leftExpression rightExpression)
  | otherwise = (ts', leftExpression)
  where
    leftExpression :: Expression
    ts' :: TokenSequence
    (ts'@(operator : _), leftExpression) = nextParser ts
    rightExpression :: Expression
    operator :: Token
    nextBinaryOperator :: BinaryOperator
    nextTs :: TokenSequence
    (nextTs, nextBinaryOperator, rightExpression)  = parseBinaryOperator' operatorToken binaryOperator nextParser ts'

parseConditional :: Parser
parseConditional = parseBinaryOperator TConditional Conditional parseEquality

parseDisjunction :: Parser
parseDisjunction = parseBinaryOperator TDisjunction Disjunction parseConditional

parseConjuction :: Parser
parseConjuction = parseBinaryOperator TConjuction Conjuction parseDisjunction


parseTerm :: Parser
parseTerm (TNegation : xs) = let (ts, expression) = parseTerm xs in
                                  (ts, UnaryExpression Negation expression)
parseTerm (TVariable name : xs) = (xs, AtomicFormula name)
parseTerm (TOpenParenthesis : xs) = parseParenthesis xs
parseTerm (x:_) = error ("Unexpected Token: " ++ show x)

parseParenthesis :: Parser
parseParenthesis ts@(x:xs)
  | x == TClosingParenthesis = error "Empty Expression in Parenthesis"
  | closing == TClosingParenthesis = (ts', expression)
  | otherwise = error "Missing closing Parenthesis"
  where
    ts' :: TokenSequence
    closing :: Token
    expression :: Expression
    (closing:ts', expression) = parseConjuction ts

parseExpression :: TokenSequence -> Expression
parseExpression ts
  | ts' == [EOI] = expression
  | otherwise = error "Expession not ended correctly"
  where
    ts' :: TokenSequence
    expression :: Expression
    (ts', expression) = parseConjuction ts
