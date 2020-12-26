module ProofTree where
import PParser(BinaryOperator(..), UnaryOperator(..), Expression(..))
import Data.List

type Sign = Bool
type Closed = Bool
data SignedExpression = SignedExpression Sign Expression deriving (Eq)

instance Show SignedExpression where
  show (SignedExpression b e) = show b ++ ": " ++ show e

printRule :: String -> SignedExpression  -> String
printRule rule se  = show se ++ "(" ++ rule ++ ")"

type AlphaExpression = SignedExpression
type BetaExpression = SignedExpression
type SignedAtomicExpression = SignedExpression

data UnexpandedExpressions = UnexpandedExpressions {alphaRules :: [AlphaExpression],
                                                     betaRules :: [BetaExpression]} deriving (Eq)
instance Show UnexpandedExpressions where
  show  UnexpandedExpressions {alphaRules = [], betaRules = []} = "{}"
  show UnexpandedExpressions {alphaRules = a, betaRules = b}
   =  "{" ++ intercalate ", " ([alphaPrinter x | x <- a] ++ [betaPrinter x | x <- b]) ++ "}"
   where
     alphaPrinter = printRule "alpha"
     betaPrinter = printRule "beta"

data ExpandedExpressions = ExpandedExpressions {atomicFormulas :: [SignedAtomicExpression]} deriving (Eq)
instance Show ExpandedExpressions where
  show ExpandedExpressions {atomicFormulas = [] } = "{}"
  show ExpandedExpressions {atomicFormulas = af } = "{" ++ intercalate ", " [show x | x <- af] ++ "}"

data ExpressionSet = ExpressionSet UnexpandedExpressions ExpandedExpressions Closed deriving (Eq)
instance Show ExpressionSet where
  show (ExpressionSet ue ee True) = "To be Expanded: " ++ show ue ++ ". Expanded: " ++ show ee ++ " (closed)"
  show (ExpressionSet ue ee False) = "To be Expanded: " ++ show ue ++ ". Expanded: " ++ show ee

data Rule = Alpha | Beta | Atom  deriving (Show, Eq)

getRule :: SignedExpression -> Rule
getRule (SignedExpression _ (BinaryExpression Equality _ _)) = Beta
getRule (SignedExpression _ (BinaryExpression Inequalty _ _)) = Beta
getRule (SignedExpression True (BinaryExpression Conjuction _ _)) = Alpha
getRule (SignedExpression False (BinaryExpression Conjuction _ _)) = Beta
getRule (SignedExpression True (BinaryExpression Disjunction _ _)) = Beta
getRule (SignedExpression False (BinaryExpression Disjunction _ _)) = Alpha
getRule (SignedExpression True (BinaryExpression Conditional _ _)) = Beta
getRule (SignedExpression False (BinaryExpression Conditional _ _)) = Alpha
getRule (SignedExpression _ (AtomicFormula _)) = Atom
getRule (SignedExpression _ (UnaryExpression Negation _)) = Alpha

addToExpressionSet :: ExpressionSet -> SignedExpression -> ExpressionSet
addToExpressionSet ee@(ExpressionSet _ _ True) _ = ee
addToExpressionSet (ExpressionSet ue ee False) se
  | r == Alpha = let a = alphaRules ue in
                    ExpressionSet ue {alphaRules = se : a} ee False
  | r == Beta  = let b = betaRules ue in
                    ExpressionSet ue {betaRules = se : b} ee False
  | r == Atom  = let a = atomicFormulas ee in
                    ExpressionSet ue (ee {atomicFormulas = se : a}) (containsConjucate se a)
  where
    r :: Rule
    r = getRule se

applyAlphaRule :: ExpressionSet -> AlphaExpression -> ExpressionSet
applyAlphaRule ee (SignedExpression b (UnaryExpression Negation expression))
  = addToExpressionSet ee (SignedExpression (not b) expression)
applyAlphaRule ee (SignedExpression _ (BinaryExpression Conjuction left right))
  = addToExpressionSet (addToExpressionSet ee (SignedExpression True left)) (SignedExpression True right)
applyAlphaRule ee (SignedExpression _ (BinaryExpression Disjunction left right))
  = addToExpressionSet (addToExpressionSet ee (SignedExpression False left)) (SignedExpression False right)
applyAlphaRule ee (SignedExpression _ (BinaryExpression Conditional left right))
  = addToExpressionSet (addToExpressionSet ee (SignedExpression True left)) (SignedExpression False right)

applyBetaRule :: ExpressionSet -> BetaExpression -> (ExpressionSet, ExpressionSet)
applyBetaRule ee (SignedExpression _ (BinaryExpression Conjuction left right))
  =  (addToExpressionSet ee (SignedExpression False left),
      addToExpressionSet ee (SignedExpression False right))
applyBetaRule ee (SignedExpression _ (BinaryExpression Disjunction left right))
  =  (addToExpressionSet ee (SignedExpression True left),
      addToExpressionSet ee (SignedExpression True right))
applyBetaRule ee (SignedExpression _ (BinaryExpression Conditional left right))
  =  (addToExpressionSet ee (SignedExpression False left),
      addToExpressionSet ee (SignedExpression True right))
applyBetaRule ee (SignedExpression b (BinaryExpression Inequalty left right))
  = applyBetaRule ee (SignedExpression (not b) (BinaryExpression Equality left right))
applyBetaRule ee (SignedExpression True (BinaryExpression Equality left right))
  = (addToExpressionSet (addToExpressionSet ee (SignedExpression False left)) (SignedExpression False right),
      addToExpressionSet (addToExpressionSet ee (SignedExpression True left)) (SignedExpression True right))
applyBetaRule ee (SignedExpression False (BinaryExpression Equality left right))
  = (addToExpressionSet (addToExpressionSet ee (SignedExpression False left)) (SignedExpression True right),
      addToExpressionSet (addToExpressionSet ee (SignedExpression True left)) (SignedExpression False right))

containsConjucate :: SignedAtomicExpression -> [SignedAtomicExpression] -> Bool
containsConjucate _ [] = False
containsConjucate ae@(SignedExpression t (AtomicFormula name))
                  (SignedExpression t' (AtomicFormula name'):xs)
                  | t /= t' && name == name' = True
                  | otherwise = containsConjucate ae xs

data Node = Node {expressionSet :: ExpressionSet,
                           next :: Node,
                         closed :: Closed}
            | BranchedNode {expressionSet :: ExpressionSet,
                                     left :: Node,
                                     right :: Node,
                                   closed :: Closed}
            | EndNode {expressionSet :: ExpressionSet,
                                   closed :: Closed} deriving (Eq)

printClosed :: Closed -> String
printClosed True = "closed"
printClosed False = "open"

instance Show Node where
  show EndNode {expressionSet = es, closed = b} = "[" ++ show es ++ " (" ++ printClosed b ++ ")]"
  show Node {expressionSet = es, next = n, closed = b} = "[" ++ show es ++ " (" ++ printClosed b ++ ") => " ++ show n ++ "]"
  show BranchedNode {expressionSet = es, left = l, right = r, closed = b}
    = "[" ++ show es ++ "(open)"   ++ show l ++ " | " ++ show r ++ "]"


removeAlphaRule :: ExpressionSet -> (ExpressionSet,  AlphaExpression)
removeAlphaRule (ExpressionSet ue@UnexpandedExpressions{alphaRules = (x:xs)} ee c)
  = (ExpressionSet ue{alphaRules = xs} ee c, x)

removeBetaRule :: ExpressionSet -> (ExpressionSet,  BetaExpression)
removeBetaRule (ExpressionSet ue@UnexpandedExpressions{betaRules = (x:xs)} ee c)
  = (ExpressionSet ue{betaRules = xs} ee c, x)

expand :: SignedExpression -> Node
expand se = expand' (addToExpressionSet (ExpressionSet
  UnexpandedExpressions{alphaRules = [], betaRules = []}
  ExpandedExpressions {atomicFormulas = []}
  False) se)


expand' :: ExpressionSet -> Node
expand' es@(ExpressionSet _ _ True) = EndNode {expressionSet = es, closed = True}
expand' es@(ExpressionSet UnexpandedExpressions{alphaRules = ar, betaRules = br} ee False)
  | not (null ar) = let (es', alphaRule) = removeAlphaRule es
                        nextEs = applyAlphaRule es' alphaRule
                        nextNode = expand' (applyAlphaRule es' alphaRule) in
                          Node  {expressionSet = es,
                                 next = nextNode,
                                 closed = closed nextNode}
  | not (null br) = let (es', betaRule) = removeBetaRule es
                        (leftEs, rightEs) = applyBetaRule es' betaRule
                        leftNode = expand' leftEs
                        rightNode = expand' rightEs in
                        BranchedNode {expressionSet = es,
                                      left = leftNode,
                                      right = rightNode,
                                      closed = closed leftNode && closed rightNode}
  | otherwise = EndNode es False
