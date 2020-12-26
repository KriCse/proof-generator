module ProofGenerator where
import PLexer (tokenize)
import PParser (parseExpression, BinaryOperator(..))
import ProofTree (SignedExpression(..), Node(..), expand, printClosed)

type BranchIndex = Int
type RuleIndex = Int
type Index = (BranchIndex, RuleIndex)

printRuleIndex :: Index -> String
printRuleIndex (bi, ri) = show bi ++ "." ++ show ri

printTree :: Node -> String
printTree n = (if closed n then "All Branches are open." else "Some Branches are open")
              ++ "\n" ++ printTree' (1, 1) n

printTree' :: Index -> Node -> String
printTree' index EndNode {expressionSet = es, closed = b} = printRuleIndex index ++ " " ++ show es ++ "\n(" ++ printClosed b ++ ")\n"

printTree' index@(bi, ri) Node {expressionSet = es, next = nextNode}
 = printRuleIndex index ++ " " ++ show es ++ "\n" ++ printTree' (bi, succ ri) nextNode

printTree' index@(bi, ri) BranchedNode {expressionSet = es, left = leftNode, right = rightNode}
   = printRuleIndex index ++ " " ++ show es ++ "\n" ++ printTree' (bi, succ ri) leftNode ++ printTree' (succ bi, 1) rightNode

evaluateExpression :: String -> String
evaluateExpression str = printTree (expand se)
  where
    se :: SignedExpression
    se = SignedExpression False (parseExpression (tokenize str))
