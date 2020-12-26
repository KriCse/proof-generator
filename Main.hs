import ProofGenerator (evaluateExpression)

main :: IO ()
main = do
  putStrLn "Input a Formula:"
  str <- getLine
  putStrLn (evaluateExpression str)
