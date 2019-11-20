import Test.Tasty
import Test.Tasty.QuickCheck
import Expr


properties = [ testProperty "+ Val" $ \x -> evalExpr (Val x) []  == x,
               testProperty "+ Add" $ \x y-> evalExpr (Add (Val x) (Val y)) []  == x + y,
               testProperty "+ Var" $ \x -> evalExpr (Var "x") [("x",x)]  == x,
               testProperty "+ commutative" $ \x y-> evalExpr (Add (Val x) (Val y)) []  == evalExpr (Add (Val y) (Val x)) [] 
             ]
tests = testGroup "Expr" properties

main = defaultMain $ tests