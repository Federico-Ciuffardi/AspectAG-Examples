import Test.Tasty
import Test.Tasty.QuickCheck
import Expr


properties = [ testProperty "+ Val" $ \x -> evalExpr (Val x) []  == x,
               testProperty "+ Var" $ \x -> evalExpr (Var "x") [("x",x)]  == x,
               
               testProperty "+ Addition-Val+Val" $ \x y-> evalExpr (Add (Val x) (Val y)) []  == x + y,
               testProperty "+ Addition-Var+Val" $ \x y-> evalExpr (Add (Var "x") (Val y)) [("x",x)] == x + y,
               testProperty "+ Addition-Val+Var" $ \x y-> evalExpr (Add (Val x) (Var "y")) [("y",y)] == x + y,
               testProperty "+ Addition-Var+Var" $ \x y-> evalExpr (Add (Var "x") (Var "y")) [("x",x),("y",y)] == x + y,
               testProperty "+ Addition is commutative" $ \x y-> evalExpr (Add (Val x) (Val y)) []  == evalExpr (Add (Val y) (Val x)) [],
               
               testProperty "+ Multiplication-Val+Val" $ \x y-> evalExpr (Mul (Val x) (Val y)) []  == x * y,
               testProperty "+ Multiplication-Var+Val" $ \x y-> evalExpr (Mul (Var "x") (Val y)) [("x",x)] == x * y,
               testProperty "+ Multiplication-Val+Var" $ \x y-> evalExpr (Mul (Val x) (Var "y")) [("y",y)] == x * y,
               testProperty "+ Multiplication-Var+Var" $ \x y-> evalExpr (Mul (Var "x") (Var "y")) [("x",x),("y",y)] == x * y,
               testProperty "+ Addition is commutative" $ \x y-> evalExpr (Mul (Val x) (Val y)) []  == evalExpr (Mul (Val y) (Val x)) [] 
             ]
tests = testGroup "Expr" properties

main = defaultMain $ tests