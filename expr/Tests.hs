import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Prelude
import Prelude hiding (or,and)
import Expr

instance Arbitrary Val where
  arbitrary = oneof  [do b <- choose (False, True)
                         return (VBool b),
                      do i <- arbitrarySizedIntegral 
                         return (VInt i)]

properties = [ testProperty "+ Val" $ \x -> evalExpr (Val x) []  == x,
               testProperty "+ Var" $ \x -> evalExpr (Var "x") [("x", x)]  == x,
               
               testProperty "+ left child inherits env in Bop"  $ \x -> (isVInt x) ==> evalExpr (Bop (Var "x") Add (Val (VInt 0))) [("x",x)] == x,
               testProperty "+ right child inherits env in Bop" $ \x -> (isVInt x) ==> evalExpr (Bop (Val (VInt 0)) Add (Var "x")) [("x",x)] == x,
                 
               testProperty "+ Child inherits env in Uop" $ \x -> (isVInt x) ==> evalExpr (Uop Neg (Var "x")) [("x",x)] == negate x,

               testProperty "+ Not" $ \x -> (isVBool x) ==> evalExpr (Uop Not (Val x)) []  == no x,
               
               testProperty "+ Neg" $ \x -> (isVInt x) ==> evalExpr (Uop Neg (Val x)) []  == negate x,
               
               testProperty "+ Or" $ \x y-> (isVBool x && isVBool y) ==> evalExpr (Bop (Val x) Or (Val y)) []  == x `or` y,
               testProperty "+ Or is commutative" $ \x y-> (isVBool x && isVBool y) ==> evalExpr (Bop (Val x) Or (Val y)) []  == evalExpr (Bop (Val y) Or (Val x)) [],

               testProperty "+ And" $ \x y-> (isVBool x && isVBool y) ==> evalExpr (Bop (Val x) And (Val y)) []  == x `and` y,
               testProperty "+ And is commutative" $ \x y-> (isVBool x && isVBool y) ==> evalExpr (Bop (Val x) And (Val y)) []  == evalExpr (Bop (Val y) And (Val x)) [],
                              
               testProperty "+ Less (Bool)" $ \x y-> (isVBool x && isVBool y) ==> evalExpr (Bop (Val x) Lt (Val y)) []  == VBool (x < y), 
               testProperty "+ Less (Int)" $ \x y-> (isVInt x  && isVInt y) ==> evalExpr (Bop (Val x) Lt (Val y)) []  == VBool (x < y),                          
                              
               testProperty "+ Equals (Bool)" $ \x y-> (isVBool x && isVBool y) ==> evalExpr (Bop (Val x) Equ (Val y)) []  == VBool (x == y), 
               testProperty "+ Equals (Int)" $ \x y-> (isVInt x  && isVInt y) ==> evalExpr (Bop (Val x) Equ (Val y)) []  == VBool (x == y), 
               
               testProperty "+ Substraction" $ \x y-> (isVInt x  && isVInt y) ==> evalExpr (Bop (Val x) Sub (Val y)) []  == x - y, 
                              
               testProperty "+ Divition" $ \x y-> (isVInt x  && isVInt y && (toInteger y /= 0)) ==> evalExpr (Bop (Val x) Div (Val y)) []  == x `div` y,                          
                 
               testProperty "+ Module" $ \x y-> (isVInt x  && isVInt y && (toInteger y /= 0)) ==> evalExpr (Bop (Val x) Mod (Val y)) []  == x `mod` y,   
                 
               testProperty "+ Multiplication" $ \x y-> (isVInt x  && isVInt y) ==> evalExpr (Bop (Val x) Mul (Val y)) []  == x * y,
               testProperty "+ Multiplication is commutative" $ \x y-> (isVInt x  && isVInt y) ==> evalExpr (Bop (Val x) Mul (Val y)) []  == evalExpr (Bop (Val y) Mul (Val x)) [],
                              
               testProperty "+ Addition" $ \x y-> (isVInt x  && isVInt y) ==> evalExpr (Bop (Val x) Add (Val y)) []  == x + y,
               testProperty "+ Addition is commutative" $ \x y-> (isVInt x  && isVInt y) ==> evalExpr (Bop (Val x) Add (Val y)) []  == evalExpr (Bop (Val y) Add (Val x)) [],
               
               testProperty "+ De Morgan's law" $ \x y-> (isVBool x && isVBool y) ==> evalExpr (Uop Not (Bop (Val x) And (Val y))) []  == evalExpr (Bop (Uop Not (Val y)) Or (Uop Not (Val x))) [],
               testProperty "+ Addition Inverse" $ \x -> (isVInt x) ==> evalExpr (Bop (Val x) Add (Uop Neg (Val x))) []  == 0
             ]
tests = testGroup "Expr" properties

main = defaultMain $ tests