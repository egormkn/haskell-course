module Hw2Spec where

import Data.List.NonEmpty (fromList)
import Hw2
import Test.Hspec

spec :: Spec
spec = do
  it "eval" $ do
    eval (Const 10) `shouldBe` Right 10
    eval (Sum (Const 5) (Const 10)) `shouldBe` Right 15
    eval (Sub (Const 5) (Const 10)) `shouldBe` Right (-5)
    eval (Mul (Const 5) (Const 10)) `shouldBe` Right 50
    eval (Div (Const 50) (Const 10)) `shouldBe` Right 5
    eval (Pow (Const 2) (Const 10)) `shouldBe` Right 1024

  it "eval" $ do -- TODO
    eval (Const 10) `shouldBe` Right 10
    eval (Sum (Const 5) (Const 10)) `shouldBe` Right 15


  it "constant" $
      eval (Const 4) `shouldBe` Right 4
  it "contant 2" $
      eval (Const (-34)) `shouldBe` Right (-34)
  it "a + b" $
      eval (Sum (Const 2) (Const 421)) `shouldBe` Right (421 + 2)
  it "a - b" $
      eval (Sub (Const 4) (Const (-6))) `shouldBe` Right (4 - (-6))
  it "a * b" $
      eval (Mul (Const 3) (Const 32)) `shouldBe` Right (3 * 32)
  it "a / b, b /= 0" $
      eval (Div (Const 432) (Const 4)) `shouldBe` Right (432 `div` 4)
  it "a / b, b == 0" $
      eval (Div (Const 43) (Const 0)) `shouldBe` Left DivideByZero
  it "a ^ b, b > 0" $
      eval (Pow (Const 2) (Const 5)) `shouldBe` Right 32
  it "a ^ b, b < 0" $
      eval (Pow (Const 2) (Const (-4))) `shouldBe` Left NegativeExponent
  it "a * (b + c)" $
      eval (Mul (Const 4) (Sum (Const 6) (Const 30))) `shouldBe` Right (4 * (6 + 30))


  it "bin 0" $
      bin 0 `shouldBe` [[]]


  it "const" $
      stringSum "34" `shouldBe` Just 34
  it "const const" $
      stringSum "43 -32" `shouldBe` Just (43 - 32)
  it "fail" $
      stringSum "fail" `shouldBe` Nothing
  it "const \\n const" $
      stringSum "4 \r\t\n 3" `shouldBe` Just (4 + 3)


  it "ok (empty string)" $
      runParser ok "" `shouldBe` Just ((), "")
  it "ok (string)" $
      runParser ok "value" `shouldBe` Just ((), "value")
  it "ok (array)" $
      runParser ok [1 :: Int, 2] `shouldBe` Just ((), [1, 2])
  it "eof (empty string)" $
      runParser eof "" `shouldBe` Just ((), "")
  it "eof (string)" $
      runParser eof "value" `shouldBe` Nothing
  it "satisfy (char)" $
      runParser (satisfy (== 0)) [0 :: Int, 1] `shouldBe` Just (0, [1])
  it "satisfy (bad char)" $
      runParser (satisfy (== 0)) [10 :: Int, 1] `shouldBe` Nothing

