import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "Lib" $ do
    describe "mylast" $ do
      it "returns the last element of a list" $
        mylast "abcd" `shouldBe` 'd' 

    describe "butlast" $ do
      it "returns the next to the last element of a list" $
        butlast "abc" `shouldBe` 'b'

    describe "elt" $ do
      it "returns nth element of the list" $
        elt 1 "abcd" `shouldBe` 'b'

    describe "mylen" $ do
      it "returns 0 for an empty list" $
        mylen [] `shouldBe` 0
      it "returns the length of a list" $
        mylen "abc" `shouldBe` 3

    describe "myrev" $ do
      it "reverses a list" $
        myrev "abc" `shouldBe` "cba"

    describe "ispal" $ do
      it "detects palendromes" $
        ispal "racecar" `shouldBe` True
      it "rejects non-palendromes" $
        ispal "abcd" `shouldBe` False

    describe "flatten" $ do
      it "flattens nested lists" $
        flatten (List [Elem 'h', List [Elem 'i', Elem '!'] ]) `shouldBe` "hi!"

    describe "compress" $ do
      it "removes duplicates" $
        compress "aabbbcdd" `shouldBe` "abcd"

    describe "pack" $ do
      it "puts duplicates in sublists" $
        pack "aabbbcdd" `shouldBe` ["aa", "bbb", "c", "dd"]

    describe "rle" $ do
      it "run length encodes lists" $
        rle "aabbbcdd" `shouldBe` [(2,'a'), (3, 'b'), (1, 'c'), (2, 'd')]
{-
    describe "elt" $ do
      it "
-}
