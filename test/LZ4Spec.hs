{-# LANGUAGE OverloadedStrings #-}

module LZ4Spec(spec) where
-- import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.ShouldBe
import Control.Applicative
import Data.ByteString(ByteString,pack)
import Test.QuickCheck hiding (property)
-- import qualified Test.Hspec.Monadic
import Codec.Compression.LZ4



spec :: Specs
spec = do
  describe "reverse" $ do
    it "can compress" $ do
      -- print (compress $ pack [1])
      -- print (compress $ pack [1])
      compress (pack [1]) `shouldBe` compress (pack [1])
    it "can compress nulls" $ do
      (uncompress $ compress $ pack [0]) `shouldBe` pack [0]
    it "can round trip simply" $ do
      (uncompress $ compress (pack [1..10])) `shouldBe` pack [1..10]
    it "can roundtrip" $ do
      (property $ forAll string $
       \s -> (uncompress $ compress s) == s)
       
-- FIX really want a better random string algo
string :: Gen ByteString
string = do
   nums <- elements [1..10]
   -- can't handle embedded nulls
   pack <$> vectorOf nums (elements $ map fromIntegral [1..127])