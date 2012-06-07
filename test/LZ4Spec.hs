{-# LANGUAGE OverloadedStrings #-}

module LZ4Spec(spec) where

import Test.Hspec.QuickCheck
import Test.Hspec.ShouldBe
import Control.Applicative
import Data.ByteString(ByteString,pack)
import Test.QuickCheck hiding (property)
import Codec.Compression.LZ4



spec :: Specs
spec = describe "LZ4" $ do
    it "can compress" $ 
      compress (pack [1]) `shouldBe` compress (pack [1])
    it "can compress nulls" $
      uncompress (compress $ pack [1,0,1]) `shouldBe` pack [1,0,1]
    it "can round trip simply" $ 
      uncompress (compress (pack [1..10])) `shouldBe` pack [1..10]
    it "can roundtrip"
      (property $ forAll string $
       \s -> uncompress (compress s) == s)
    it "can roundtrip using high compression"
      (property $ forAll string $
       \s -> uncompress (compressHC s) == s)  
      
       
-- FIX really want a better random string algo
string :: Gen ByteString
string = do
   nums <- elements [1..10000]
   -- can't handle embedded nulls
   pack <$> vectorOf nums (elements $ map fromIntegral [0..127])