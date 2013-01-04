{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
module Main (main) where
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit
import Test.HUnit
import Test.QuickCheck
import Control.Applicative

import qualified Data.ByteString as S
import Codec.Compression.LZ4

main :: IO ()
main = hspec $ do
  describe "compression" $ do
    prop "is pure"              $ prop_compress_pure compress decompress
    prop "(>>= decomp) = id" $ prop_compression_id compress decompress
  describe "high compression" $ do
    prop "is pure"              $ prop_compress_pure compressHC decompress
    prop "(>>= decomp) = id" $ prop_compression_id compressHC decompress
  describe "ultra compression" $ do
    prop "is pure"              $ prop_compress_pure compressPlusHC decompressPlusHC
    prop "(>>= decomp) = id" $ prop_compression_id compressPlusHC decompressPlusHC
  describe "decompression" $ do
    prop "is pure (normal)"     $ prop_decompress_pure compress decompress
    prop "is pure (high)"       $ prop_decompress_pure compressHC decompress
    prop "is pure (ultra)"      $ prop_decompress_pure compressPlusHC decompressPlusHC
  describe "regression test" $ do
    let input = "\STXd\STX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\vexample.com\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SI\NUL\NUL\NUL\NUL\NUL\NUL\NUL\tWhirlpool\NUL\NUL\NUL\NUL\NUL\NUL\NUL\vexample.com\NUL\STXf\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ffacebook.com\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOTSHA1\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ffacebook.com\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\tgmail.com\SOH\NUL\NUL\NUL\NUL\NUL\NUL"
    it "can compress an oddly full-of-NULLs string"
      (TestCase $ assertEqual "odd string" (Just input) (return input >>=
                                                         compress >>= 
                                                         decompress))
      

prop_compress_pure comp decomp (S.pack -> xs) =
  (comp xs) == (comp xs)

prop_compression_id comp decomp (S.pack -> xs) =
  maybe False (== xs) (comp xs >>= decomp) 

prop_decompress_pure comp decomp (S.pack -> xs) =
  let z = comp xs
  in (z >>= decomp) == (z >>= decomp)


