{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Main (main) where
import           Control.Applicative
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Codec.Compression.LZ4
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString as BS

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
  describe "frame format" $ do
    it "round-trips via the framed API" $ do
      let input = "lz4 frame round-trip test payload"
      compressed <- compressFrame input
      compressed `shouldSatisfy` maybe False (not . BS.null)
      decompressed <- maybe (return Nothing) decompressFrame compressed
      decompressed `shouldBe` Just input
    it "keeps framed API separate from legacy decode format" $ do
      let input = "legacy and framed APIs should stay independent"
      compressed <- compressFrame input
      compressed `shouldSatisfy` maybe False (not . BS.null)
      case compressed of
        Nothing -> expectationFailure "expected compressed frame payload"
        Just framePayload -> do
          decompress framePayload `shouldBe` Nothing
          decompressFrame framePayload `shouldReturn` Just input
    it "decompresses standard lz4 frame payload fixture" $ do
      decompressFrame standardFrameFixture `shouldReturn` Just "standard lz4 frame payload\n"
    it "decompresses standard empty-file lz4 frame fixture" $ do
      decompressFrame emptyFrameFixture `shouldReturn` Just ""
  describe "regression test" $ do
    let input = "\STXd\STX\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\vexample.com\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SI\NUL\NUL\NUL\NUL\NUL\NUL\NUL\tWhirlpool\NUL\NUL\NUL\NUL\NUL\NUL\NUL\vexample.com\NUL\STXf\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ffacebook.com\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOTSHA1\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ffacebook.com\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\tgmail.com\SOH\NUL\NUL\NUL\NUL\NUL\NUL"
    it "can compress an oddly full-of-NULLs string" $ do
      (compress input >>= decompress) `shouldBe`  Just input

standardFrameFixture :: BS.ByteString
standardFrameFixture = BS.pack
  [ 0x04, 0x22, 0x4d, 0x18, 0x64, 0x40, 0xa7, 0x1b
  , 0x00, 0x00, 0x80, 0x73, 0x74, 0x61, 0x6e, 0x64
  , 0x61, 0x72, 0x64, 0x20, 0x6c, 0x7a, 0x34, 0x20
  , 0x66, 0x72, 0x61, 0x6d, 0x65, 0x20, 0x70, 0x61
  , 0x79, 0x6c, 0x6f, 0x61, 0x64, 0x0a, 0x00, 0x00
  , 0x00, 0x00, 0x68, 0x7d, 0xb4, 0x88
  ]

emptyFrameFixture :: BS.ByteString
emptyFrameFixture = BS.pack
  [ 0x04, 0x22, 0x4d, 0x18, 0x64, 0x40, 0xa7, 0x00
  , 0x00, 0x00, 0x00, 0x05, 0x5d, 0xcc, 0x02
  ]

prop_compress_pure comp decomp (S.pack -> xs) =
  (comp xs) == (comp xs)

prop_compression_id comp decomp (S.pack -> xs) =
  maybe False (== xs) (comp xs >>= decomp)

prop_decompress_pure comp decomp (S.pack -> xs) =
  let z = comp xs
  in (z >>= decomp) == (z >>= decomp)
