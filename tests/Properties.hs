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
    it "round-trips via the typed framed API" $ do
      let input = "lz4 typed frame round-trip test payload"
      compressed <- compressFrameEither input
      compressed `shouldSatisfy` either (const False) (not . BS.null)
      decompressed <- case compressed of
        Left err -> expectationFailure ("unexpected encode error: " ++ show err) >> return (Left DecompressFrameInvalidInput)
        Right payload -> decompressFrameEither payload
      decompressed `shouldBe` Right input
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
    it "decompresses concatenated standard lz4 frames" $ do
      let combined = standardFrameFixture <> standardFrameFixture
      decompressFrame combined `shouldReturn` Just ("standard lz4 frame payload\n" <> "standard lz4 frame payload\n")
    it "fails cleanly on truncated framed payload fixture" $ do
      decompressFrame truncatedFrameFixture `shouldReturn` Nothing
    it "fails cleanly on corrupted frame header fixture" $ do
      decompressFrame badHeaderFrameFixture `shouldReturn` Nothing
    it "fails cleanly on corrupted frame checksum fixture" $ do
      decompressFrame badChecksumFrameFixture `shouldReturn` Nothing
    it "returns typed malformed-input errors for representative invalid frames" $ do
      let assertMalformed payload = do
            result <- decompressFrameEither payload
            case result of
              Left (DecompressFrameMalformedInput _) -> return ()
              other -> expectationFailure ("expected malformed-input error, got: " ++ show other)
      assertMalformed truncatedFrameFixture
      assertMalformed badHeaderFrameFixture
      assertMalformed badChecksumFrameFixture
    it "keeps typed decode errors backward-compatible with Maybe wrapper" $ do
      typed <- decompressFrameEither truncatedFrameFixture
      legacy <- decompressFrame truncatedFrameFixture
      typed `shouldSatisfy` (\r -> case r of Left (DecompressFrameMalformedInput _) -> True; _ -> False)
      legacy `shouldBe` Nothing
    it "fails cleanly on trailing malformed bytes after complete frame stream" $ do
      let malformed = standardFrameFixture <> BS.pack [0xff, 0x00, 0x7f]
      decompressFrame malformed `shouldReturn` Nothing
    it "supports bounded framed decode when output fits within limit" $ do
      result <- decompressFrameBounded 1024 standardFrameFixture
      result `shouldBe`
        Right
          (DecompressFrameBoundedResult
            "standard lz4 frame payload\n"
            DecompressFrameBoundedDone)
    it "supports bounded framed decode continuation across limit boundaries" $ do
      let payload = standardFrameFixture <> standardFrameFixture
          stepSize = 10
      collected <- collectBoundedChunks 0 stepSize payload []
      collected `shouldBe` "standard lz4 frame payload\nstandard lz4 frame payload\n"
    it "reports malformed input for bounded framed decode" $ do
      decompressFrameBounded 1024 truncatedFrameFixture
        `shouldReturn` Left DecompressFrameBoundedMalformedInput
    it "rejects invalid bounded framed decode parameters" $ do
      decompressFrameBoundedFrom (-1) 8 standardFrameFixture
        `shouldReturn` Left DecompressFrameBoundedInvalidOffset
      decompressFrameBounded 0 standardFrameFixture
        `shouldReturn` Left DecompressFrameBoundedInvalidLimit
  describe "regression corpus fixtures" $ do
    it "decodes representative legacy payload fixture with legacy decoder only" $ do
      decompress legacyPayloadFixture `shouldBe` Just "standard lz4 legacy payload\n"
      decompressFrame legacyPayloadFixture `shouldReturn` Nothing
    it "decodes representative framed payload fixture with framed decoder only" $ do
      decompress standardFrameFixture `shouldBe` Nothing
      decompressFrame standardFrameFixture `shouldReturn` Just "standard lz4 frame payload\n"
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

-- Regression corpus expectations:
--   * standardFrameFixture / emptyFrameFixture: valid framed payloads -> Just decoded text.
--   * truncatedFrameFixture / badHeaderFrameFixture / badChecksumFrameFixture: invalid framed payloads -> Nothing.
--   * legacyPayloadFixture: valid legacy payload -> legacy decoder Just decoded text, framed decoder Nothing.
truncatedFrameFixture :: BS.ByteString
truncatedFrameFixture = BS.take (BS.length standardFrameFixture - 1) standardFrameFixture

badHeaderFrameFixture :: BS.ByteString
badHeaderFrameFixture = BS.cons 0x00 (BS.drop 1 standardFrameFixture)

badChecksumFrameFixture :: BS.ByteString
badChecksumFrameFixture = BS.take (BS.length standardFrameFixture - 1) standardFrameFixture <> BS.singleton 0x89

legacyPayloadFixture :: BS.ByteString
legacyPayloadFixture = BS.pack
  [ 28, 0, 0, 0, 30, 0, 0, 0, 240, 13, 115, 116, 97, 110, 100, 97
  , 114, 100, 32, 108, 122, 52, 32, 108, 101, 103, 97, 99, 121, 32
  , 112, 97, 121, 108, 111, 97, 100, 10
  ]

prop_compress_pure comp decomp (S.pack -> xs) =
  (comp xs) == (comp xs)

prop_compression_id comp decomp (S.pack -> xs) =
  maybe False (== xs) (comp xs >>= decomp)

prop_decompress_pure comp decomp (S.pack -> xs) =
  let z = comp xs
  in (z >>= decomp) == (z >>= decomp)

collectBoundedChunks :: Int -> Int -> BS.ByteString -> [BS.ByteString] -> IO BS.ByteString
collectBoundedChunks offset stepSize payload acc = do
  result <- decompressFrameBoundedFrom offset stepSize payload
  case result of
    Left err ->
      expectationFailure ("unexpected bounded decode failure: " ++ show err) >> return BS.empty
    Right (DecompressFrameBoundedResult chunk DecompressFrameBoundedDone) ->
      return (BS.concat (reverse (chunk : acc)))
    Right (DecompressFrameBoundedResult chunk (DecompressFrameBoundedLimitReached nextOffset))
      | nextOffset <= offset ->
          expectationFailure "bounded decode continuation did not advance" >> return BS.empty
      | otherwise ->
          collectBoundedChunks nextOffset stepSize payload (chunk : acc)
