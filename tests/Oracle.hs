{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.Compression.LZ4 (compressFrame, decompressFrame)
import Control.Monad (unless)
import Data.List (findIndex)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Minithesis
import qualified Minithesis.Hspec as MH
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, hSetBinaryMode)
import System.Process
  ( CreateProcess (std_err, std_in, std_out),
    StdStream (CreatePipe),
    createProcess,
    proc,
    waitForProcess,
  )
import Test.Hspec
import Prelude hiding (any)

main :: IO ()
main =
  hspec $
    describe "lz4 oracle (library vs CLI)" $
      MH.prop "framed encode/decode conforms to lz4 -z/-d" $
        withTests 40 $ \tc -> do
          input <- any tc byteStringStrategy

          mLibFrame <- compressFrame input
          libFrame <- case mLibFrame of
            Nothing ->
              mismatch "lib-compress-failed" input [("lib-compress-input.bin", input)] "compressFrame returned Nothing"
            Just payload -> pure payload

          cliDecoded <- runOrMismatch "cli-decode-lib-frame" input [("lib-frame.lz4", libFrame)] (runLz4Decode libFrame)
          assertBytesEqual "cli decode of lib frame differed from source" input cliDecoded

          cliFrame <- runOrMismatch "cli-compress" input [("cli-compress-input.bin", input)] (runLz4Encode input)
          mLibDecoded <- decompressFrame cliFrame
          libDecoded <- case mLibDecoded of
            Nothing ->
              mismatch "lib-decode-cli-frame" input [("cli-frame.lz4", cliFrame)] "decompressFrame returned Nothing for CLI payload"
            Just payload -> pure payload

          assertBytesEqual "lib decode of cli frame differed from source" input libDecoded
          assertBytesEqual "library and CLI decode outputs differed" libDecoded cliDecoded

byteStringStrategy :: Strategy BS.ByteString
byteStringStrategy =
  fmap BS.pack $
    lists (fmap integerToWord8 (integers 0 255)) (Just 0) (Just 4096)

integerToWord8 :: Integer -> Word8
integerToWord8 = fromIntegral

runOrMismatch :: String -> BS.ByteString -> [(FilePath, BS.ByteString)] -> IO (Either String BS.ByteString) -> IO BS.ByteString
runOrMismatch label input attachments ioResult = do
  result <- ioResult
  case result of
    Left err -> mismatch label input attachments err
    Right payload -> pure payload

runLz4Encode :: BS.ByteString -> IO (Either String BS.ByteString)
runLz4Encode input = runLz4 ["-z", "-q", "-c"] input

runLz4Decode :: BS.ByteString -> IO (Either String BS.ByteString)
runLz4Decode input = runLz4 ["-d", "-q", "-c"] input

runLz4 :: [String] -> BS.ByteString -> IO (Either String BS.ByteString)
runLz4 args input = do
  (exitCode, stdoutBytes, stderrBytes) <- runProcessBinary "lz4" args input
  case exitCode of
    ExitSuccess -> pure (Right stdoutBytes)
    ExitFailure code ->
      pure (Left ("lz4 " ++ unwords args ++ " failed with exit code " ++ show code ++ " and stderr: " ++ C8.unpack stderrBytes))

runProcessBinary :: FilePath -> [String] -> BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
runProcessBinary command args stdinBytes = do
  (Just stdinHandle, Just stdoutHandle, Just stderrHandle, processHandle) <-
    createProcess
      (proc command args)
        { std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe
        }
  hSetBinaryMode stdinHandle True
  hSetBinaryMode stdoutHandle True
  hSetBinaryMode stderrHandle True
  BS.hPut stdinHandle stdinBytes
  hClose stdinHandle
  stdoutBytes <- BS.hGetContents stdoutHandle
  stderrBytes <- BS.hGetContents stderrHandle
  exitCode <- waitForProcess processHandle
  pure (exitCode, stdoutBytes, stderrBytes)

assertBytesEqual :: String -> BS.ByteString -> BS.ByteString -> IO ()
assertBytesEqual reason expected actual =
  unless (expected == actual) $ do
    let extra = mismatchSummary expected actual
    expectationFailure (reason ++ ". " ++ extra)

mismatchSummary :: BS.ByteString -> BS.ByteString -> String
mismatchSummary expected actual =
  case findIndex id (BS.zipWith (/=) expected actual) of
    Just idx ->
      "first differing byte at offset "
        ++ show idx
        ++ " (expected="
        ++ show (BS.index expected idx)
        ++ ", actual="
        ++ show (BS.index actual idx)
        ++ ")"
    Nothing ->
      "length mismatch (expected="
        ++ show (BS.length expected)
        ++ ", actual="
        ++ show (BS.length actual)
        ++ ")"

mismatch :: String -> BS.ByteString -> [(FilePath, BS.ByteString)] -> String -> IO a
mismatch label input attachments details = do
  let artifactDir = "oracle-artifacts" </> "latest"
      metaFile = artifactDir </> "README.txt"
  createDirectoryIfMissing True artifactDir
  BS.writeFile (artifactDir </> "input.bin") input
  mapM_ (\(name, bytes) -> BS.writeFile (artifactDir </> name) bytes) attachments
  writeFile metaFile ("label: " ++ label ++ "\n" ++ details ++ "\n")
  expectationFailure ("oracle mismatch: " ++ label ++ ". " ++ details ++ ". artifacts: " ++ artifactDir)
  fail "oracle mismatch"
