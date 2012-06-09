-- | This is a binding to the LZ4 library, source of which can be found at
-- http://code.google.com/p/lz4/
--
-- Following zlib's lead, we pretend to be total on Bytestrings
-- errors may be thrown for bad formats. I'm not entirely comfortable with this,
-- but I'd like it to be a drop-in replacement for ease of use.
--
-- TODO: use a standard chunk size & support lazy Bytestrings, pending Yann's
-- publication of a formal standard.

module Codec.Compression.LZ4 (compress, compressHC, uncompress) where
import Codec.Compression.LZ4.Foreign
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Alloc(allocaBytes)
import Foreign.C.String
import Data.ByteString
import Data.ByteString.Unsafe
-- import Debug.Trace

-- |  Compresses a LZ4-packed String using the slow, better packing.
compressHC :: ByteString -> ByteString
compressHC x = compressWorker c_LZ4_compressHC x

-- |  Compresses a LZ4-packed String
compress :: ByteString -> ByteString
compress x = compressWorker c_LZ4_compress x

compressWorker :: Num b => (CString -> CString -> b -> IO Int) -> ByteString -> ByteString
compressWorker compressor x = unsafePerformIO $ unsafeUseAsCStringLen x $ \(input,len) ->
  let outLen = c_LZ4_compressBound len in
  allocaBytes (1+outLen) $ \out -> do
    written <- compressor input out (fromIntegral len)
    if written == 0
      then error "compression failed"
      else packCStringLen (out, written)
           --  `doTrace` ("compress", x, len, outLen, written)

-- |  Uncompresses a LZ4-packed String
--    No chunking yet - Yann is still working on a framing.
--    As a dirty hack, we assume that it won't decompress to more than 5 times
--    the length of the compressed file - this will go away with a fixed
--    frame size
uncompress :: ByteString -> ByteString
uncompress x = unsafePerformIO $ unsafeUseAsCStringLen x $ \(input,len) ->
  let outLen = 5 * len in
  allocaBytes (1+outLen) $ \out -> do
    written <- c_LZ4_uncompress_unknownOutputSize input out
                 (fromIntegral len)
                 (fromIntegral outLen)
    if written < 0
      then error "decompression failed!"
      else packCStringLen (out, written)
