module Codec.Compression.LZ4 (compress, compressHC, uncompress) where
import Codec.Compression.LZ4.Foreign
import Foreign
import Data.ByteString
import Debug.Trace
import Data.ByteString.Unsafe

-- Following zlib's lead, we pretend to be pure on Bytestrings
-- errors may be thrown for bad formats. I'm not entirely comfortable with this.

-- we use unsafeUseAsCStringLen because the underlying files may be large, and
-- we trust lz4 not to tamper with the input buffer.

-- TODO: use a standard chunk size & support lazy Bytestrings
-- TODO: load files from disk: we don't currently support the full file format with
--       magic archive numbers etc.

-- compressHC = compressWorker c_LZ4_compressHC
-- compress   = compressWorker c_LZ4_compress

-- for some reason, when I factor this out in the obvious way, I get link errors. Very confusing. FIXME
compressHC x = unsafePerformIO $ unsafeUseAsCStringLen x $ \(input,len) ->
  let outLen = c_LZ4_compressBound len in
  allocaBytes (1+outLen) $ \out -> do    
    written <- c_LZ4_compressHC input out (fromIntegral len)
    if written == 0
      then error "compression failed"
      else packCStringLen (out, written) --  `doTrace` ("compress", x, len, outLen, written)

compress x = unsafePerformIO $ unsafeUseAsCStringLen x $ \(input,len) ->
  let outLen = c_LZ4_compressBound len in
  allocaBytes (1+outLen) $ \out -> do    
    written <- c_LZ4_compress input out (fromIntegral len)
    if written == 0
      then error "compression failed"
      else packCStringLen (out, written) --  `doTrace` ("compress", x, len, outLen, written)

uncompress :: ByteString -> ByteString
uncompress x = unsafePerformIO $ unsafeUseAsCStringLen x $ \(input,len) ->
  -- this is a complete punt. let's say we use 5*len for the moment,
  -- and extend to lazy bytestrings later.
  let outLen = 5 * len in
  allocaBytes (1+outLen) $ \out -> do
    written <- c_LZ4_uncompress_unknownOutputSize input out (fromIntegral len) (fromIntegral outLen)
    if written < 0
      then error "decompression failed!"
      else packCStringLen (out, written)
