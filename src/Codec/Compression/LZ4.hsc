{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Codec.Compression.LZ4
-- Copyright   : (c) Mark Wotton, Austin Seipp 2012
-- License     : BSD3
--
-- Maintainer  : mwotton@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides a high level 'ByteString' interface to the
-- lz4 library. More information about lz4 can be found here:
-- <http://code.google.com/p/lz4/>.
--
-- This module prefixes the buffer that is compressed with the
-- uncompressed length (as lz4 can't recover this information
-- itself.) It also has this property: all functions when
-- called with an empty string return @Just Data.ByteString.empty@
--
module Codec.Compression.LZ4
       ( -- * High level interface
         -- ** Compressing and decompressing strict 'ByteString's
         compress            -- :: S.ByteString -> S.ByteString
       , decompress          -- :: S.ByteString -> Maybe S.ByteString

         -- ** High-compression mode
       , compressHC          -- :: S.ByteString -> S.ByteString

         -- ** Compression + HC mode
       , compressPlusHC      -- :: S.ByteString -> S.ByteString
       , decompressPlusHC    -- :: S.ByteString -> S.ByteString

         -- ** Framed format (compatible with @lz4@ CLI files)
       , compressFrame       -- :: S.ByteString -> IO (Maybe S.ByteString)
       , decompressFrame     -- :: S.ByteString -> IO (Maybe S.ByteString)
       , DecompressFrameBoundedError(..)
       , DecompressFrameBoundedStatus(..)
       , DecompressFrameBoundedResult(..)
       , decompressFrameBounded
       , decompressFrameBoundedFrom

         -- * FFI functions
       , c_LZ4_compress      -- :: Ptr CChar -> Ptr Word8 -> CInt -> IO CInt
       , c_LZ4_compressHC    -- :: Ptr CChar -> Ptr Word8 -> CInt -> IO CInt
       , c_LZ4_uncompress    -- :: Ptr CChar -> Ptr Word8 -> CInt -> IO CInt
       , c_LZ4_compressBound -- :: CInt -> CInt
       ) where

import Prelude
import Data.Word
import Foreign.Ptr
import Foreign.C
import Control.Exception (finally)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as SI
import qualified Data.ByteString.Unsafe as U

import Data.Serialize

#include <lz4.h>
#include <lz4hc.h>
#include <lz4frame.h>

type LZ4FDecompressionContext = Ptr ()

lz4fVersion :: CUInt
lz4fVersion = #{const LZ4F_VERSION}

-- | Errors returned by bounded framed decompression.
data DecompressFrameBoundedError
  = DecompressFrameBoundedInvalidOffset
  | DecompressFrameBoundedInvalidLimit
  | DecompressFrameBoundedMalformedInput
  deriving (Eq, Show)

-- | Bounded framed decompression completion status.
data DecompressFrameBoundedStatus
  = DecompressFrameBoundedDone
  | DecompressFrameBoundedLimitReached !Int
  deriving (Eq, Show)

-- | Output produced by bounded framed decompression.
data DecompressFrameBoundedResult = DecompressFrameBoundedResult
  { decompressFrameBoundedOutput :: !S.ByteString
  , decompressFrameBoundedStatus :: !DecompressFrameBoundedStatus
  } deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Compression

-- | Compresses the input 'ByteString'.
--
-- Will return 'Nothing' if the compression fails. Otherwise, returns
-- @Just xs@ with the compressed string (and additionally, if @xs ==
-- empty@ then @compress empty == Just empty@.)
compress :: S.ByteString -> Maybe S.ByteString
compress xs
  | S.null xs = Just S.empty
  | otherwise = compressor c_LZ4_compress xs
{-# INLINEABLE compress #-}

-- | Compress the input 'ByteString' as much as possible, but comes
-- with a massive speed drop in compression. Decompression is faster
-- however and can be done with 'decompress'.
--
-- Will return 'Nothing' if the compression fails. Otherwise, returns
-- @Just xs@ with the compressed string (and additionally, if @xs ==
-- empty@ then @compressHC empty == Just empty@.)
compressHC :: S.ByteString -> Maybe S.ByteString
compressHC xs
  | S.null xs = Just S.empty
  | otherwise = compressor c_LZ4_compressHC xs
{-# INLINEABLE compressHC #-}

-- | Essentially defined as:
--
-- > compressPlusHC xs = compress xs >>= compressHC
--
--
-- This is an experimental interface. After regular compression, due
-- to output encoding, things like relative offsets in the compression
-- buffer or artifacts from number encoding can end up the same in the
-- output buffer for often repeated data. Therefore, further savings
-- are possible in the input buffer by compressing again. lz4 even in
-- high compression mode will quickly ignore already-compressed data
-- and remain quite fast. Thus, this interface is designed to give a
-- better compression/speed tradeoff than 'compressHC': it doesn't
-- compress as well, but is nowhere near as slow. Some context:
-- <http://www.reddit.com/r/programming/comments/vyu7r/compressing_log_files_twice_improves_ratio/c58svj3?context=3>
--
-- Must be decompressed with 'decompressPlusHC'.
--
-- Will return 'Nothing' if the compression fails. Otherwise, returns
-- @Just xs@ with the compressed string (and additionally, if @xs ==
-- empty@ then @compressPlusHC empty == Just empty@.)
compressPlusHC :: S.ByteString -> Maybe S.ByteString
compressPlusHC xs
  | S.null xs = Just S.empty
  | otherwise = compress xs >>= compressHC
{-# INLINEABLE compressPlusHC #-}


--------------------------------------------------------------------------------
-- Decompression

-- | Decompress the input 'ByteString'.
decompress :: S.ByteString -> Maybe S.ByteString
decompress xs
  | S.null xs = Just S.empty
  | otherwise =
      -- Get the length of the uncompressed buffer and do our thing
      either (const Nothing) (unsafePerformIO . go) $ runGet unformat xs
  where go (l, str) =
          U.unsafeUseAsCString str $ \cstr -> do
            out <- SI.createAndTrim l $ \p -> do
              r :: Int <- fromIntegral <$> c_LZ4_uncompress cstr p (fromIntegral l)
              --- NOTE: r is the count of bytes c_LZ4_uncompress read from input buffer,
              --- and NOT the count of bytes used in result buffer
              return $! if (r <= 0) then 0 else l
            return $! if (S.null out) then Nothing else (Just out)
{-# INLINEABLE decompress #-}

-- | Decompress a string compressed with 'compressPlusHC'. Essentially
-- defined as:
-- 
-- > decompressPlusHC xs = decompress xs >>= decompress
-- 
decompressPlusHC :: S.ByteString -> Maybe S.ByteString
decompressPlusHC xs
  | S.null xs = Just S.empty
  | otherwise = decompress xs >>= decompress
{-# INLINEABLE decompressPlusHC #-}

-- | Compress to the standard LZ4 frame format (compatible with @lz4@ CLI files).
--
-- Returns 'Nothing' if frame encoding fails.
compressFrame :: S.ByteString -> IO (Maybe S.ByteString)
compressFrame xs =
  U.unsafeUseAsCStringLen xs $ \(src, srcLen) -> do
    let srcLen' = fromIntegral srcLen :: CSize
        dstCap = c_LZ4F_compressFrameBound srcLen' nullPtr
    if lz4fIsError dstCap
      then return Nothing
      else do
        out <- SI.createAndTrim (fromIntegral dstCap) $ \dst -> do
          written <- c_LZ4F_compressFrame dst dstCap (castPtr src) srcLen' nullPtr
          return $! if lz4fIsError written then 0 else fromIntegral written
        return $! if S.null out then Nothing else Just out

-- | Decompress the standard LZ4 frame format (compatible with @lz4@ CLI files).
--
-- Returns 'Nothing' if input is not valid framed LZ4 data.
decompressFrame :: S.ByteString -> IO (Maybe S.ByteString)
decompressFrame xs
  | S.null xs = return Nothing
  | otherwise =
      U.unsafeUseAsCStringLen xs $ \(src, srcLen) ->
        alloca $ \ctxPtr -> do
          createResult <- c_LZ4F_createDecompressionContext ctxPtr lz4fVersion
          if lz4fIsError createResult
            then return Nothing
            else do
              ctx <- peek ctxPtr
              finally (go ctx src srcLen 0 []) (c_LZ4F_freeDecompressionContext ctx >> return ())
  where
    outputChunkSize = 64 * 1024

    go :: LZ4FDecompressionContext -> Ptr CChar -> Int -> Int -> [S.ByteString] -> IO (Maybe S.ByteString)
    go ctx src srcLen srcOffset chunks = allocaBytes outputChunkSize $ \dst -> do
      alloca $ \dstSizePtr ->
        alloca $ \srcSizePtr -> do
          poke dstSizePtr (fromIntegral outputChunkSize :: CSize)
          poke srcSizePtr (fromIntegral (srcLen - srcOffset) :: CSize)
          hint <- c_LZ4F_decompress ctx
                                   (castPtr dst)
                                   dstSizePtr
                                   (castPtr src `plusPtr` srcOffset)
                                   srcSizePtr
                                   nullPtr
          consumed <- fromIntegral <$> peek srcSizePtr
          produced <- fromIntegral <$> peek dstSizePtr
          if lz4fIsError hint
            then return Nothing
            else do
              chunk <- if produced == 0
                         then return S.empty
                         else S.packCStringLen (dst, produced)
              let nextOffset = srcOffset + consumed
                  nextChunks = if S.null chunk then chunks else chunk : chunks
              if hint == 0
                then if nextOffset == srcLen
                       then return $! Just (S.concat (reverse nextChunks))
                       else if consumed == 0 && produced == 0
                              then return Nothing
                              else go ctx src srcLen nextOffset nextChunks
                else if consumed == 0 && produced == 0
                       then return Nothing
                       else go ctx src srcLen nextOffset nextChunks

-- | Decompress framed LZ4 with an explicit output byte limit.
--
-- If decoded output reaches @limit@ bytes before stream completion, returns
-- 'DecompressFrameBoundedLimitReached' with the next output offset. Continue by
-- calling 'decompressFrameBoundedFrom' with that offset and the same input.
--
-- Malformed input yields 'Left DecompressFrameBoundedMalformedInput'.
decompressFrameBounded :: Int
                       -> S.ByteString
                       -> IO (Either DecompressFrameBoundedError DecompressFrameBoundedResult)
decompressFrameBounded limit = decompressFrameBoundedFrom 0 limit

-- | Resume bounded framed decompression from a decoded-output offset.
--
-- Continuation contract:
--   * 'DecompressFrameBoundedDone' means full stream completion.
--   * 'DecompressFrameBoundedLimitReached nextOffset' means partial output.
--     Resume with @decompressFrameBoundedFrom nextOffset limit input@.
--   * 'Left DecompressFrameBoundedMalformedInput' means decoding failed and
--     no further continuation is valid for that payload.
decompressFrameBoundedFrom :: Int
                           -> Int
                           -> S.ByteString
                           -> IO (Either DecompressFrameBoundedError DecompressFrameBoundedResult)
decompressFrameBoundedFrom outputOffset limit xs
  | outputOffset < 0 = return (Left DecompressFrameBoundedInvalidOffset)
  | limit <= 0 = return (Left DecompressFrameBoundedInvalidLimit)
  | S.null xs = return (Left DecompressFrameBoundedMalformedInput)
  | otherwise =
      U.unsafeUseAsCStringLen xs $ \(src, srcLen) ->
        alloca $ \ctxPtr -> do
          createResult <- c_LZ4F_createDecompressionContext ctxPtr lz4fVersion
          if lz4fIsError createResult
            then return (Left DecompressFrameBoundedMalformedInput)
            else do
              ctx <- peek ctxPtr
              finally (go ctx src srcLen 0 0 [] 0) (c_LZ4F_freeDecompressionContext ctx >> return ())
  where
    outputChunkSize = 64 * 1024
    windowEnd = outputOffset + limit

    go :: LZ4FDecompressionContext
       -> Ptr CChar
       -> Int
       -> Int
       -> Int
       -> [S.ByteString]
       -> Int
       -> IO (Either DecompressFrameBoundedError DecompressFrameBoundedResult)
    go ctx src srcLen srcOffset producedTotal chunks collectedLen = allocaBytes outputChunkSize $ \dst -> do
      alloca $ \dstSizePtr ->
        alloca $ \srcSizePtr -> do
          poke dstSizePtr (fromIntegral outputChunkSize :: CSize)
          poke srcSizePtr (fromIntegral (srcLen - srcOffset) :: CSize)
          hint <- c_LZ4F_decompress ctx
                                   (castPtr dst)
                                   dstSizePtr
                                   (castPtr src `plusPtr` srcOffset)
                                   srcSizePtr
                                   nullPtr
          consumed <- fromIntegral <$> peek srcSizePtr
          produced <- fromIntegral <$> peek dstSizePtr
          if lz4fIsError hint
            then return (Left DecompressFrameBoundedMalformedInput)
            else do
              chunk <- if produced == 0
                         then return S.empty
                         else S.packCStringLen (dst, produced)
              let nextOffset = srcOffset + consumed
                  producedEnd = producedTotal + produced
                  chunkStart = max outputOffset producedTotal
                  chunkEnd = min windowEnd producedEnd
                  captureLen = max 0 (chunkEnd - chunkStart)
                  captureDrop = max 0 (chunkStart - producedTotal)
                  captureChunk =
                    if captureLen == 0 || S.null chunk
                      then S.empty
                      else S.take captureLen (S.drop captureDrop chunk)
                  nextChunks =
                    if S.null captureChunk
                      then chunks
                      else captureChunk : chunks
                  nextCollectedLen = collectedLen + S.length captureChunk
                  done = hint == 0 && nextOffset == srcLen
              if done
                then return $ Right $
                  DecompressFrameBoundedResult
                    (S.concat (reverse nextChunks))
                    (if producedEnd > windowEnd
                       then DecompressFrameBoundedLimitReached windowEnd
                       else DecompressFrameBoundedDone)
                else if consumed == 0 && produced == 0
                       then return (Left DecompressFrameBoundedMalformedInput)
                       else if nextCollectedLen >= limit
                              then return $ Right $
                                DecompressFrameBoundedResult
                                  (S.concat (reverse nextChunks))
                                  (DecompressFrameBoundedLimitReached (outputOffset + nextCollectedLen))
                              else go ctx src srcLen nextOffset producedEnd nextChunks nextCollectedLen

lz4fIsError :: CSize -> Bool
lz4fIsError code = c_LZ4F_isError code /= 0

--------------------------------------------------------------------------------
-- Utilities

-- The compression methods are all identical, so this just abstracts them
compressor :: (Ptr CChar -> Ptr Word8 -> CInt -> IO CInt)
           -> S.ByteString
           -> Maybe S.ByteString
compressor f xs = unsafePerformIO $ do
  U.unsafeUseAsCStringLen xs $ \(cstr,len) -> do
    let len' = fromIntegral len :: CInt
    let maxLen = c_LZ4_compressBound len'
    bs <- SI.createAndTrim (fromIntegral maxLen) $ \output ->
            fromIntegral <$> f cstr output len'
    return $ if S.null bs then Nothing else
               -- Prefix the compressed string with the uncompressed length
               Just $ runPut $ format (fromIntegral len) bs
{-# INLINEABLE compressor #-}

-- Pushes a Word32 and a ByteString into the format we use to correctly
-- encode/decode.
format :: Word32 -> Putter S.ByteString
format l xs = do
  putWord32le l
  putWord32le (fromIntegral $ S.length xs)
  putByteString xs

-- Gets a ByteString and it's length from the compressed format.
unformat :: Get (Int, S.ByteString)
unformat =  (,) <$> (fromIntegral <$> getWord32le)
                <*> (fromIntegral <$> getWord32le >>= getByteString)



--------------------------------------------------------------------------------
-- FFI Bindings

-- In lz4 r71, LZ4_compressBound was changed to a macro. This is identical to
-- that macro so we don't have to go through C land just to get at it.
--
-- NB: MUST *ALWAYS* BE KEPT IN SYNC WITH lz4.h!

--foreign import ccall unsafe "lz4.h LZ4_compressBound"
--  c_LZ4_compressBound :: CInt -> IO CInt
-- | Worst case compression bounds on an input string.
c_LZ4_compressBound :: CInt -- ^ String length
                    -> CInt -- ^ Worst-case size
c_LZ4_compressBound sz = sz + (sz `div` 255) + 16
{-# INLINE c_LZ4_compressBound #-}

-- | Compresses a string.
foreign import ccall unsafe "lz4.h LZ4_compress"
  c_LZ4_compress :: Ptr CChar -- ^ Source
                 -> Ptr Word8 -- ^ Dest
                 -> CInt      -- ^ Input size
                 -> IO CInt   -- ^ Result

-- | Compresses a string with very high compression.
foreign import ccall unsafe "lz4hc.h LZ4_compressHC"
  c_LZ4_compressHC :: Ptr CChar -- ^ Source
                   -> Ptr Word8 -- ^ Dest
                   -> CInt      -- ^ Input size
                   -> IO CInt   -- ^ Result

-- | Decompresses a string. Works for both 'c_LZ4_compress' and
-- 'c_LZ4_compressHC'.
foreign import ccall unsafe "lz4.h LZ4_uncompress"
  c_LZ4_uncompress :: Ptr CChar -- ^ Source
                   -> Ptr Word8 -- ^ Dest
                   -> CInt      -- ^ Size of ORIGINAL INPUT
                   -> IO CInt   -- ^ Result

foreign import ccall unsafe "LZ4F_compressFrameBound"
  c_LZ4F_compressFrameBound :: CSize
                            -> Ptr ()
                            -> CSize

foreign import ccall unsafe "LZ4F_compressFrame"
  c_LZ4F_compressFrame :: Ptr Word8
                       -> CSize
                       -> Ptr Word8
                       -> CSize
                       -> Ptr ()
                       -> IO CSize

foreign import ccall unsafe "LZ4F_isError"
  c_LZ4F_isError :: CSize
                 -> CUInt

foreign import ccall unsafe "LZ4F_createDecompressionContext"
  c_LZ4F_createDecompressionContext :: Ptr LZ4FDecompressionContext
                                    -> CUInt
                                    -> IO CSize

foreign import ccall unsafe "LZ4F_freeDecompressionContext"
  c_LZ4F_freeDecompressionContext :: LZ4FDecompressionContext
                                  -> IO CSize

foreign import ccall unsafe "LZ4F_decompress"
  c_LZ4F_decompress :: LZ4FDecompressionContext
                    -> Ptr Word8
                    -> Ptr CSize
                    -> Ptr Word8
                    -> Ptr CSize
                    -> Ptr ()
                    -> IO CSize
