{-# LANGUAGE CPP #-}
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

         -- * FFI functions
       , c_LZ4_compress      -- :: Ptr CChar -> Ptr Word8 -> CInt -> IO CInt
       , c_LZ4_compressHC    -- :: Ptr CChar -> Ptr Word8 -> CInt -> IO CInt
       , c_LZ4_uncompress    -- :: Ptr CChar -> Ptr Word8 -> CInt -> IO CInt
       , c_LZ4_compressBound -- :: CInt -> CInt
       ) where

import Prelude hiding (max)
import Data.Word
import Foreign.Ptr
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as SI
import qualified Data.ByteString.Unsafe as U

import Data.Serialize

#include <lz4.h>
#include <lz4hc.h>


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
              r <- fromIntegral <$> c_LZ4_uncompress cstr p (fromIntegral l)
              return $! if (r <= 0) then 0 else r
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


--------------------------------------------------------------------------------
-- Utilities

-- The compression methods are all identical, so this just abstracts them
compressor :: (Ptr CChar -> Ptr Word8 -> CInt -> IO CInt)
           -> S.ByteString
           -> Maybe S.ByteString
compressor f xs = unsafePerformIO $ do
  U.unsafeUseAsCStringLen xs $ \(cstr,len) -> do
    let len' = fromIntegral len :: CInt
    let max = c_LZ4_compressBound len'
    bs <- SI.createAndTrim (fromIntegral max) $ \output ->
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
unformat = do
  c <-  fromIntegral <$> getWord32le
  l  <- fromIntegral <$> getWord32le
  bs <- getByteString l
  return (c, bs)


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
