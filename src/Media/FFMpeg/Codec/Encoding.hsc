{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
 
Description : Bindings to libavcodec
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavcodec.

-}

module Media.FFMpeg.Codec.Encoding (
) where

#include "ffmpeg.h"

import Control.Monad.IO.Class
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Media.FFMpeg.Codec.AVPacket
import Media.FFMpeg.Codec.Core
import Media.FFMpeg.Codec.Enums
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util.AVFrame

foreign import ccall "avcodec_find_encoder" avcodec_find_encoder :: CInt -> IO (Ptr AVCodec)
foreign import ccall "avcodec_find_encoder_by_name" avcodec_find_encoder_by_name :: CString -> IO (Ptr AVCodec)
foreign import ccall "avcodec_encode_audio2" avcodec_encode_audio2 :: Ptr AVCodecContext -> Ptr AVPacket -> Ptr AVFrame -> Ptr CInt -> IO CInt
foreign import ccall "avcodec_encode_video2" avcodec_encode_video2 :: Ptr AVCodecContext -> Ptr AVPacket -> Ptr AVFrame -> Ptr CInt -> IO CInt
-- foreign import ccall "avcodec_encode_subtitle" avcodec_encode_subtitle :: Ptr AVCodecContext -> Ptr Word8 -> CInt -> Ptr AVSubtitle -> IO CInt
 
-- Find an encoder for a given AVCodecId
findEncoder :: MonadIO m => AVCodecId -> m (Maybe AVCodec)
findEncoder cid = liftIO$ do
	r <- avcodec_find_encoder (fromCEnum cid)
	if r == nullPtr then return Nothing else return.Just$ (AVCodec r)

-- Find the encoder with the given name
findEncoderByName :: MonadIO m => String -> m (Maybe AVCodec)
findEncoderByName s = liftIO.withCString s$ \ps -> do
	r <- avcodec_find_encoder_by_name ps
	if r == nullPtr then return Nothing else return.Just$ (AVCodec r)

