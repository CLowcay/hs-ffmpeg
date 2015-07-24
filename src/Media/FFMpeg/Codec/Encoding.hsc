{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
 
Description : Bindings to libavcodec
Copyright   : Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavcodec.

-}

module Media.FFMpeg.Codec.Encoding (
) where

#include "ffmpeg.h"

foreign import ccall "avcodec_find_encoder" avcodec_find_encoder :: CInt -> IO (Ptr AVCodec)
foreign import ccall "avcodec_find_encoder_by_name" avcodec_find_encoder_by_name :: CString -> IO (Ptr AVCodec)
foreign import ccall "avcodec_encode_audio2" avcodec_encode_audio2 :: Ptr AVCodecContext -> Ptr AVPacket -> Ptr AVFrame -> Ptr CInt -> IO CInt
foreign import ccall "avcodec_encode_video2" avcodec_encode_video2 :: Ptr AVCodecContext -> Ptr AVPacket -> Ptr AVFrame -> Ptr CInt -> IO CInt
-- foreign import ccall "avcodec_encode_subtitle" avcodec_encode_subtitle :: Ptr AVCodecContext -> Ptr Word8 -> CInt -> Ptr AVSubtitle -> IO CInt
 

