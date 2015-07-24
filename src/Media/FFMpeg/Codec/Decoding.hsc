{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
 
Description : Bindings to libavcodec
Copyright   : Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavcodec.

-}

module Media.FFMpeg.Codec.Decoding (
) where

#include "ffmpeg.h"

foreign import ccall "avcodec_find_decoder" avcodec_find_decoder :: AVCodecId -> IO (Ptr AVCodec)
foreign import ccall "avcodec_find_decoder_by_name" avcodec_find_decoder_by_name :: CString -> IO (Ptr AVCodec)
foreign import ccall "avcodec_default_get_buffer2" avcodec_default_get_buffer2 :: Ptr AVCodecContext -> Ptr AVFrame -> CInt -> IO CInt
foreign import ccall "avcodec_align_dimensions" avcodec_align_dimensions :: Ptr AVCodecContext -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "avcodec_align_dimensions2" avcodec_align_dimensions2 :: Ptr AVCodecContext -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "avcodec_enum_to_chroma_pos" avcodec_enum_to_chroma_pos :: Ptr CInt -> Ptr CInt -> CInt -> IO CInt
foreign import ccall "avcodec_chroma_pos_to_enum" avcodec_chroma_pos_to_enum :: CInt -> CInt -> IO CInt
foreign import ccall "avcodec_decode_audio4" avcodec_decode_audio4 :: Ptr AVCodecContext -> Ptr AVFrame -> Ptr CInt -> Ptr AVPacket -> IO CInt
foreign import ccall "avcodec_decode_video2" avcodec_decode_video2 :: Ptr AVCodecContext -> Ptr AVFrame -> Ptr CInt -> Ptr AVPacket -> IO CInt
-- foreign import ccall "avcodec_decode_subtitle2" avcodec_decode_subtitle2 :: Ptr AVCodecContext -> Ptr AVSubtitle -> Ptr CInt -> Ptr AVPacket -> IO CInt

