{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{- |
 
Description : Bindings to libavformat
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavformat.

-}

module Media.FFMpeg.Format.Demuxing (
) where

#include "ffmpeg.h"

import Data.Int
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Media.FFMpeg.Codec
import Media.FFMpeg.Format.Core
import Media.FFMpeg.Internal.Common

foreign import ccall "av_find_input_format" av_find_input_format :: CString -> IO (Ptr AVInputFormat)
foreign import ccall "avformat_open_input" avformat_open_input :: Ptr (Ptr AVFormatContext) -> CString -> Ptr AVInputFormat -> Ptr (Ptr ()) -> IO CInt
foreign import ccall "avformat_find_stream_info" avformat_find_stream_info :: Ptr AVFormatContext -> Ptr (Ptr ()) -> IO CInt
foreign import ccall "av_find_program_from_stream" av_find_program_from_stream :: Ptr AVFormatContext -> Ptr AVProgram -> CInt -> IO (Ptr AVProgram)
foreign import ccall "av_find_best_stream" av_find_best_stream :: Ptr AVFormatContext -> CInt -> CInt -> CInt -> Ptr (Ptr AVCodec) -> CInt -> IO CInt
foreign import ccall "av_read_frame" av_read_frame :: Ptr AVFormatContext -> Ptr AVPacket -> IO CInt
foreign import ccall "av_seek_frame" av_seek_frame :: Ptr AVFormatContext -> CInt -> Int64 -> CInt -> IO CInt
foreign import ccall "avformat_seek_file" avformat_seek_file :: Ptr AVFormatContext -> CInt -> Int64 -> Int64 -> Int64 -> CInt -> IO CInt
foreign import ccall "avformat_flush" avformat_flush :: Ptr AVFormatContext -> IO CInt
foreign import ccall "av_read_play" av_read_play :: Ptr AVFormatContext -> IO CInt
foreign import ccall "av_read_pause" av_read_pause :: Ptr AVFormatContext -> IO CInt
foreign import ccall "avformat_close_input" avformat_close_input :: Ptr (Ptr AVFormatContext) -> IO ()
 
