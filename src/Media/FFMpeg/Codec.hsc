{- |
 
Description : Bindings to libavcodec
Copyright   : (c) Vasyl Pasternak, 2009
                  Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavcodec.

-}

module Media.FFMpeg.Codec (
	module Media.FFMpeg.Codec.AVPacket,
	module Media.FFMpeg.Codec.AVPacketSideData,
	module Media.FFMpeg.Codec.AVPicture,
	module Media.FFMpeg.Codec.Core,
	module Media.FFMpeg.Codec.Decoding,
	module Media.FFMpeg.Codec.Encoding,
	module Media.FFMpeg.Codec.Enums,
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Error
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Media.FFMpeg.Codec.AVPacket
import Media.FFMpeg.Codec.AVPacketSideData
import Media.FFMpeg.Codec.AVPicture hiding (AVPicture(..), AVSubtitlePicture)
import Media.FFMpeg.Codec.AVPicture (AVPicture)
import Media.FFMpeg.Codec.Core hiding (AVCodec(..))
import Media.FFMpeg.Codec.Core (AVCodec)
import Media.FFMpeg.Codec.Decoding
import Media.FFMpeg.Codec.Encoding
import Media.FFMpeg.Codec.Enums
import Media.FFMpeg.Codec.Names
import Media.FFMpeg.Internal.Common

