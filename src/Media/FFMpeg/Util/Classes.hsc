{-# LANGUAGE TypeFamilies #-}
{- |
 
Description : Bindings to libavutil
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavutil.

-}

module Media.FFMpeg.Util.Classes (
	AVCodecContext(..),
	AVFormatContext(..)
) where

#include "ffmpeg.h"

import Control.Monad.IO.Class
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import Media.FFMpeg.Internal.Common

-- | AVCodecContext struct
newtype AVCodecContext = AVCodecContext (ForeignPtr (Ptr (AVCodecContext)))
instance ExternalPointer AVCodecContext where
	type UnderlyingType AVCodecContext = AVCodecContext
	withThis (AVCodecContext fp) action = withThis fp$ \p -> action =<< (liftIO$ peek p)

-- | AVFormatContext struct
newtype AVFormatContext = AVFormatContext (ForeignPtr AVFormatContext)
instance ExternalPointer AVFormatContext where
	type UnderlyingType AVFormatContext = AVFormatContext
	withThis (AVFormatContext fp) = withThis fp

