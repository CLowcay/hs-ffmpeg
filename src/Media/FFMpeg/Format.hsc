{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{- |

Description : Bindings to libavformat
Copyright   : (c) Vasyl Pasternak, 2009
License     : BSD3
Stability   : experimental

Bindings to libavformat.

-}

module Media.FFMpeg.Format (
	module Media.FFMpeg.Format.Core,
	module Media.FFMpeg.Format.Demuxing,
	module Media.FFMpeg.Format.Enums,
	module Media.FFMpeg.Format.Muxing,

	libAVFormatVersion
) where

#include "ffmpeg.h"

import Data.Version

import Media.FFMpeg.Codec
import Media.FFMpeg.Format.Core hiding (AVInputFormat(..), AVOutputFormat(..))
import Media.FFMpeg.Format.Core (AVInputFormat, AVOutputFormat)
import Media.FFMpeg.Format.Demuxing
import Media.FFMpeg.Format.Enums
import Media.FFMpeg.Format.Muxing
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util

-- | Which version of libavformat are we using?
libAVFormatVersion :: Version
libAVFormatVersion = fromVersionNum #{const LIBAVFORMAT_VERSION_INT}

