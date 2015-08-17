{- |

Description : Bindings to libswscale
Copyright   : (c) Vasyl Pasternak, 2009
                  Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libswscale.

-}

module Media.FFMpeg.SWScale (
	module Media.FFMpeg.SWScale.Core,
	module Media.FFMpeg.SWScale.Enums,
	module Media.FFMpeg.SWScale.Names,

	libSWScaleVersion
) where

#include "ffmpeg.h"

import Data.Version

import Media.FFMpeg.Internal.Common
import Media.FFMpeg.SWScale.Core
import Media.FFMpeg.SWScale.Enums
import Media.FFMpeg.SWScale.Names

-- | Which version of libswscale are we using?
libSWScaleVersion :: Version
libSWScaleVersion = fromVersionNum #{const LIBSWSCALE_VERSION_INT}

