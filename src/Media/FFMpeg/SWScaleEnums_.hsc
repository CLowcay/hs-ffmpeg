-- -*- haskell -*- 
{-# LANGUAGE ForeignFunctionInterface #-}

{- | Module 'Media.FFMpeg.SWScaleEnums_' implements 
   enumeration from libswscale
   
   (c) 2009 Vasyl Pasternak
 -}


module Media.FFMpeg.SWScaleEnums_
    (
     ScaleAlgorithm (..)
    ) where

#include "ffmpeg.h"
#include "macros.hsc2hs.h"

-- |ScaleAlgorithm Enumeration
#{begin_enum ScaleAlgorithm, SWS_FAST_BILINEAR}
#{add_enum SWS_BICUBIC}
#{add_enum SWS_X}
#{add_enum SWS_POINT}
#{add_enum SWS_AREA}
#{add_enum SWS_BICUBLIN}
#{add_enum SWS_GAUSS}
#{add_enum SWS_SINC}
#{add_enum SWS_LANCZOS}
#{add_enum SWS_SPLINE}
#{end_enum "Eq,Ord,Show"}

