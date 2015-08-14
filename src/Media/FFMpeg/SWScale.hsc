{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}

{- |

Description : Bindings to libswscale
Copyright   : (c) Vasyl Pasternak, 2009
License     : BSD3
Stability   : experimental

Bindings to libswscale.

-}

module Media.FFMpeg.SWScale (
	module Media.FFMpeg.SWScale.Enums,
	SwsContext

) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad.Except
import Data.Int
import Data.Monoid
import Data.Version
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Media.FFMpeg.Internal.Common
import Media.FFMpeg.SWScale.Enums
import Media.FFMpeg.Util
import Media.FFMpeg.Util.Classes

-- | Which version of libswscale are we using?
libSWScaleVersion :: Version
libSWScaleVersion = fromVersionNum #{const LIBSWSCALE_VERSION_INT}

foreign import ccall "swscale_configuration" swscale_configuration :: IO CString
foreign import ccall "swscale_license" swscale_license :: IO CString
foreign import ccall "sws_getCoefficients" sws_getCoefficients :: CInt -> IO (Ptr CInt)
foreign import ccall "sws_isSupportedInput" sws_isSupportedInput :: CInt -> IO CInt
foreign import ccall "sws_isSupportedOutput" sws_isSupportedOutput :: CInt -> IO CInt
foreign import ccall "sws_isSupportedEndiannessConversion" sws_isSupportedEndiannessConversion :: CInt -> IO CInt
foreign import ccall "sws_alloc_context" sws_alloc_context :: IO (Ptr SwsContext)
foreign import ccall "sws_init_context" sws_init_context :: Ptr SwsContext -> Ptr SwsFilter -> Ptr SwsFilter -> IO CInt
foreign import ccall "sws_getContext" sws_getContext ::
	CInt -> CInt -> CInt ->
	CInt -> CInt -> CInt ->
	CInt -> Ptr SwsFilter -> Ptr SwsFilter -> Ptr Double -> IO (Ptr SwsContext)
foreign import ccall "&sws_freeContext" psws_freeContext :: FunPtr (Ptr SwsContext -> IO ())
foreign import ccall "sws_scale" sws_scale ::
	Ptr SwsContext -> Ptr (Ptr Word8) -> Ptr CInt ->
	CInt -> CInt -> Ptr (Ptr Word8) -> Ptr CInt -> IO CInt
foreign import ccall "sws_setColorspaceDetails" sws_setColorspaceDetails ::
	Ptr SwsContext -> Ptr CInt -> CInt -> Ptr CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "sws_getColorspaceDetails" sws_getColorspaceDetails ::
	Ptr SwsContext -> Ptr (Ptr CInt) -> Ptr CInt -> Ptr (Ptr CInt) -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "sws_allocVec" sws_allocVec :: CInt -> IO (Ptr SwsVector)
foreign import ccall "sws_getGaussianVec" sws_getGaussianVec :: Double -> Double -> IO (Ptr SwsVector)
foreign import ccall "sws_getConstVec" sws_getConstVec :: Double -> CInt -> IO (Ptr SwsVector)
foreign import ccall "sws_getIdentityVec" sws_getIdentityVec :: IO (Ptr SwsVector)
foreign import ccall "sws_scaleVec" sws_scaleVec :: Ptr SwsVector -> Double -> IO ()
foreign import ccall "sws_normalizeVec" sws_normalizeVec :: Ptr SwsVector -> Double -> IO ()
foreign import ccall "sws_convVec" sws_convVec :: Ptr SwsVector -> Ptr SwsVector -> IO ()
foreign import ccall "sws_addVec" sws_addVec :: Ptr SwsVector -> Ptr SwsVector -> IO ()
foreign import ccall "sws_subVec" sws_subVec :: Ptr SwsVector -> Ptr SwsVector -> IO ()
foreign import ccall "sws_shiftVec" sws_shiftVec :: Ptr SwsVector -> CInt -> IO ()
foreign import ccall "sws_cloneVec" sws_cloneVec :: Ptr SwsVector -> IO (Ptr SwsVector)
foreign import ccall "sws_freeVec" sws_freeVec :: Ptr SwsVector -> IO ()
foreign import ccall "&sws_freeVec" psws_freeVec :: FunPtr (Ptr SwsVector -> IO ())
foreign import ccall "sws_getDefaultFilter" sws_getDefaultFilter :: Float -> Float -> Float -> Float -> Float -> Float -> CInt -> Ptr SwsFilter
foreign import ccall "sws_freeFilter" sws_freeFilter :: Ptr SwsFilter -> IO ()
foreign import ccall "&sws_freeFilter" psws_freeFilter :: FunPtr (Ptr SwsFilter -> IO ())
foreign import ccall "sws_getCachedContext" sws_getCachedContext ::
	Ptr SwsContext ->
	CInt -> CInt -> CInt ->
	CInt -> CInt -> CInt ->
	CInt -> Ptr SwsFilter -> Ptr SwsFilter -> Ptr Double -> IO (Ptr SwsContext)
foreign import ccall "sws_convertPalette8ToPacked32" sws_convertPalette8ToPacked32 :: Ptr Word8 -> Ptr Word8 -> CInt -> Ptr Word8 -> IO ()
foreign import ccall "sws_convertPalette8ToPacked24" sws_convertPalette8ToPacked24 :: Ptr Word8 -> Ptr Word8 -> CInt -> Ptr Word8 -> IO ()
foreign import ccall "sws_get_class" sws_get_class :: IO (Ptr (AVClass SwsContext))

-- | Get SWScale context
getContext :: (MonadIO m, MonadError String m) =>
	(Int, Int, PixelFormat)       -- ^ source (width, height, pixel format)
	-> (Int, Int, PixelFormat)    -- ^ destination (width, height, pixel format)
	-> [ScaleAlgorithm]           -- ^ the algorithm to use and its options
	-> m SwsContext
getContext  (srcW, srcH, srcPf) (dstW, dstH, dstPf) flags = do
	r <- liftIO$ sws_getContext
		(fromIntegral srcW) (fromIntegral srcH) (fromCEnum srcPf)
		(fromIntegral dstW) (fromIntegral dstH) (fromCEnum dstPf)
		(fromCEnum.mconcat$ flags) nullPtr nullPtr nullPtr
	if (r == nullPtr)
		then throwError "getContext: sws_getContext returned a null pointer"
		else liftIO$ (SwsContext . castForeignPtr) <$> newForeignPtr psws_freeContext r

-- | SwsVector struct
newtype SwsVector = SwsVector (ForeignPtr SwsVector)
instance ExternalPointer SwsVector where
	type UnderlyingType SwsVector = SwsVector
	withThis (SwsVector fp) = withThis fp

-- | Create an SwsVector from a list
swsVectorFromList :: MonadIO m => [Double] -> m SwsVector
swsVectorFromList l = liftIO$ do
	let len = length l
	pv <- castPtr <$> av_malloc #{size SwsVector}
	pdata <- castPtr <$> av_malloc (#{size double} * (fromIntegral len))

	forM (l `zip` [0..])$ \(c, i) -> pokeElemOff pdata i c
	#{poke SwsVector, coeff} pv pdata
	#{poke SwsVector, length} pv (fromIntegral len :: CInt)

	SwsVector <$> newForeignPtr psws_freeVec pv

-- | Convert an SwsVector to a list
swsVectorToList :: MonadIO m => SwsVector -> m [Double]
swsVectorToList (SwsVector fp) = withThis fp$ \pv -> liftIO$ do
	pdata <- #{peek SwsVector, coeff} pv
	len <- #{peek SwsVector, length} pv :: IO CInt
	peekArray (fromIntegral len) pdata

-- | SwsFilter struct
data SwsFilter = SwsFilter {
		filter_lumH :: SwsVector,
		filter_lumV :: SwsVector,
		filter_chrH :: SwsVector,
		filter_chrV :: SwsVector
	}

instance ExternalPointer SwsFilter where
	type UnderlyingType SwsFilter = SwsFilter
	withThis f action =
		withThis (filter_lumH f)$ \plumH ->
		withThis (filter_lumV f)$ \plumV ->
		withThis (filter_chrH f)$ \pchrH ->
		withThis (filter_chrV f)$ \pchrV -> do
			pf <- liftIO$ castPtr <$> av_malloc #{size SwsFilter}
			liftIO$ do
				(#{poke SwsFilter, lumH} pf) =<< sws_cloneVec plumH
				(#{poke SwsFilter, lumV} pf) =<< sws_cloneVec plumV
				(#{poke SwsFilter, chrH} pf) =<< sws_cloneVec pchrH
				(#{poke SwsFilter, chrV} pf) =<< sws_cloneVec pchrV

			r <- action pf
			liftIO$ sws_freeFilter pf
			return r

