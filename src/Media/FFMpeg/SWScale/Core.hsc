{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{- |
 
Description : Bindings to libswscale
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libswscale.

-}

module Media.FFMpeg.SWScale.Core (
	SwsContext,

	SwsVector,
	swsVectorFromList,
	swsVectorToList,
	SwsFilter,

	swscaleConfiguration,
	swscaleLicense,
	isSupportedInput,
	isSupportedOutput,
	isSupportedEndiannessConversion,

	SwsFlags,
	encodeSwsFlags,

	getCoefficients,
	newSwsContext,
	getSwsContext,
	scale,
	convertPalette8ToPacked32,
	convertPalette8ToPacked24,
	ColorSpaceDetails,
	tableToList,
	setColorSpaceDetails,
	getColorSpaceDetails,

	newGaussianVector,
	newConstantVector,
	newIdentityVector,
	scaleVector,
	normalizeVector,
	convolveVector,
	addVector,
	subVector,
	shiftVector,
	cloneVector,
	getDefaultFilter
) where

import Control.Applicative
import Control.Monad.Except
import Data.Bits
import Data.ByteString.Unsafe
import Data.Monoid
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString as B
import System.IO.Unsafe

import Media.FFMpeg.Codec.AVPicture
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.SWScale.Enums
import Media.FFMpeg.Util

#include "ffmpeg.h"

foreign import ccall "swscale_configuration" swscale_configuration :: IO CString
foreign import ccall "swscale_license" swscale_license :: IO CString
foreign import ccall "sws_getCoefficients" sws_getCoefficients :: CInt -> IO (Ptr CInt)
foreign import ccall "sws_isSupportedInput" sws_isSupportedInput :: CInt -> CInt
foreign import ccall "sws_isSupportedOutput" sws_isSupportedOutput :: CInt -> CInt
foreign import ccall "sws_isSupportedEndiannessConversion" sws_isSupportedEndiannessConversion :: CInt -> CInt
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
foreign import ccall "sws_getDefaultFilter" sws_getDefaultFilter :: Float -> Float -> Float -> Float -> Float -> Float -> CInt -> IO (Ptr SwsFilter)
foreign import ccall "sws_freeFilter" sws_freeFilter :: Ptr SwsFilter -> IO ()
foreign import ccall "&sws_freeFilter" psws_freeFilter :: FunPtr (Ptr SwsFilter -> IO ())
foreign import ccall "sws_getCachedContext" sws_getCachedContext ::
	Ptr SwsContext ->
	CInt -> CInt -> CInt ->
	CInt -> CInt -> CInt ->
	CInt -> Ptr SwsFilter -> Ptr SwsFilter -> Ptr Double -> IO (Ptr SwsContext)
foreign import ccall "sws_convertPalette8ToPacked32" sws_convertPalette8ToPacked32 :: Ptr Word8 -> Ptr Word8 -> CInt -> Ptr Word8 -> IO ()
foreign import ccall "sws_convertPalette8ToPacked24" sws_convertPalette8ToPacked24 :: Ptr Word8 -> Ptr Word8 -> CInt -> Ptr Word8 -> IO ()
foreign import ccall "sws_get_class" sws_get_class :: Ptr (AVClass SwsContext)

-- | SwsContext struct
newtype SwsContext = SwsContext (ForeignPtr SwsContext)
instance ExternalPointer SwsContext where
	type UnderlyingType SwsContext = SwsContext
	withThis (SwsContext fp) = withThis fp
instance ReflectClass SwsContext where
	withClass ctx action = withThis ctx$ \pctx ->
		action =<< (liftIO$ avClassFromPtr <$> peek (castPtr pctx))
instance HasClass SwsContext where
	getClass = avClassFromPtr sws_get_class

-- | SwsVector struct
newtype SwsVector = SwsVector (ForeignPtr SwsVector)
instance ExternalPointer SwsVector where
	type UnderlyingType SwsVector = SwsVector
	withThis (SwsVector fp) = withThis fp

-- | Copy a vector from a pointer.  Frees the original vector.
swsVectorFromPtr :: MonadIO m => Ptr SwsVector -> m SwsVector
swsVectorFromPtr pv = liftIO$ do
	pdata <- #{peek SwsVector, coeff} pv :: IO (Ptr Double)
	len <- #{peek SwsVector, length} pv :: IO CInt
	let cBytes = #{size double} * (fromIntegral len)

	pv' <- castPtr <$> av_malloc #{size SwsVector}
	pdata' <- castPtr <$> av_malloc cBytes
	#{poke SwsVector, coeff} pv pdata
	#{poke SwsVector, length} pv len
	moveBytes pdata' pdata (fromIntegral cBytes)

	sws_freeVec pv

	SwsVector <$> newForeignPtr psws_freeVec pv'

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

-- | Get the libswscale configuration string
swscaleConfiguration :: String
swscaleConfiguration =
	-- safe because swscale_configuration returns const char*
	unsafePerformIO$ peekCString =<< swscale_configuration

-- | Get the libswscale license string
swscaleLicense :: String
swscaleLicense =
	-- safe because swscale_license returns const char*
	unsafePerformIO$ peekCString =<< swscale_license

-- | Determine if a pixel format is supported for input
isSupportedInput :: AVPixelFormat -> Bool
isSupportedInput pf = sws_isSupportedInput (fromCEnum pf) /= 0

-- | Determine if a pixel format is supported for output
isSupportedOutput :: AVPixelFormat -> Bool
isSupportedOutput pf = sws_isSupportedOutput (fromCEnum pf) /= 0

-- | Determine if a pixel format is supported for endianness conversion
isSupportedEndiannessConversion :: AVPixelFormat -> Bool
isSupportedEndiannessConversion pf =
	sws_isSupportedEndiannessConversion (fromCEnum pf) /= 0

-- | Get the coefficient table associated with a color space
getCoefficients :: SwsColorSpace -> (CInt, CInt, CInt, CInt)
getCoefficients cs = unsafePerformIO$ do
	-- safe because sws_getCoefficients returns a const pointer
	p <- sws_getCoefficients (fromCEnum cs)
	if p == nullPtr then error "sws_getCoefficients returned a null pointer.  This cannot happen"
	else tableFromList <$> peekArray 4 p

-- | Allocate a new SwsContext with the given source and destination filters
newSwsContext :: (MonadIO m, MonadError HSFFError m) =>
	SwsFilter        -- ^ source filter
	-> SwsFilter     -- ^ destination filter
	-> m SwsContext
newSwsContext srcFilter dstFilter = do
	pctx <- liftIO$ sws_alloc_context

	when (pctx == nullPtr)$ throwError$
		mkNullPointerError "newSwsContext" "sws_alloc_context"

	withThis srcFilter$ \psrc ->
		withThis dstFilter$ \pdst ->
			liftIO$ sws_init_context pctx psrc pdst
	
	liftIO$ SwsContext <$> newForeignPtr psws_freeContext pctx

-- | Initialise a new SwsContext with the given options
getSwsContext :: (MonadIO m, MonadError HSFFError m) =>
	(Int, Int, AVPixelFormat)        -- ^ Source (width, height, format)
	-> (Int, Int, AVPixelFormat)     -- ^ Destination (width, height, format)
	-> SwsFlags                      -- ^ Use encodeSwsFlags to produce this value
	-> Maybe SwsFilter               -- ^ Source filter
	-> Maybe SwsFilter               -- ^ Destination filter
	-> Maybe (Double, Double)        -- ^ Optional parameters for the scaling algorithm
	-> m SwsContext
getSwsContext
	(srcW, srcH, srcPF) (dstW, dstH, dstPF)
	flags msrcFilter mdstFilter mparams = do
		p <-
			withOrNull msrcFilter$ \psrcFilter ->
			withOrNull mdstFilter$ \pdstFilter ->
			liftIO.withParams$ \pparams -> sws_getContext
				(fromIntegral srcW) (fromIntegral srcH) (fromCEnum srcPF)
				(fromIntegral dstW) (fromIntegral dstH) (fromCEnum dstPF)
				(fromCEnum flags) psrcFilter pdstFilter pparams

		if p == nullPtr then throwError$
			mkNullPointerError "getSwsContext" "sws_getContext"
		else liftIO$ SwsContext <$> newForeignPtr psws_freeContext p

	where
		withParams action = case mparams of
			Nothing -> action nullPtr
			Just (x, y) -> withArray [x, y] action

-- | The flags field for a SwsContext
newtype SwsFlags = SwsFlags CInt deriving (Eq, CEnum)

-- | Encode information about the scaling algorithm to use into SwsFlags
encodeSwsFlags :: (ScaleAlgorithm, [ScaleFlag], ChromaDrop) -> SwsFlags
encodeSwsFlags (alg, flags, cdrop) = SwsFlags$
	(fromCEnum alg) .|. (fromCEnum (mconcat flags)) .|. case cdrop of
		SwsSrcVChrDrop0 -> 0
		SwsSrcVChrDrop1 -> 1 `unsafeShiftL` #{const SWS_SRC_V_CHR_DROP_SHIFT}
		SwsSrcVChrDrop2 -> 2 `unsafeShiftL` #{const SWS_SRC_V_CHR_DROP_SHIFT}
		SwsSrcVChrDrop3 -> 3 `unsafeShiftL` #{const SWS_SRC_V_CHR_DROP_SHIFT}

-- | scale an image slice
scale :: (MonadIO m, HasAVPicture src, HasAVPicture dst) =>
	SwsContext   -- ^ Scaling context
	-> src       -- ^ Source picture
	-> Int       -- ^ Source slice y
	-> Int       -- ^ Source slice height
	-> dst       -- ^ Destination picture
	-> m Int     -- ^ The number of destination lines
scale ctx src y height dst =
	withThis ctx$ \pctx ->
	withAVPicturePtr src$ \psrc ->
	withAVPicturePtr dst$ \pdst -> liftIO$ do
		srcSlice <- #{peek AVPicture, data} psrc
		srcStride <- #{peek AVPicture, linesize} psrc
		dstSlice <- #{peek AVPicture, data} pdst
		dstStride <- #{peek AVPicture, linesize} pdst
		fromIntegral <$> sws_scale pctx srcSlice srcStride
			(fromIntegral y) (fromIntegral height) dstSlice dstStride

-- | Convert an 8 bit palettized to a 32 bit pixel format
convertPalette8ToPacked32 :: MonadIO m =>
	Ptr Word8 -> Ptr Word8 -> Int -> B.ByteString -> m ()
convertPalette8ToPacked32 src dst num_pixels palette =
	liftIO.unsafeUseAsCString palette$ \ppal ->
		sws_convertPalette8ToPacked32 src dst (fromIntegral num_pixels) (castPtr ppal)

-- | Convert an 8 bit palettized to a 24 bit pixel format
convertPalette8ToPacked24 :: MonadIO m =>
	Ptr Word8 -> Ptr Word8 -> Int -> B.ByteString -> m ()
convertPalette8ToPacked24 src dst num_pixels palette =
	liftIO.unsafeUseAsCString palette$ \ppal ->
		sws_convertPalette8ToPacked24 src dst (fromIntegral num_pixels) (castPtr ppal)

-- | Description of a color space
data ColorSpaceDetails = ColorSpaceDetails {
		csd_inv_table :: (CInt, CInt, CInt, CInt),
		csd_srcRange :: CInt,
		csd_table :: (CInt, CInt, CInt, CInt),
		csd_dstRange :: CInt,
		csd_brightness :: CInt,
		csd_contrast :: CInt,
		csd_saturation :: CInt
	}

tableFromList :: [CInt] -> (CInt, CInt, CInt, CInt)
tableFromList [a, b, c, d] = (a, b, c, d)
tableFromList _ = error "Conversion table was the wrong size.  This cannot happen"

-- | Convert the color space tables to lists
tableToList :: (CInt, CInt, CInt, CInt) -> [CInt]
tableToList (a, b, c, d) = [a, b, c, d]

-- | Set SwsContext fields relating to the colorspace
setColorSpaceDetails :: (MonadIO m, MonadError HSFFError m) =>
	SwsContext -> ColorSpaceDetails -> m ()
setColorSpaceDetails ctx csd =
	withThis ctx$ \pctx -> do
		r <- liftIO$
			withArray (tableToList.csd_inv_table$ csd)$ \pinv ->
			withArray (tableToList.csd_table$ csd)$ \ptab ->
				sws_setColorspaceDetails pctx
					pinv (csd_srcRange csd) ptab (csd_dstRange csd)
					(csd_brightness csd) (csd_contrast csd) (csd_saturation csd)
		when (r == -1)$ throwError$
			mkError r "setColorSpaceDetails" "sws_setColorspaceDetails"

-- | Get SwsContext fields relating to the color space
getColorSpaceDetails :: (MonadIO m, MonadError HSFFError m) =>
	SwsContext -> m ColorSpaceDetails
getColorSpaceDetails ctx = do
	x <- withThis ctx$ \pctx -> liftIO$
		alloca$ \pinv_table ->
		alloca$ \psrcRange ->
		alloca$ \ptable ->
		alloca$ \pdstRange ->
		alloca$ \pbrightness ->
		alloca$ \pcontrast ->
		alloca$ \psaturation -> do
			r <- sws_getColorspaceDetails pctx
				pinv_table psrcRange
				ptable pdstRange
				pbrightness pcontrast psaturation
			if r == -1 then return$ Left r else do
				_inv_table <- peekArray 4 =<< peek pinv_table
				_srcRange <- peek psrcRange
				_table <- peekArray 4 =<< peek ptable
				_dstRange <- peek pdstRange
				_brightness <- peek pbrightness
				_contrast <- peek pcontrast
				_saturation <- peek psaturation

				return.Right$ ColorSpaceDetails
					(tableFromList _inv_table) _srcRange
					(tableFromList _table) _dstRange
					_brightness _contrast _saturation

	case x of
		Left e -> throwError$
			mkError e "getColorSpaceDetails" "sws_getColorspaceDetails"
		Right r -> return r

-- | Allocate a new Guassian vector
newGaussianVector :: MonadIO m =>
	Double           -- ^ variance
	-> Double        -- ^ quality
	-> m SwsVector
newGaussianVector variance quality =
	swsVectorFromPtr =<< (liftIO$ sws_getGaussianVec variance quality)

-- | Create a new constant vector
newConstantVector :: MonadIO m =>
	Double           -- ^ constant
	-> Int           -- ^ length of vector
	-> m SwsVector
newConstantVector c len =
	swsVectorFromPtr =<< (liftIO$ sws_getConstVec c (fromIntegral len))

-- | Create an "identity" vector
newIdentityVector :: MonadIO m => m SwsVector
newIdentityVector = swsVectorFromPtr =<< liftIO sws_getIdentityVec

-- | Scale a vector by a scalar value
scaleVector :: MonadIO m => SwsVector -> Double -> m ()
scaleVector v scalar = withThis v$ \pv -> liftIO$ sws_scaleVec pv scalar

-- | Normalise a vector so all elements sum to a particular value
normalizeVector :: MonadIO m =>
	SwsVector    -- vector to normalise
	-> Double    -- value that the vector should sum to
	-> m ()
normalizeVector v height = withThis v$ \pv -> liftIO$ sws_normalizeVec pv height

-- | Convolve two vectors.  The first vector is updated to contain the result.
convolveVector :: MonadIO m => SwsVector -> SwsVector -> m ()
convolveVector vd vs =
	withThis vd$ \pvd ->
	withThis vs$ \pvs -> liftIO$ sws_convVec pvd pvs

-- | Add two vectors.  The first vector is updated to contain the result.
addVector :: MonadIO m => SwsVector -> SwsVector -> m ()
addVector vd vs =
	withThis vd$ \pvd ->
	withThis vs$ \pvs -> liftIO$ sws_convVec pvd pvs

-- | Subtract two vectors.  The first vector is updated to contain the result.
subVector :: MonadIO m => SwsVector -> SwsVector -> m ()
subVector vd vs =
	withThis vd$ \pvd ->
	withThis vs$ \pvs -> liftIO$ sws_convVec pvd pvs

-- | Shift a vector
shiftVector :: MonadIO m => SwsVector -> Int -> m ()
shiftVector v shift = withThis v$ \pv -> liftIO$
	sws_shiftVec pv (fromIntegral shift)

-- | Copy a vector
cloneVector :: MonadIO m => SwsVector -> m SwsVector
cloneVector v = swsVectorFromPtr =<< withThis v (liftIO.sws_cloneVec)

-- | Construct a filter with the given characteristics
getDefaultFilter :: (MonadIO m, MonadError HSFFError m) =>
	Float      -- ^ lumaGBlur
	-> Float   -- ^ chromaGBlur
	-> Float   -- ^ lumaSharpen
	-> Float   -- ^ chromaSharpen
	-> Float   -- ^ chromaHShift
	-> Float   -- ^ chromaVShift
	-> m SwsFilter
getDefaultFilter lumaGBlur chromaGBlur lumaSharpen chromaSharpen chromaHShift chromaVShift = do
	pf <- liftIO$ sws_getDefaultFilter
		lumaGBlur chromaGBlur lumaSharpen
		chromaSharpen chromaHShift chromaVShift 0
	
	if pf == nullPtr then throwError$
		mkNullPointerError "getDefaultFilter" "sws_getDefaultFilter"
	else do
		lumH <- swsVectorFromPtr =<< (liftIO$ #{peek SwsFilter, lumH} pf)
		lumV <- swsVectorFromPtr =<< (liftIO$ #{peek SwsFilter, lumV} pf)
		chrH <- swsVectorFromPtr =<< (liftIO$ #{peek SwsFilter, chrH} pf)
		chrV <- swsVectorFromPtr =<< (liftIO$ #{peek SwsFilter, chrV} pf)
		liftIO$ av_free pf

		return$ SwsFilter lumH lumV chrH chrV

