{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |

Description : AVPacket and related functions
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavcodec.

-}

module Media.FFMpeg.Codec.AVPacket (
	av_init_packet,
	av_new_packet,
	av_shrink_packet,
	av_grow_packet,
	av_packet_from_data,
	av_dup_packet,
	av_copy_packet,
	av_copy_packet_side_data,
	av_free_packet,
	av_packet_new_side_data,
	av_packet_shrink_side_data,
	av_packet_get_side_data,
	av_packet_merge_side_data,
	av_packet_split_side_data,
	av_packet_side_data_name,
	av_packet_pack_dictionary,
	av_packet_unpack_dictionary,
	av_packet_free_side_data,
	av_packet_ref,
	av_packet_unref,
	av_packet_move_ref,
	av_packet_copy_props,
	av_packet_rescale_ts,

	AVPacket,

	packetGetPTS,
	packetGetDTS,
	packetGetData,
	packetGetSize,
	packetGetStreamIndex,
	packetFlags,
	packetGetDuration,
	packetGetPos,
	packetGetConvergenceDuration,
	packetSetPTS,
	packetSetDTS,
	packetSetStreamIndex,
	packetSetFlags,
	packetSetDuration,
	packetSetConvergenceDuration,

	mkAVPacket,
	packetAlloc,
	packetFree,
	copyPacket,
	copyPacketSideData,
	packetCopyProps,
	AVPacketSideDataPayload(peekPayload, pokePayload),
	peekAVPacketSideDataPtrType,
	getPackedDictSize,
	getPackedDict,
	packDict,
	packetSetSideData,
	packetGetSideData,
	packetFreeSideDataType,
	packetFreeSideData,
	packetSideDataName,
	packetRescaleTS,
	newPacketFromBuffer
) where

#include "ffmpeg.h"
#include "string.h"

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Data.Int
import Data.Ratio
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
import qualified Data.ByteString.Unsafe as B
import System.IO.Unsafe

import Media.FFMpeg.Codec.AVPacketSideData
import Media.FFMpeg.Codec.Enums
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util

foreign import ccall "av_init_packet" av_init_packet :: Ptr AVPacket -> IO ()
foreign import ccall "av_new_packet" av_new_packet :: Ptr AVPacket -> CInt -> IO CInt
foreign import ccall "av_shrink_packet" av_shrink_packet :: Ptr AVPacket -> CInt -> IO ()
foreign import ccall "av_grow_packet" av_grow_packet :: Ptr AVPacket -> CInt -> CInt
foreign import ccall "av_packet_from_data" av_packet_from_data :: Ptr AVPacket -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall "av_dup_packet" av_dup_packet :: Ptr AVPacket -> IO CInt
foreign import ccall "av_copy_packet" av_copy_packet :: Ptr AVPacket -> Ptr AVPacket -> IO CInt
foreign import ccall "av_copy_packet_side_data" av_copy_packet_side_data :: Ptr AVPacket -> Ptr AVPacket -> IO CInt
foreign import ccall "av_free_packet" av_free_packet :: Ptr AVPacket -> IO ()
foreign import ccall "av_packet_new_side_data" av_packet_new_side_data :: Ptr AVPacket -> CInt -> CInt -> IO (Ptr Word8)
foreign import ccall "av_packet_shrink_side_data" av_packet_shrink_side_data :: Ptr AVPacket -> CInt -> CInt -> IO CInt
foreign import ccall "av_packet_get_side_data" av_packet_get_side_data :: Ptr AVPacket -> CInt -> Ptr CInt -> IO (Ptr Word8)
foreign import ccall "av_packet_merge_side_data" av_packet_merge_side_data :: Ptr AVPacket -> IO CInt
foreign import ccall "av_packet_split_side_data" av_packet_split_side_data :: Ptr AVPacket -> IO CInt
foreign import ccall "av_packet_side_data_name" av_packet_side_data_name :: CInt -> CString
foreign import ccall "av_packet_pack_dictionary" av_packet_pack_dictionary :: Ptr () -> Ptr CInt -> IO (Ptr Word8)
foreign import ccall "av_packet_unpack_dictionary" av_packet_unpack_dictionary :: Ptr Word8 -> CInt -> Ptr (Ptr ()) -> IO CInt
foreign import ccall "av_packet_free_side_data" av_packet_free_side_data :: Ptr AVPacket -> IO ()
foreign import ccall "av_packet_ref" av_packet_ref :: Ptr AVPacket -> Ptr AVPacket -> IO CInt
foreign import ccall "av_packet_unref" av_packet_unref :: Ptr AVPacket -> IO ()
foreign import ccall "av_packet_move_ref" av_packet_move_ref :: Ptr AVPacket -> Ptr AVPacket -> IO ()
foreign import ccall "av_packet_copy_props" av_packet_copy_props :: Ptr AVPacket -> Ptr AVPacket -> IO CInt

foreign import ccall "b_av_packet_rescale_ts" av_packet_rescale_ts :: Ptr AVPacket -> CInt -> CInt -> CInt -> CInt -> IO ()
foreign import ccall "&b_free_packet" pav_free_packet :: FunPtr (Ptr () -> IO ())

-- | AVPacket struct
newtype AVPacket = AVPacket (ForeignPtr AVPacket)

instance ExternalPointer AVPacket where
	withThis (AVPacket f) io = withForeignPtr f (io.castPtr)

getInt :: CInt -> Int
getInt = fromIntegral

setInt :: Int -> CInt
setInt = fromIntegral

-- | Get the presentation timestamp from a packet
packetGetPTS :: (MonadIO m) => AVPacket -> m AVTimestamp
packetGetPTS pkt = liftIO.(AVTimestamp <$>).withThis pkt$ #{peek AVPacket, pts}

-- | Get the decompression timestamp from a packet
packetGetDTS :: MonadIO m => AVPacket -> m AVTimestamp
packetGetDTS pkt = liftIO.(AVTimestamp <$>).withThis pkt$ #{peek AVPacket, dts}

-- | Get a pointer to the data of a packet
packetGetData :: MonadIO m => AVPacket -> m (Ptr Word8)
packetGetData pkt = liftIO.withThis pkt$ #{peek AVPacket, data}

-- | Get the data size of a packet
packetGetSize :: MonadIO m => AVPacket -> m Int
packetGetSize pkt = liftIO.(getInt <$>).withThis pkt$ #{peek AVPacket, size}

-- | Get the stream_index from a packet
packetGetStreamIndex :: MonadIO m => AVPacket -> m Int
packetGetStreamIndex pkt = liftIO.(getInt <$>).withThis pkt$ #{peek AVPacket, stream_index}

-- | Get the flags field from a packet
packetFlags :: MonadIO m => AVPacket -> m AVPacketFlag
packetFlags pkt = liftIO.(toCEnum <$>).withThis pkt$ #{peek AVPacket, flags}

-- | Get the duration of a packet
packetGetDuration :: MonadIO m => AVPacket -> m Int
packetGetDuration pkt = liftIO.(getInt <$>).withThis pkt$ #{peek AVPacket, duration}

-- | Get the byte position in the file of a packet
packetGetPos :: MonadIO m => AVPacket -> m Int64
packetGetPos pkt = liftIO.withThis pkt$ #{peek AVPacket, pos}

-- | Get the convergence_duration of a packet
packetGetConvergenceDuration :: MonadIO m => AVPacket -> m AVTimestamp
packetGetConvergenceDuration pkt = liftIO.(AVTimestamp <$>).withThis pkt$ #{peek AVPacket, convergence_duration}

-- | Set the presentation timestamp of a packet
packetSetPTS :: (MonadIO m) => AVPacket -> AVTimestamp -> m ()
packetSetPTS pkt (AVTimestamp x) = liftIO.withThis pkt$ \ptr -> #{poke AVPacket, pts} ptr x

-- | Set the decompression timestamp of a packet
packetSetDTS :: MonadIO m => AVPacket -> AVTimestamp -> m ()
packetSetDTS pkt (AVTimestamp x) = liftIO.withThis pkt$ \ptr -> #{poke AVPacket, dts} ptr x

-- | Set the stream_index of a packet
packetSetStreamIndex :: MonadIO m => AVPacket -> Int -> m ()
packetSetStreamIndex pkt v = liftIO.withThis pkt$ \ptr -> #{poke AVPacket, stream_index} ptr (setInt v)

-- | Set the flags field of a packet
packetSetFlags :: MonadIO m => AVPacket -> AVPacketFlag -> m ()
packetSetFlags pkt fs = liftIO.withThis pkt$ \ptr -> #{poke AVPacket, flags} ptr (fromCEnum fs)

-- | Set the duration of a packet
packetSetDuration :: MonadIO m => AVPacket -> Int -> m ()
packetSetDuration pkt v = liftIO.withThis pkt$ \ptr -> #{poke AVPacket, duration} ptr (setInt v)

-- | Set the convergence_duration of a packet
packetSetConvergenceDuration :: MonadIO m => AVPacket -> AVTimestamp -> m ()
packetSetConvergenceDuration pkt (AVTimestamp x) = liftIO.withThis pkt$ \ptr -> #{poke AVPacket, convergence_duration} ptr x

-- | Create a new AVPacket which will be freed by the garbage collector
mkAVPacket :: MonadIO m => m AVPacket
mkAVPacket = liftIO$ do
	fp <- mallocForeignPtrBytes #{size AVPacket}
	addForeignPtrFinalizer pav_free_packet fp
	return.(AVPacket).castForeignPtr$ fp

-- | Initialise a packet, allocating a new buffer.  If the packet already has a
-- buffer, or side data, it will be freed.
packetAlloc :: (MonadIO m, MonadError String m) => AVPacket -> Int -> m ()
packetAlloc pkt size = do
	packetFree pkt
	r <- liftIO.withThis pkt$ \ptr -> av_new_packet ptr (fromIntegral size)
	when (r /= 0)$
		throwError$ "packetAlloc: av_new_packet failed with error code " ++ (show r)

-- | Manually free the data associated with a packet, including side data
packetFree :: MonadIO m => AVPacket -> m ()
packetFree pkt = liftIO.withThis pkt$ \ptr -> av_free_packet ptr

-- | Copy all the data associated with one packet to another
copyPacket :: (MonadIO m, MonadError String m) =>
	AVPacket     -- ^ Destination
	-> AVPacket  -- ^ Source
	-> m ()
copyPacket dst src = do
	r <- liftIO$
		withThis dst$ \pd ->
		withThis src$ \ps -> av_copy_packet pd ps
	when (r < 0)$ throwError$ "copyPacket: failed with error code " ++ (show r)

-- | Copy the side data from one packet to another
copyPacketSideData :: (MonadIO m, MonadError String m) =>
	AVPacket     -- Destination
	-> AVPacket  -- Source
	-> m ()
copyPacketSideData dst src = do
	r <- liftIO$
		withThis dst$ \pd ->
		withThis src$ \ps -> av_copy_packet_side_data pd ps
	when (r < 0)$ throwError$ "copyPacketSideData: failed with error code " ++ (show r)

-- | Copy packet properties from one packet to another.  Includes side data and
-- metadata fields, but not the buffers
packetCopyProps :: (MonadIO m, MonadError String m) =>
	AVPacket     -- ^ Destination
	-> AVPacket  -- ^ Source
	-> m ()
packetCopyProps dst src = do
	r <- liftIO$
		withThis dst$ \pd ->
		withThis src$ \ps -> av_packet_copy_props pd ps
	when (r < 0)$ throwError$ "packetCopyProps: failed with error code " ++ (show r)

-- | Class of valid AVPacketSideData types
class AVPacketSideDataPayload a where
	peekAVPacketSideDataPtr :: Ptr AVPacket -> IO (Ptr a, Int)
	getPayloadType :: a -> AVPacketSideDataType
	getPayloadSize :: a -> IO Int
	peekPayload :: Ptr a -> Int -> IO a
	pokePayload :: Ptr a -> a -> IO ()

peekAVPacketSideDataPtrType :: AVPacketSideDataType -> Ptr AVPacket -> IO (Ptr Word8, Int)
peekAVPacketSideDataPtrType sdType ptr =
	alloca $ \psize -> do
		pdata <- av_packet_get_side_data ptr (fromCEnum sdType) psize
		size <- fromIntegral <$> peek psize
		return (pdata, size)

getPackedDictSize :: AVDictionary -> IO Int
getPackedDictSize dict =
	withThis dict$ \pd ->
	alloca$ \psize -> do
		x <- av_packet_pack_dictionary pd psize
		av_free x
		fromIntegral <$> peek psize

getPackedDict :: Ptr Word8 -> Int -> IO AVDictionary
getPackedDict ptr size = do
	dict <- newAVDictionary
	r <- withThis dict$ av_packet_unpack_dictionary ptr (fromIntegral size)
	if r < 0
		then fail$ "reading dictionary from packet side data failed with error code: " ++ (show r)
		else return dict

packDict :: Ptr AVDictionary -> AVDictionary -> IO ()
packDict ptr dict = 
	withThis dict$ \pd ->
	alloca$ \psize -> do
		x <- av_packet_pack_dictionary pd psize
		size <- fromIntegral <$> peek psize
		copyArray (castPtr ptr) x size 

instance AVPacketSideDataPayload AVPacketSideDataPalette where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_palette
	getPayloadType _ = av_pkt_data_palette
	getPayloadSize (AVPacketSideDataPalette s) = return$ B.length s
	peekPayload ptr size = AVPacketSideDataPalette <$> B.packCStringLen (castPtr ptr, size)
	pokePayload ptr (AVPacketSideDataPalette s) =
		B.unsafeUseAsCString s$ \ps -> copyArray (castPtr ptr) ps (B.length s)
instance AVPacketSideDataPayload AVPacketSideDataNewExtradata where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_new_extradata
	getPayloadType _ = av_pkt_data_new_extradata
	getPayloadSize (AVPacketSideDataNewExtradata s) = return$ B.length s
	peekPayload ptr size = AVPacketSideDataNewExtradata <$> B.packCStringLen (castPtr ptr, size)
	pokePayload ptr (AVPacketSideDataNewExtradata s) =
		B.unsafeUseAsCString s$ \ps -> copyArray (castPtr ptr) ps (B.length s)
instance AVPacketSideDataPayload AVPacketSideDataParamChange where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_param_change
	getPayloadType _ = av_pkt_data_param_change
	getPayloadSize x = return$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVPacketSideDataPayload AVPacketSideDataH263MbInfo where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_h263_mb_info
	getPayloadType _ = av_pkt_data_h263_mb_info
	getPayloadSize x = return$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVPacketSideDataPayload AVPacketSideDataReplayGain where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_replaygain
	getPayloadType _ = av_pkt_data_replaygain
	getPayloadSize x = return$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVPacketSideDataPayload AVPacketSideDataDisplayMatrix where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_displaymatrix
	getPayloadType _ = av_pkt_data_displaymatrix
	getPayloadSize x = return$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVPacketSideDataPayload AVPacketSideDataStereo3d where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_stereo3d
	getPayloadType _ = av_pkt_data_stereo3d
	getPayloadSize x = return$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
--instance AVPacketSideDataPayload AVPacketSideDataAudioServiceType where
--	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_audio_service_type
--	getPayloadType _ = av_pkt_data_audio_service_type
--	getPayloadSize x = return$ sizeOf x
--	peekPayload ptr _ = peek ptr
--	pokePayload = poke
--instance AVPacketSideDataPayload AVPacketSideDataQualityFactor where
--	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_quality_factor
--	getPayloadType _ = av_pkt_data_quality_factor
--	getPayloadSize x = return$ sizeOf x
--	peekPayload ptr _ = peek ptr
--	pokePayload = poke
instance AVPacketSideDataPayload AVPacketSideDataSkipSamples where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_skip_samples
	getPayloadType _ = av_pkt_data_skip_samples
	getPayloadSize x = return$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVPacketSideDataPayload AVPacketSideDataJpDualmono where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_jp_dualmono
	getPayloadType _ = av_pkt_data_jp_dualmono
	getPayloadSize x = return$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVPacketSideDataPayload AVPacketSideDataStringsMetadata where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_strings_metadata
	getPayloadType _ = av_pkt_data_strings_metadata
	getPayloadSize (AVPacketSideDataStringsMetadata d) = getPackedDictSize d
	peekPayload ptr size = AVPacketSideDataStringsMetadata <$> getPackedDict (castPtr ptr) size
	pokePayload ptr (AVPacketSideDataStringsMetadata d) = packDict (castPtr ptr) d
instance AVPacketSideDataPayload AVPacketSideDataSubtitlePosition where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_subtitle_position
	getPayloadType _ = av_pkt_data_subtitle_position
	getPayloadSize x = return$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVPacketSideDataPayload AVPacketSideDataMatroskaBlockadditional where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_matroska_blockadditional
	getPayloadType _ = av_pkt_data_matroska_blockadditional
	getPayloadSize (AVPacketSideDataMatroskaBlockadditional s) = return$ B.length s
	peekPayload ptr size = AVPacketSideDataMatroskaBlockadditional <$> B.packCStringLen (castPtr ptr, size)
	pokePayload ptr (AVPacketSideDataMatroskaBlockadditional s) =
		B.unsafeUseAsCString s$ \ps -> copyArray (castPtr ptr) ps (B.length s)
instance AVPacketSideDataPayload AVPacketSideDataWebvttIdentifier where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_webvtt_identifier
	getPayloadType _ = av_pkt_data_webvtt_identifier
	getPayloadSize (AVPacketSideDataWebvttIdentifier s) = return$ length s
	peekPayload ptr size = AVPacketSideDataWebvttIdentifier <$> peekCStringLen (castPtr ptr, size)
	pokePayload ptr (AVPacketSideDataWebvttIdentifier s) =
		withCStringLen s$ \(ps, len) -> copyArray (castPtr ptr) ps len
instance AVPacketSideDataPayload AVPacketSideDataWebvttSettings where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_webvtt_settings
	getPayloadType _ = av_pkt_data_webvtt_settings
	getPayloadSize (AVPacketSideDataWebvttSettings s) = return$ length s
	peekPayload ptr size = AVPacketSideDataWebvttSettings <$> peekCStringLen (castPtr ptr, size)
	pokePayload ptr (AVPacketSideDataWebvttSettings s) =
		withCStringLen s$ \(ps, len) -> copyArray (castPtr ptr) ps len
instance AVPacketSideDataPayload AVPacketSideDataMetadataUpdate where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType av_pkt_data_metadata_update
	getPayloadType _ = av_pkt_data_metadata_update
	getPayloadSize (AVPacketSideDataMetadataUpdate d) = getPackedDictSize d
	peekPayload ptr size = AVPacketSideDataMetadataUpdate <$> getPackedDict (castPtr ptr) size
	pokePayload ptr (AVPacketSideDataMetadataUpdate d) = packDict (castPtr ptr) d

-- | Add side data to a packet.  If there is already side data of the same
-- type, then the old side data is replaced.
packetSetSideData :: forall a m. (MonadIO m, MonadError String m, AVPacketSideDataPayload a) =>
	AVPacket -> AVPacketSideData a -> m ()
packetSetSideData pkt sd = do
	let AVPacketSideData payload = sd
	let sdType = getPayloadType payload
	sdSize <- liftIO$ getPayloadSize payload

	packetFreeSideDataType pkt sdType

	pdata <- liftIO.withThis pkt$ \ptr -> castPtr <$>
		av_packet_new_side_data ptr (fromCEnum sdType) (fromIntegral sdSize)
	
	when (pdata == nullPtr)$
		throwError$ "packetSetSideData: av_packet_new_side_data returned a null pointer"

	liftIO$ pokePayload pdata payload
	
-- | Get the side data associated with a packet
packetGetSideData :: forall a m. (MonadIO m, AVPacketSideDataPayload a) =>
	AVPacket -> m (Maybe (AVPacketSideData a))
packetGetSideData pkt = liftIO.withThis pkt$ \ptr -> do
	(pdata, dsize) <- peekAVPacketSideDataPtr ptr :: IO (Ptr a, Int)
	if (pdata == nullPtr) then return Nothing else do
		v <- peekPayload pdata (fromIntegral dsize)
		return.Just$ AVPacketSideData v

foreign import ccall "b_av_packet_get_side_data_i" b_av_packet_get_side_data_i :: Ptr () -> CInt -> IO (Ptr ())

-- | Free all the side data of a particular type
packetFreeSideDataType :: (MonadIO m, MonadError String m) =>
	AVPacket -> AVPacketSideDataType -> m ()
packetFreeSideDataType pkt sdType = do
	(pdata', nFiltered) <- liftIO.withThis pkt$ \ptr -> do
		ndata <- #{peek AVPacket, side_data_elems} ptr
		pdata <- #{peek AVPacket, side_data} ptr
		filtered <-
			filterM notMatchingAndFreeFiltered =<<
				mapM ((castPtr <$>).b_av_packet_get_side_data_i pdata) [0..(ndata - 1)]

		let nFiltered = length filtered
		let offsets = fmap (* #{size AVPacketSideData}) [0..]
		forM (filtered `zip` offsets)$ \(pSrc, offDst) ->
			moveBytes (pdata `plusPtr` offDst) pSrc #{size AVPacketSideData}

		pdata' <- av_realloc pdata.fromIntegral$ #{size AVPacketSideData} * nFiltered
		return (pdata', nFiltered)

	when (pdata' == nullPtr)$
		throwError$ "packetFreeSideDataType: av_realloc returned a null pointer"

	liftIO.withThis pkt$ \ptr -> do
		#{poke AVPacket, side_data} ptr pdata'
		#{poke AVPacket, side_data_elems} ptr (fromIntegral nFiltered :: CInt)

	where
		notMatchingAndFreeFiltered :: Ptr () -> IO Bool
		notMatchingAndFreeFiltered ptr = do
			ptype <- #{peek AVPacketSideData, type} ptr
			when (ptype == sdType)$ av_free ptr
			return$ ptype /= sdType
		

-- | Free all the side data associated with a packet
packetFreeSideData :: MonadIO m => AVPacket -> m ()
packetFreeSideData pkt = liftIO$ withThis pkt av_packet_free_side_data

-- | Get a string representation of a side data type
packetSideDataName :: AVPacketSideDataType -> String
packetSideDataName =
	-- safe because av_packet_side_data_name returns a const string
	unsafePerformIO.peekCString.av_packet_side_data_name.fromCEnum

-- | Change the timebase of a packet, rescaling the timestamps
packetRescaleTS :: MonadIO m => AVPacket -> Rational -> Rational -> m ()
packetRescaleTS pkt src dst =
	liftIO.withThis pkt$ \ptr -> av_packet_rescale_ts ptr
		(fromIntegral$ numerator src) (fromIntegral$ denominator src)
		(fromIntegral$ numerator dst) (fromIntegral$ denominator dst)

-- | Initialise a packet with the given buffer, and default values for all
-- other fields
newPacketFromBuffer :: MonadIO m => AVPacket -> Ptr AVBufferRef -> m ()
newPacketFromBuffer pkt pbuffRef = do
	packetFree pkt
	(pdata, size) <- getBufferData pbuffRef
	liftIO.withThis pkt$ \ppkt -> do
		av_init_packet ppkt
		#{poke AVPacket, buf} ppkt pbuffRef
		#{poke AVPacket, data} ppkt pdata
		#{poke AVPacket, size} ppkt (fromIntegral size :: CInt)

