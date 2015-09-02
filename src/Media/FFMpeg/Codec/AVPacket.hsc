{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
	--av_packet_side_data_name,
	av_packet_pack_dictionary,
	av_packet_unpack_dictionary,
	av_packet_free_side_data,
	av_packet_ref,
	av_packet_unref,
	av_packet_move_ref,
	av_packet_copy_props,
	av_packet_rescale_ts,

	AVPacket,

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
	--packetSideDataName,
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
--foreign import ccall "av_packet_side_data_name" av_packet_side_data_name :: CInt -> CString
foreign import ccall "av_packet_pack_dictionary" av_packet_pack_dictionary :: UnderlyingType AVDictionary -> Ptr CInt -> IO (Ptr Word8)
foreign import ccall "av_packet_unpack_dictionary" av_packet_unpack_dictionary :: Ptr Word8 -> CInt -> Ptr (UnderlyingType AVDictionary) -> IO CInt
foreign import ccall "av_packet_free_side_data" av_packet_free_side_data :: Ptr AVPacket -> IO ()
foreign import ccall "av_packet_ref" av_packet_ref :: Ptr AVPacket -> Ptr AVPacket -> IO CInt
foreign import ccall "av_packet_unref" av_packet_unref :: Ptr AVPacket -> IO ()
foreign import ccall "av_packet_move_ref" av_packet_move_ref :: Ptr AVPacket -> Ptr AVPacket -> IO ()
foreign import ccall "av_packet_copy_props" av_packet_copy_props :: Ptr AVPacket -> Ptr AVPacket -> IO CInt

foreign import ccall "b_av_packet_rescale_ts" av_packet_rescale_ts :: Ptr AVPacket -> Ptr (Maybe AVRational) -> Ptr (Maybe AVRational) -> IO ()
foreign import ccall "&b_free_packet" pav_free_packet :: FunPtr (Ptr () -> IO ())

-- | AVPacket struct
newtype AVPacket = AVPacket (ForeignPtr AVPacket)
instance ExternalPointer AVPacket where
	type UnderlyingType AVPacket = AVPacket
	withThis (AVPacket fp) = withThis fp

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

peekAVPacketSideDataPtrType ::
	AVPacketSideDataType -> Ptr AVPacket -> IO (Ptr Word8, Int)
peekAVPacketSideDataPtrType sdType ptr =
	alloca $ \psize -> do
		pdata <- av_packet_get_side_data ptr (fromCEnum sdType) psize
		size <- fromIntegral <$> peek psize
		return (pdata, size)

getPackedDictSize :: AVDictionary -> IO Int
getPackedDictSize dict =
	withThis dict$ \ppd ->
	alloca$ \psize -> do
		pd <- peek ppd
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
	withThis dict$ \ppd ->
	alloca$ \psize -> do
		pd <- peek ppd
		x <- av_packet_pack_dictionary pd psize
		size <- fromIntegral <$> peek psize
		copyArray (castPtr ptr) x size 

instance AVPacketSideDataPayload AVPacketSideDataPalette where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType AVPktDataPalette
	getPayloadType _ = AVPktDataPalette
	getPayloadSize (AVPacketSideDataPalette s) = return$ B.length s
	peekPayload ptr size = AVPacketSideDataPalette <$> B.packCStringLen (castPtr ptr, size)
	pokePayload ptr (AVPacketSideDataPalette s) =
		B.unsafeUseAsCString s$ \ps -> copyArray (castPtr ptr) ps (B.length s)
instance AVPacketSideDataPayload AVPacketSideDataNewExtradata where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType AVPktDataNewExtradata
	getPayloadType _ = AVPktDataNewExtradata
	getPayloadSize (AVPacketSideDataNewExtradata s) = return$ B.length s
	peekPayload ptr size = AVPacketSideDataNewExtradata <$> B.packCStringLen (castPtr ptr, size)
	pokePayload ptr (AVPacketSideDataNewExtradata s) =
		B.unsafeUseAsCString s$ \ps -> copyArray (castPtr ptr) ps (B.length s)
instance AVPacketSideDataPayload AVPacketSideDataParamChange where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType AVPktDataParamChange
	getPayloadType _ = AVPktDataParamChange
	getPayloadSize x = return$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVPacketSideDataPayload AVPacketSideDataH263MbInfo where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType AVPktDataH263MbInfo
	getPayloadType _ = AVPktDataH263MbInfo
	getPayloadSize x = return$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVPacketSideDataPayload AVPacketSideDataReplayGain where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType AVPktDataReplayGain
	getPayloadType _ = AVPktDataReplayGain
	getPayloadSize x = return$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVPacketSideDataPayload AVPacketSideDataDisplayMatrix where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType AVPktDataDisplayMatrix
	getPayloadType _ = AVPktDataDisplayMatrix
	getPayloadSize x = return$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVPacketSideDataPayload AVPacketSideDataStereo3d where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType AVPktDataStereo3d
	getPayloadType _ = AVPktDataStereo3d
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
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType AVPktDataSkipSamples
	getPayloadType _ = AVPktDataSkipSamples
	getPayloadSize x = return$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVPacketSideDataPayload AVPacketSideDataJpDualmono where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType AVPktDataJpDualmono
	getPayloadType _ = AVPktDataJpDualmono
	getPayloadSize x = return$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVPacketSideDataPayload AVPacketSideDataStringsMetadata where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType AVPktDataStringsMetadata
	getPayloadType _ = AVPktDataStringsMetadata
	getPayloadSize (AVPacketSideDataStringsMetadata d) = getPackedDictSize d
	peekPayload ptr size = AVPacketSideDataStringsMetadata <$> getPackedDict (castPtr ptr) size
	pokePayload ptr (AVPacketSideDataStringsMetadata d) = packDict (castPtr ptr) d
instance AVPacketSideDataPayload AVPacketSideDataSubtitlePosition where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType AVPktDataSubtitlePosition
	getPayloadType _ = AVPktDataSubtitlePosition
	getPayloadSize x = return$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVPacketSideDataPayload AVPacketSideDataMatroskaBlockadditional where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType AVPktDataMatroskaBlockadditional
	getPayloadType _ = AVPktDataMatroskaBlockadditional
	getPayloadSize (AVPacketSideDataMatroskaBlockadditional s) = return$ B.length s
	peekPayload ptr size = AVPacketSideDataMatroskaBlockadditional <$> B.packCStringLen (castPtr ptr, size)
	pokePayload ptr (AVPacketSideDataMatroskaBlockadditional s) =
		B.unsafeUseAsCString s$ \ps -> copyArray (castPtr ptr) ps (B.length s)
instance AVPacketSideDataPayload AVPacketSideDataWebvttIdentifier where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType AVPktDataWebvttIdentifier
	getPayloadType _ = AVPktDataWebvttIdentifier
	getPayloadSize (AVPacketSideDataWebvttIdentifier s) = return$ length s
	peekPayload ptr size = AVPacketSideDataWebvttIdentifier <$> peekCStringLen (castPtr ptr, size)
	pokePayload ptr (AVPacketSideDataWebvttIdentifier s) =
		withCStringLen s$ \(ps, len) -> copyArray (castPtr ptr) ps len
instance AVPacketSideDataPayload AVPacketSideDataWebvttSettings where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType AVPktDataWebvttSettings
	getPayloadType _ = AVPktDataWebvttSettings
	getPayloadSize (AVPacketSideDataWebvttSettings s) = return$ length s
	peekPayload ptr size = AVPacketSideDataWebvttSettings <$> peekCStringLen (castPtr ptr, size)
	pokePayload ptr (AVPacketSideDataWebvttSettings s) =
		withCStringLen s$ \(ps, len) -> copyArray (castPtr ptr) ps len
instance AVPacketSideDataPayload AVPacketSideDataMetadataUpdate where
	peekAVPacketSideDataPtr = (first castPtr <$>).peekAVPacketSideDataPtrType AVPktDataMetadataUpdate
	getPayloadType _ = AVPktDataMetadataUpdate
	getPayloadSize (AVPacketSideDataMetadataUpdate d) = getPackedDictSize d
	peekPayload ptr size = AVPacketSideDataMetadataUpdate <$> getPackedDict (castPtr ptr) size
	pokePayload ptr (AVPacketSideDataMetadataUpdate d) = packDict (castPtr ptr) d

-- | Add side data to a packet.  If there is already side data of the same
-- type, then the old side data is replaced.
packetSetSideData :: forall a m.
	(MonadIO m, MonadError String m, AVPacketSideDataPayload a) =>
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
-- packetSideDataName :: AVPacketSideDataType -> String
-- packetSideDataName =
-- 	-- safe because av_packet_side_data_name returns a const string
-- 	unsafePerformIO.peekCString.av_packet_side_data_name.fromCEnum

-- | Change the timebase of a packet, rescaling the timestamps
packetRescaleTS :: MonadIO m => AVPacket -> AVRational -> AVRational -> m ()
packetRescaleTS pkt src dst =
	withThis pkt$ \ptr -> liftIO$
		with (Just src)$ \psrc ->
		with (Just dst)$ \pdst -> av_packet_rescale_ts ptr psrc pdst

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

