{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
 
Description : Bindings to libavformat
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavformat.

-}

module Media.FFMpeg.Format.AVIO (
) where

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Media.FFMpeg.Format.Enums
import Media.FFMpeg.Internal.Common

--newtype AVIODirContext = AVIODirContext (Ptr AVIODirContext)
--data AVIODirEntry = AVIODirEntry {
--	dirEntry_name :: String,
--	dirEntry_type :: AVIODirEntryType,
--	dirEntry_size :: Int64,
--	dirEntry_modification_timestamp :: Int64,
--	dirEntry_access_timestamp :: Int64,
--	dirEntry_status_change_timestamp :: Int64,
--	dirEntry_user_id :: Int64,
--	dirEntry_group_id :: Int64,
--	dirEntry_filemode :: Int64
--}
newtype AVIOContext = AVIOContext (Ptr AVIOContext)

foreign import ccall "avio_find_protocol_name" avio_find_protocol_name :: CString -> CString
foreign import ccall "avio_check" avio_check :: CString -> CInt -> IO CInt
-- foreign import ccall "avpriv_io_move" avpriv_io_move :: CString -> CString -> IO CInt
-- foreign import ccall "avpriv_io_delete" avpriv_io_delete :: CString -> IO CInt
-- foreign import ccall "avio_open_dir" avio_open_dir :: Ptr (Ptr AVIODirContext) -> CString -> Ptr (UnderlyingType AVDictionary) -> IO CInt
-- foreign import ccall "avio_read_dir" avio_read_dir :: Ptr AVIODirContext -> Ptr (Ptr AVIODirEntry) -> IO CInt
-- foreign import ccall "avio_close_dir" avio_close_dir :: Ptr (Ptr AVIODirContext) -> IO CInt
-- foreign import ccall "avio_free_directory_entry" avio_free_directory_entry :: Ptr (Ptr AVIODirEntry) -> IO ()
foreign import ccall "avio_alloc_context" avio_alloc_context ::
	Ptr CUChar -> CInt -> CInt -> Ptr () ->
	FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO CInt) ->
	FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO CInt) ->
	FunPtr (Ptr () -> Int64 -> CInt -> IO Int64) -> Ptr AVIOContext
foreign import ccall "avio_w8" avio_w8 :: Ptr AVIOContext -> CInt -> IO ()
foreign import ccall "avio_write" avio_write :: Ptr AVIOContext -> CString -> CInt -> IO ()
foreign import ccall "avio_wl64" avio_wl64 :: Ptr AVIOContext -> Word64 -> IO ()
foreign import ccall "avio_wb64" avio_wb64 :: Ptr AVIOContext -> Word64 -> IO ()
foreign import ccall "avio_wl32" avio_wl32 :: Ptr AVIOContext -> CUInt -> IO ()
foreign import ccall "avio_wb32" avio_wb32 :: Ptr AVIOContext -> CUInt -> IO ()
foreign import ccall "avio_wl24" avio_wl24 :: Ptr AVIOContext -> CUInt -> IO ()
foreign import ccall "avio_wb24" avio_wb24 :: Ptr AVIOContext -> CUInt -> IO ()
foreign import ccall "avio_wl16" avio_wl16 :: Ptr AVIOContext -> CUInt -> IO ()
foreign import ccall "avio_wb16" avio_wb16 :: Ptr AVIOContext -> CUInt -> IO ()
foreign import ccall "avio_put_str" avio_put_str :: Ptr AVIOContext -> CString -> IO CInt
foreign import ccall "avio_seek" avio_seek :: Ptr AVIOContext -> Int64 -> CInt -> IO Int64
foreign import ccall "avio_skip" avio_skip :: Ptr AVIOContext -> Int64 -> IO Int64
foreign import ccall "avio_size" avio_size :: Ptr AVIOContext -> IO Int64
foreign import ccall "avio_feof" avio_feof :: Ptr AVIOContext -> IO CInt
foreign import ccall "avio_flush" avio_flush :: Ptr AVIOContext -> IO ()
foreign import ccall "avio_read" avio_read :: Ptr AVIOContext -> CString -> CInt -> IO CInt
foreign import ccall "avio_get_str" avio_get_str :: Ptr AVIOContext -> CInt -> CString -> CInt -> IO CInt
foreign import ccall "avio_open" avio_open :: Ptr (Ptr AVIOContext) -> CString -> CInt -> IO CInt
foreign import ccall "avio_close" avio_close :: Ptr AVIOContext -> IO CInt
foreign import ccall "avio_closep" avio_closep :: Ptr (Ptr AVIOContext) -> IO CInt
foreign import ccall "avio_open_dyn_buf" avio_open_dyn_buf :: Ptr (Ptr AVIOContext) -> IO CInt
foreign import ccall "avio_close_dyn_buf" avio_close_dyn_buf :: Ptr AVIOContext -> Ptr (Ptr Word8) -> IO CInt
foreign import ccall "avio_enum_protocols" avio_enum_protocols :: Ptr (Ptr ()) -> CInt -> IO CString
foreign import ccall "avio_pause" avio_pause :: Ptr AVIOContext -> CInt -> IO CInt
foreign import ccall "avio_seek_time" avio_seek_time :: Ptr AVIOContext -> CInt -> Int64 -> CInt -> IO Int64
foreign import ccall "avio_accept" avio_accept :: Ptr AVIOContext -> Ptr (Ptr AVIOContext) -> IO CInt
foreign import ccall "avio_handshake" avio_handshake :: Ptr AVIOContext -> IO CInt

