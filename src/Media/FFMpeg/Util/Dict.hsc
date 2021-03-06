{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

{- |

Description : Bindings to libavformat
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to dict.c

-}

module Media.FFMpeg.Util.Dict (
	av_dict_get,
	av_dict_count,
	av_dict_set,
	av_dict_set_int,
	av_dict_copy,
	av_dict_get_string,
	av_dict_free,

	DictFlag,
	pattern AVDictMatchCase,
	pattern AVDictIgnoreSuffix,
	pattern AVDictDontOverwrite,
	pattern AVDictAppend,

	AVDictionary,
	newAVDictionary,
	dictGet,
	dictGetAll,
	dictCount,
	dictSet,
	dictSetInt,
	dictCopy,
	dictGetString,
	unsafeDictCopyFromPtr,

	getDictField,
	setDictField
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Bits
import Data.Int
import Data.Monoid
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util.Error

foreign import ccall "&b_free_dictionary" pb_free_dictionary :: FunPtr (Ptr (Ptr AVDictionary) -> IO ())

foreign import ccall "av_dict_get" av_dict_get :: Ptr AVDictionary -> CString -> Ptr () -> CInt -> IO (Ptr ())
foreign import ccall "av_dict_count" av_dict_count :: Ptr AVDictionary -> IO CInt
foreign import ccall "av_dict_set" av_dict_set :: Ptr (Ptr AVDictionary) -> CString -> CString -> CInt -> IO CInt
foreign import ccall "av_dict_set_int" av_dict_set_int :: Ptr (Ptr AVDictionary) -> CString -> Int64 -> CInt -> IO CInt
foreign import ccall "av_dict_copy" av_dict_copy :: Ptr (Ptr AVDictionary) -> Ptr AVDictionary -> CInt -> IO ()
foreign import ccall "av_dict_get_string" av_dict_get_string :: Ptr AVDictionary -> Ptr CString -> CChar -> CChar -> IO CInt
foreign import ccall "av_dict_free" av_dict_free :: Ptr (Ptr AVDictionary) -> IO ()

-- re-imported for local use
foreign import ccall "av_freep" av_freep :: Ptr a -> IO ()

-- | Flags
newtype DictFlag = DictFlag CInt deriving (Eq, Show, CEnum, CFlags)
pattern AVDictMatchCase = DictFlag (#{const AV_DICT_MATCH_CASE})
pattern AVDictIgnoreSuffix = DictFlag (#{const AV_DICT_IGNORE_SUFFIX})
pattern AVDictDontOverwrite = DictFlag (#{const AV_DICT_DONT_OVERWRITE})
pattern AVDictAppend = DictFlag (#{const AV_DICT_APPEND})

-- | AVDictionary type
newtype AVDictionary = AVDictionary (ForeignPtr (Ptr AVDictionary))
instance ExternalPointer AVDictionary where
	type UnderlyingType AVDictionary = Ptr AVDictionary
	withThis (AVDictionary x) = withThis x
instance Show AVDictionary where
	show _ = "{AVDictionary}"

-- | Allocate a new AVDictionary
newAVDictionary :: MonadIO m => m AVDictionary
newAVDictionary = liftIO$ do
	fp <- mallocForeignPtr
	withForeignPtr fp$ \p -> poke p nullPtr
	addForeignPtrFinalizer pb_free_dictionary fp
	return$ AVDictionary fp

-- | Get all the dictionary entries associated with a particular key
dictGet :: MonadIO m =>
	AVDictionary             -- ^ the dictionary
	-> String                -- ^ the key
	-> DictFlag              -- ^ flags
	-> m [(String, String)]  -- ^ a list of all the entries matching the key
dictGet dict key flags = liftIO$
	withThis dict$ \ppd ->
		withCString key$ \ckey -> do
			pd <- peek ppd
			let results = \prev -> do
				next <- av_dict_get pd ckey prev (fromCEnum flags)
				if next == nullPtr then return [] else fmap (next :) (results next)

			rs <- results nullPtr
			forM rs $ \p -> do
				k <- peekCString =<< (#{peek AVDictionaryEntry, key} p :: IO CString)
				v <- peekCString =<< (#{peek AVDictionaryEntry, value} p :: IO CString)
				return (k, v)

-- | Get all entries in a dictionary
dictGetAll :: MonadIO m => AVDictionary -> m [(String, String)]
dictGetAll dict = dictGet dict "" AVDictIgnoreSuffix

-- | Count the number of entries in a dictionary
dictCount :: MonadIO m => AVDictionary -> m Int
dictCount dict = liftIO$ do
	withThis dict$ \ppd -> do
		pd <- peek ppd
		fromIntegral <$> av_dict_count pd

-- | Set a dictionary entry
dictSet :: (MonadIO m, MonadThrow m) =>
	AVDictionary           -- ^ the dictionary to set
	-> (String, String)    -- ^ the (key, value) pair to set
	-> DictFlag            -- ^ flags
	-> m ()
dictSet dict (key, value) flags = do
	r <- liftIO$
		withThis dict$ \ppd ->
		withCString key$ \ckey ->  -- av_dict_set duplicates the keys, so this is safe
		withCString value$ \cvalue ->
			av_dict_set ppd ckey cvalue (fromCEnum flags)

	when (r < 0)$ throwM$ mkError r "dictSet" "av_dict_set"

-- | Set a dictionary entry to a number, converting it to a string
dictSetInt :: (MonadIO m, MonadThrow m) =>
	AVDictionary           -- ^ the dictionary to set
	-> (String, Int64)     -- ^ the (key, value) pair.  The value is converted to a decimal string
	-> DictFlag            -- ^ flags
	-> m ()
dictSetInt dict (key, value) flags = do
	r <- liftIO$
		withThis dict$ \ppd ->
		withCString key$ \ckey ->  -- av_dict_set duplicates the keys, so this is safe
			av_dict_set_int ppd ckey value (fromCEnum flags)

	when (r < 0)$ throwM$ mkError r "dictSetInt" "av_dict_set_int"

-- | Generate a copy of a dictionary
dictCopy :: MonadIO m =>
	AVDictionary             -- ^ the dictionary to set
	-> DictFlag              -- ^ flags
	-> m AVDictionary        -- ^ a new dictionary that is a copy of the original
dictCopy src flags = do
	dst <- newAVDictionary
	liftIO$
		withThis dst$ \ppd ->
		withThis src$ \pps -> do
			ps <- peek pps
			av_dict_copy ppd ps (fromCEnum flags)
	return dst

-- | Get a string representation of a dictionary
dictGetString :: (MonadIO m, MonadThrow m) =>
	AVDictionary     -- ^the dictionary to convert
	-> Char          -- ^character to separate keys and values
	-> Char          -- ^character to separate pairs of keys and values
	-> m String      -- ^a string representation of the dictionary
dictGetString dict keyValSep pairsSep = do
	pbuffer <- liftIO$ malloc
	liftIO$ poke pbuffer nullPtr
	r <- liftIO$
		withThis dict$ \ppd -> do
			pd <- peek ppd
			av_dict_get_string
				pd pbuffer
				(castCharToCChar keyValSep)
				(castCharToCChar pairsSep)

	if r < 0 then do
		liftIO$ do
			av_freep pbuffer
			free pbuffer
		throwM$ mkError r "dictGetString" "av_dict_get_string"
	else liftIO$ do
		s <- peekCString =<< peek pbuffer
		liftIO$ do
			av_freep pbuffer
			free pbuffer
		return s

-- | Copy a dictionary from an arbitrary pointer
unsafeDictCopyFromPtr :: MonadIO m =>
	Ptr AVDictionary
	-> DictFlag
	-> m AVDictionary
unsafeDictCopyFromPtr src flags = do
	dst <- newAVDictionary
	liftIO$
		withThis dst$ \ppd ->
			av_dict_copy ppd src (fromCEnum flags)
	return dst

-- | Get the metadata field from an AVStream
getDictField :: (MonadIO m, ExternalPointer a) =>
	a -> Field a AVDictionary ro -> m AVDictionary
getDictField x (Field offset offsets) = do
	pd <- liftIO$ withThis x$ \px ->
		peek =<< (castPtr <$> chasePointers px offset offsets)
	unsafeDictCopyFromPtr pd mempty 

-- | Set the metadata field of an AVStream
setDictField :: (MonadIO m, ExternalPointer a) =>
	a -> Field a AVDictionary ReadWrite -> AVDictionary -> m ()
setDictField x (Field offset offsets) dict =
	withThis x$ \px ->
	withThis dict$ \ppsrc -> liftIO$ do
		pd <- peek =<< (castPtr <$> chasePointers px offset offsets)
		av_dict_free$ pd
		psrc <- liftIO$ peek ppsrc
		av_dict_copy pd psrc 0

