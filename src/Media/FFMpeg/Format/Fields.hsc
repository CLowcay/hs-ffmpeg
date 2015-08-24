{-# LANGUAGE DataKinds #-}

{- |
 
Description : Bindings to libavformat
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavformat.

-}

module Media.FFMpeg.Format.Fields (
	avstream_index,
	avstream_id,
	avstream_codec,
	avstream_time_base,
	avstream_start_time,
	avstream_duration,
	avstream_nb_frames,
	avstream_disposition,
	avstream_discard,
	avstream_sample_aspect_ratio,
	avstream_metadata,
	avstream_avg_frame_rate,
	avstream_attached_pic
) where

#include "ffmpeg.h"

import Data.Int
import Foreign.C.Types
import Foreign.Ptr

import Media.FFMpeg.Codec
import Media.FFMpeg.Format.Core
import Media.FFMpeg.Format.Enums
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util

avstream_index :: Field AVStream CInt ReadOnly
avstream_index = Field #{offset AVStream, index}

avstream_id :: Field AVStream CInt ReadOnly
avstream_id = Field #{offset AVStream, id}

avstream_codec :: Field AVStream AVCodecContext ReadOnly
avstream_codec = Field #{offset AVStream, codec}

avstream_time_base :: Field AVStream AVRational ReadWrite
avstream_time_base = Field #{offset AVStream, time_base}

avstream_start_time :: Field AVStream AVTimestamp ReadWrite
avstream_start_time = Field #{offset AVStream, start_time}

avstream_duration :: Field AVStream AVTimestamp ReadWrite
avstream_duration = Field #{offset AVStream, duration}

avstream_nb_frames :: Field AVStream Int64 ReadWrite
avstream_nb_frames = Field #{offset AVStream, nb_frames}

avstream_disposition :: Field AVStream AVDispositionFlag ReadWrite
avstream_disposition = Field #{offset AVStream, disposition}

avstream_discard :: Field AVStream AVDiscard ReadWrite
avstream_discard = Field #{offset AVStream, discard}

avstream_sample_aspect_ratio :: Field AVStream AVRational ReadWrite
avstream_sample_aspect_ratio = Field #{offset AVStream, sample_aspect_ratio}

avstream_metadata :: Field AVStream (Ptr AVDictionary) ReadWrite
avstream_metadata = Field #{offset AVStream, metadata}

avstream_avg_frame_rate :: Field AVStream AVRational ReadWrite
avstream_avg_frame_rate = Field #{offset AVStream, avg_frame_rate}

avstream_attached_pic :: Field AVStream AVPacket ReadOnly
avstream_attached_pic = Field #{offset AVStream, attached_pic}

