
#include "config.h"

#if (HAVE_FFMPEG_SWSCALE_H == 1)
#include "ffmpeg/swscale.h"
#elif (HAVE_LIBSWSCALE_SWSCALE_H == 1)
#include "libswscale/swscale.h"
#else
#error "Can't find swscale.h file"
#endif

#if (HAVE_FFMPEG_AVCODEC_H == 1)
#include "ffmpeg/avcodec.h"
#elif (HAVE_LIBAVCODEC_AVCODEC_H == 1)
#include "libavcodec/avcodec.h"
#else
#error "Can't find avcodec.h file"
#endif

#if (HAVE_FFMPEG_AVFORMAT_H == 1)
#include "ffmpeg/avformat.h"
#elif (HAVE_LIBAVFORMAT_AVFORMAT_H == 1)
#include "libavformat/avformat.h"
#else
#error "Can't find avformat.h file"
#endif

#if (HAVE_FFMPEG_AVUTIL_H == 1)
#include "ffmpeg/avutil.h"
#include "ffmpeg/stereo3d.h"
#include "ffmpeg/downmix_info.h"
#include "ffmpeg/replaygain.h"
#include "ffmpeg/motion_vector.h"
#include "ffmpeg/opt.h"
#include "ffmpeg/imgutils.h"
#include "ffmpeg/timestamp.h"
#elif (HAVE_LIBAVUTIL_AVUTIL_H == 1)
#include "libavutil/avutil.h"
#include "libavutil/stereo3d.h"
#include "libavutil/downmix_info.h"
#include "libavutil/replaygain.h"
#include "libavutil/motion_vector.h"
#include "libavutil/opt.h"
#include "libavutil/imgutils.h"
#include "libavutil/timestamp.h"
#else
#error "Can't find avutil.h file"
#endif
