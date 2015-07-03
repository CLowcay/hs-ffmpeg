
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
#elif (HAVE_LIBAVUTIL_AVUTIL_H == 1)
#include "libavutil/avutil.h"
#else
#error "Can't find avutil.h file"
#endif
