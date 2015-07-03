
#include "ffmpeg.h"

void b_free_packet (AVPacket * pkt){
  av_free_packet (pkt);
}

void b_init_packet (AVPacket * pkt){
  av_init_packet (pkt);
}
