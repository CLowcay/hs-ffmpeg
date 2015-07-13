#include "ffmpeg.h"

void b_free_packet(AVPacket * pkt) {
	av_free_packet (pkt);
}

void b_init_packet(AVPacket * pkt) {
	av_init_packet (pkt);
}

void b_free_dictionary(AVDictionary **d) {
	if (*d != NULL) {
		av_dict_free(d);
		*d = NULL;
	}
}

