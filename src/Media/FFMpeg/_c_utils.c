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

void b_av_packet_rescale_ts(AVPacket *pkt, int tb_src_num, int tb_src_den, int tb_dst_num, int tb_dst_den) {
	AVRational src;
	AVRational dst;
	src.num = tb_src_num;
	src.den = tb_src_den;
	dst.num = tb_dst_num;
	dst.den = tb_dst_den;
	av_packet_rescale_ts(pkt, src, dst);
}

