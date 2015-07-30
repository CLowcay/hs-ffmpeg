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

AVPacketSideData *b_av_packet_get_side_data_i(AVPacketSideData *p, int i) {
	return &(p[i]);
}

void b_av_codec_get_pkt_timebase(AVCodecContext *p, int *num, int *den) {
	AVRational r = av_codec_get_pkt_timebase(p);
	*num = r.num;
	*den = r.den;
}

void b_av_codec_set_pkt_timebase(AVCodecContext *p, int num, int den) {
	AVRational v;
	v.num = num;
	v.den = den;
	av_codec_set_pkt_timebase(p, v);
}

void b_av_guess_sample_aspect_ratio(AVFormatContext *format, AVStream *stream, AVFrame *frame, int *num, int *den) {
	AVRational r;
	r = av_guess_sample_aspect_ratio(format, stream, frame);
	*num = r.num;
	*den = r.den;
}

void b_av_guess_frame_rate(AVFormatContext *ctx, AVStream *stream, AVFrame *frame, int *num, int *den) {
	AVRational r;
	r = av_guess_frame_rate(ctx, stream, frame);
	*num = r.num;
	*den = r.den;
}

void b_avformat_close_input(AVFormatContext *ctx) {
	AVFormatContext **s;
	*s = ctx;
	avformat_close_input(s);
}

