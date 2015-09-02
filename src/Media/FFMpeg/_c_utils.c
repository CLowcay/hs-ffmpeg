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

void b_av_packet_rescale_ts(AVPacket *pkt, AVRational *tb_src, AVRational *tb_dst) {
	av_packet_rescale_ts(pkt, *tb_src, *tb_dst);
}

AVPacketSideData *b_av_packet_get_side_data_i(AVPacketSideData *p, int i) {
	return &(p[i]);
}

void b_av_codec_get_pkt_timebase(AVCodecContext *p, AVRational *r) {
	*r = av_codec_get_pkt_timebase(p);
}

void b_av_codec_set_pkt_timebase(AVCodecContext *p, AVRational *v) {
	av_codec_set_pkt_timebase(p, *v);
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

int b_av_opt_set_q(void *obj, const char *name, AVRational *v, int search_flags) {
	return av_opt_set_q(obj, name, *v, search_flags);
}

int b_av_opt_set_video_rate(void *obj, const char *name, int num, int den, int search_flags) {
	AVRational r;
	r.num = num;
	r.den = den;
	return av_opt_set_video_rate(obj, name, r, search_flags);
}

int b_av_opt_get_video_rate(void *obj, const char *name, int search_flags, int *num, int *den) {
	AVRational r;
	int ret = av_opt_get_video_rate(obj, name, search_flags, &r);
	*num = r.num;
	*den = r.den;
	return ret;
}

int b_image_check_sar(unsigned int w, unsigned int h, AVRational *sar) {
	return av_image_check_sar(w, h, *sar);
}

void b_av_stream_get_r_frame_rate (const AVStream *s, AVRational *r) {
	*r = av_stream_get_r_frame_rate(s);
}

void b_av_stream_set_r_frame_rate (AVStream *s, AVRational *r) {
	av_stream_set_r_frame_rate(s, *r);
}

char *b_av_ts_make_string(char *buf, int64_t ts) {
	return av_ts_make_string(buf, ts);
}

char *b_av_ts_make_time_string(char *buf, int64_t ts, AVRational *tb) {
	return av_ts_make_time_string(buf, ts, tb);
}

void close_format_context(AVFormatContext *ctx) {
	avio_closep(&ctx->pb);
}

