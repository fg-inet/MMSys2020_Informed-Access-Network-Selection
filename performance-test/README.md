This directory contains some scripts for running performance tests for IANS, such as for evaluating video performance.

Video Streaming
-----------

	video/*
        eval/video_over_time.R     Plot video representation, buffer level, throughput rate, etc over time
        eval/stallings.R           Plot stalling count, median/mean/max duration
        eval/representations.R     Plot representations during video playout
        eval/dump_json.py          From logged data with stallings, compute P.1203 QoE
        eval/plot_qoe.R            Plot QoE data computed by dump_json.py as barplot
        eval/heatmap_video.R       Plot QoE data as heatmap
        eval/show_best_worst.R     Output the video loads with the best and the worst MOS
        eval/data_during_load.R    Analyze and plot distributions of certain MAM network characteristics, such as download rate, during video load


Running experiments
-----------

	video/
		run.sh              Run a single experiment
        stream_video.sh     Start streaming one or multiple videos given in a URL file (run as part of run.sh)
        load_stuff.sh       Fake audio data to be loaded
