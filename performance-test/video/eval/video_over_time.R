#!/usr/bin/Rscript
# Plot the bitrate of loaded video segments over time
# e.g., show what the Adaptive Bitrate algorithm is doing and what MAM is measuring

source("videodata.R")

PRINT_SUMMARY = F

SPLIT_SUMMARY_BY = c("abr", "workload")

SPLIT_BY_LOAD = T

metricslist = list(
#	c("cqm_qoe"),
#    c("representation", "bufferlevel")
    c("representation")
#    c("downloadtime", "stalling_duration"), c("download_rate", "download_max_rate"), c("min_srtt", "median_srtt"), c("srtt_var", "median_of_rtt_vars", "num_conns"), 
#    c("download_rate_max_10", "download_rate_max_50", "download_rate_max_100", "download_rate_max_300"), c("download_rate_max_600", "download_rate_max_1200", "download_rate_max_3000", "download_rate_max_6000"),
#    c("download_rate_q90_10", "download_rate_q90_50", "download_rate_q90_100", "download_rate_q90_300"), c("download_rate_q90_600", "download_rate_q90_1200", "download_rate_q90_3000", "download_rate_q90_6000"),
#    c("download_rate_q50_10", "download_rate_q50_50", "download_rate_q50_100", "download_rate_q50_300"), c("download_rate_q50_600", "download_rate_q50_1200", "download_rate_q50_3000", "download_rate_q50_6000"),
#    c("download_rate_bw_sma_10", "download_rate_bw_sma_50", "download_rate_bw_sma_100", "download_rate_bw_sma_300"), c("download_rate_bw_sma_600", "download_rate_bw_sma_1200", "download_rate_bw_sma_3000", "download_rate_bw_sma_6000"),
#    c("download_rate_bw_wma_10", "download_rate_bw_wma_50", "download_rate_bw_wma_100", "download_rate_bw_wma_300"), c("download_rate_bw_wma_600", "download_rate_bw_wma_1200", "download_rate_bw_wma_3000", "download_rate_bw_wma_6000"),
#    c("download_rate_bw_ewma_10", "download_rate_bw_ewma_50", "download_rate_bw_ewma_100", "download_rate_bw_ewma_300"), c("download_rate_bw_ewma_600", "download_rate_bw_ewma_1200", "download_rate_bw_ewma_3000", "download_rate_bw_ewma_6000")
#    c("download_rate_bw_sma_10", "download_sma_max_short", "download_sma_max_mid", "download_sma_max_long", "download_rate_max_6000")
#    c("download_rate_bw_sma_10", "download_sma_10q_short", "download_sma_10q_mid", "download_sma_10q_long")
    )

#metricslist = list(c("representation", "bufferlevel", "dl_rate_prev"), c("downloadtime", "stalling_duration"), c("download_rate", "download_max_rate"), c("min_srtt", "median_srtt"), c("duration"))
#metricslist = list(c("representation", "bufferlevel", "dl_rate_prev"), c("downloadtime", "stalling_duration"))#, c("download_rate", "download_max_rate"), c("min_srtt", "median_srtt"))

logprefix="data/"

PLOTTYPE="l"
FORCE_ABS_X=TRUE

SEGMENT_DURATION=4

PRINT_STALLINGS=F


filterscenarios=c()

# Parse command line arguments, determine which runs will be read
logruns = runstoread(commandArgs(trailingOnly=TRUE))

logprefix=logruns$logprefix
runs = logruns$runs

OUTPUTDIR=paste(logprefix, runs[length(runs)], "/plots/overtime", sep="")
dir.create(OUTPUTDIR, showWarnings=FALSE)

DATAFILE=paste(OUTPUTDIR, "videodata_over_time.Rdata", sep="/")
if (file.exists(DATAFILE)) {
    load(DATAFILE)
    cat("Loaded data from", DATAFILE, "\n")
} else {
    data = read_abr_runs(logprefix=logprefix, runs=runs, print=F)
    idata = read_init_runs(logprefix=logprefix, runs=runs, print=F)
    framedata = read_frame_runs(logprefix=logprefix, runs=runs, print=F)
    #policydata = read_policy_runs(logprefix=logprefix, runs=runs, print=F)
    #cqmdata = read_cqm_runs(logprefix=logprefix, runs=runs, print=F)

    mamdata = read_one_run(logprefix=logprefix, run=runs[1], print=F)
    data = compute_stallings(data, idata, segment_duration=SEGMENT_DURATION * 1000, verbose=F, summaryprint=PRINT_STALLINGS)
    data = actual_stallings(data, framedata, idata, verbose=F, summaryprint=PRINT_STALLINGS)

    #policydata = mam_readings_before_decision(policydata, mamdata)

    data$dl_rate_prev = data$dl_rate_prev / 1000

    data$downloadtime = data$downloadtime / 1000
    data$starttimestamp_numeric = data$starttimestamp_numeric * 1000
}

save(data, idata, framedata, mamdata, file=DATAFILE)
#save(data, idata, framedata, mamdata, cqmdata, file=DATAFILE)
#str(data)

for (metrics in metricslist) {
    cat("\nLooking up metrics", metrics, "\n")
	datatoplot = NULL
	if (!is.null(mamdata$ifdata[[1]][[metrics[1]]])) {
        cat("Plotting mamdata (interface)\n")
		datatoplot = mamdata$ifdata
        xmetrics = "timestamp"
	} else if (!is.null(mamdata$pfdata[[1]][[metrics[1]]])) {
        cat("Plotting mamdata (prefix)\n")
		datatoplot = mamdata$pfdata
        xmetrics = "timestamp"
	#} else if (!is.null(policydata[[metrics[1]]])) {
    #    cat("Plotting policy data (prefix)\n")
	#	datatoplot = policydata
    #    xmetrics = "timestamp"
	#} else if (!is.null(cqmdata[[metrics[1]]])) {
    #    cat("Plotting CQM data\n")
	#	datatoplot = cqmdata
    #    xmetrics = "content"
	} else {
        cat("Plotting video data\n")
        datatoplot = data
        xmetrics = "segmentindex"
    }

	if (SPLIT_BY_LOAD) {
		for (startt in levels(data$starttimestamp)) {
			cat("\nStarttimestamp now:", startt, "\n")
			if (!is.null(datatoplot$starttimestamp)) {
				datatoplotnow = fix.factors(subset(datatoplot, starttimestamp == startt))
				if (is.null(datatoplotnow$abr) | is.null(datatoplotnow$policy)) {
					corresponding_video_data = fix.factors(subset(data, data$starttimestamp == startt))
					abralg = levels(corresponding_video_data$abr)[1]
					policy = levels(corresponding_video_data$policy)[1]
				} else {
					abralg = levels(datatoplotnow$abr)[1]
					policy = levels(datatoplotnow$policy)[1]
				}
			} else {
				#str(datatoplot)
				corresponding_video_data = fix.factors(subset(data, data$starttimestamp == startt))
				corresponding_init_data = fix.factors(subset(idata, idata$starttimestamp == startt))
				abralg = levels(corresponding_video_data$abr)[1]
				policy = levels(corresponding_video_data$policy)[1]
				min_timestamp = min(corresponding_init_data$timestamp_start_mpd_load) / 1000
				max_timestamp = max(corresponding_video_data$timestamp_end) / 1000
				#str(min_timestamp)
				#str(max_timestamp)
				#str(datatoplot[[1]]$timestamp)
				if (!is.null(datatoplot$timestamp)) {
					datatoplotnow = gettimewindow(datatoplot, xmin=min_timestamp, xmax=max_timestamp)
					cat("Subsetting data to time window between", min_timestamp, "and", max_timestamp, "\n")
				}
				#datatoplotnow = subset(datatoplot, timestamp > min_timestamp & timestamp <= max_timestamp)
				#str(datatoplotnow)
			}

			filename = paste(OUTPUTDIR, "/", abralg, "_", policy, "_", startt,  "_", paste(metrics, collapse="_"), ".pdf", sep="")
			plotovertime(datatoplotnow, metrics, xdata=xmetrics, filename=filename, filterscenarios=filterscenarios, plotlabel=paste(paste(gsub("_", " ", metrics), collapse=", "), " with ", abralg, " and ", policy), plot="pdf", plottype=PLOTTYPE, force_abs_x=FORCE_ABS_X, yaxis2 = ifelse("dl_rate_prev" %in% metrics, "dl_rate_prev", ""), verbose=F)
			cat("Plotted", metrics, "to", filename, "\n")
			if (PRINT_SUMMARY) {
				for (m in metrics) {
					#cat(m, ":\n")
					#print(summary(datatoplotnow[[m]]))
					summarizedata(datatoplotnow, m, index=ifelse(xmetrics=="segmentindex", "", "interface"))
				}
			}
		}
	} else {
		abralg="(all)"
		policy="(all)"
		datalabels = unlist(sapply(levels(datatoplot$starttimestamp), function(x) { cvd = fix.factors(subset(data, data$starttimestamp==x)); return(paste(cvd$abr[1], cvd$policy[1], sep="/")) } ))
		str(datalabels)
		filename = paste(OUTPUTDIR, "/", "all_", paste(metrics, collapse="_"), ".pdf", sep="")
		plotovertime(split(datatoplot, datatoplot$starttimestamp), metrics, xdata=xmetrics, data_labels=datalabels, filename=filename, filterscenarios=filterscenarios, plotlabel=paste(paste(gsub("_", " ", metrics), collapse=", "), " with ", abralg, " and ", policy), plot="pdf", plottype=PLOTTYPE, force_abs_x=FORCE_ABS_X, yaxis2 = ifelse("dl_rate_prev" %in% metrics, "dl_rate_prev", ""), verbose=T)
		cat("Plotted", metrics, "to", filename, "\n")

	}
}

#save(data, idata, framedata, mamdata, policydata, file=DATAFILE)
