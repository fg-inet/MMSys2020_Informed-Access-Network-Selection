#!/usr/bin/Rscript
# Show the distribution of data during video load

source("videodata.R")
source("../../web-timing/eval/plottimings.R")

metricslist = c(
    "download_rate_bw_sma_10"
#    "download_sma_max_short",
#    "download_sma_max_mid",
#    "download_sma_max_long"
) 

logprefix="data/"

PLOTTYPE="l"
FORCE_ABS_X=TRUE

SEGMENT_DURATION=4

PRINT_STALLINGS=F
PRINT_SUMMARY = T

WLAN0_THRESHOLD = 100000
ETH0_THRESHOLD=500000000

filterscenarios=c()

#POLICIES=c("wlan0", "video", "video_pessimist")
POLICIES=c("wlan0")

PLOT_INDIVIDUAL=T

SEPARATE_PLOT_FOR=c("scenario")
#SEPARATE_PLOT_FOR=c()
#SPLIT_BY=c("policy", "abr")
#SPLIT_BY=c("abr", "scenario")
SPLIT_BY=c("abr")
#SPLIT_BY=c("policy", "scenario")

# Parse command line arguments, determine which runs will be read
logruns = runstoread(commandArgs(trailingOnly=TRUE))

logprefix=logruns$logprefix
runs = logruns$runs

OUTPUTDIR_LAST=paste(logprefix, runs[length(runs)], "/plots/overtime", sep="")
AGGREGATED_DATA_FILE = paste(OUTPUTDIR_LAST, "/aggregated_data.Rdata", sep="")

cat("Looking for", AGGREGATED_DATA_FILE, "\n")

for (metrics in metricslist) {
	if (file.exists(AGGREGATED_DATA_FILE)) {
		load(AGGREGATED_DATA_FILE)
		cat("Loaded data from", AGGREGATED_DATA_FILE, "\n")
	} else {
		cat("\nLooking up metrics", metrics, "\n")
		datatoplot = NULL
		datatoplotlist_all = list()

		for (run in runs) {
			datatoplotlist = list()
			datatoplotlabels = c()

			OUTPUTDIR=paste(logprefix, run, "/plots/overtime", sep="")
			dir.create(OUTPUTDIR, showWarnings=FALSE)

			qoedata = read_qoe_runs(logprefix=logprefix, runs=run, print=F)

			DATAFILE=paste(OUTPUTDIR, "videodata_over_time.Rdata", sep="/")
			if (file.exists(DATAFILE)) {
				load(DATAFILE)
				cat("Loaded data from", DATAFILE, "\n")
			} else {
				data = read_abr_runs(logprefix=logprefix, runs=run, print=F)
				idata = read_init_runs(logprefix=logprefix, runs=run, print=F)
				framedata = read_frame_runs(logprefix=logprefix, runs=run, print=F)
				#policydata = read_policy_runs(logprefix=logprefix, runs=run, print=F)

				mamdata = read_one_run(logprefix=logprefix, run=run, print=F)
				data = compute_stallings(data, idata, segment_duration=SEGMENT_DURATION * 1000, verbose=F, summaryprint=PRINT_STALLINGS)
				data = actual_stallings(data, framedata, idata, verbose=F, summaryprint=PRINT_STALLINGS)

				#policydata = mam_readings_before_decision(policydata, mamdata)

				data$dl_rate_prev = data$dl_rate_prev / 1000

				data$downloadtime = data$downloadtime / 1000
				data$starttimestamp_numeric = data$starttimestamp_numeric * 1000
			}
			save(data, idata, framedata, mamdata, file=DATAFILE)


			if (!is.null(mamdata$ifdata[[1]][[metrics]])) {
				cat("Plotting mamdata (interface)\n")
				datatoplot = mamdata$ifdata
				xmetrics = "timestamp"
			} else if (!is.null(mamdata$pfdata[[1]][[metrics]])) {
				cat("Plotting mamdata (prefix)\n")
				datatoplot = mamdata$pfdata
				xmetrics = "timestamp"
			#} else if (!is.null(policydata[[metrics[1]]])) {
			#    cat("Plotting policy data (prefix)\n")
			#	datatoplot = policydata
			#    xmetrics = "timestamp"
			} else {
				cat("Plotting video data\n")
				datatoplot = data
				xmetrics = "segmentindex"
			}

			for (startt in levels(qoedata$starttimestamp)[order(qoedata$qoe)]) {
				#cat("\nStarttimestamp now:", startt, "\n")
				qoevalue = subset(qoedata, qoedata$starttimestamp == startt)[1,]$qoe
				if (!is.null(datatoplot$starttimestamp)) {
					datatoplotnow = fix.factors(subset(datatoplot, starttimestamp == startt))
					abralg = levels(datatoplotnow$abr)[1]
					policy = levels(datatoplotnow$policy)[1]
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
					datatoplotnow = gettimewindow(datatoplot, xmin=min_timestamp, xmax=max_timestamp)
					#datatoplotnow = subset(datatoplot, timestamp > min_timestamp & timestamp <= max_timestamp)
					#cat("Subsetting data to time window between", min_timestamp, "and", max_timestamp, "\n")
					#str(datatoplotnow)
				}
				if (!policy %in% POLICIES) {
					next
				}
				if (grepl("download_rate_bw_sma|download_sma_max", metrics)) {
					#str(datatoplotnow)
					rate_eth0 = datatoplotnow[[1]][[metrics]]
					rate_wlan0 = datatoplotnow[[2]][[metrics]]
					#str(rate_eth0)
					# Subset to times when wlan0 was used (rate on eth0 sufficiently low and rate on wlan0 sufficiently high)
					datatoshow = rate_wlan0[rate_wlan0 > WLAN0_THRESHOLD & rate_eth0 < ETH0_THRESHOLD]
					datatoplotnow = fix.factors(subset(datatoplotnow[[2]], rate_wlan0 > WLAN0_THRESHOLD & rate_eth0 < ETH0_THRESHOLD))
					#str(datatoplotnow)
					if (nrow(datatoplotnow) > 0) {
						datatoplotnow$policy = as.factor(policy)
						datatoplotnow$abr = as.factor(abralg)
						datatoplotnow$qoe = qoevalue
						datatoplotnow$startt = as.factor(startt)
						#str(datatoplotlist)
						#str(length(datatoplotlist) > 0)
						#if (length(datatoplotlist) > 0) {
						#	if (qoevalue < datatoplotlist[[1]]$qoe[1]) {
								# Lower QoE -- insert left
						#		datatoplotlist = c(list(datatoplotnow), datatoplotlist)
						#		datatoplotlabels=c(paste(round(qoevalue, 1), policy, abralg), datatoplotlabels)
						#		cat("-- insert left\n")
						#	} else {
								datatoplotlist = c(datatoplotlist, list(datatoplotnow))
								datatoplotlabels=c(datatoplotlabels, paste(round(qoevalue, 1), ifelse(policy == "wlan0", "Net2", ifelse(policy == "video_pessimist", "Pess", "Opt")), abralg))
						#		cat("-- insert right\n")
						#	}
						#} else {
						#	datatoplotlist = c(list(datatoplotnow))
						#	datatoplotlabels=c(paste(round(qoevalue, 1), policy, abralg))
						#}
					}
				}

				#filename = paste(OUTPUTDIR, "/", abralg, "_", policy, "_", startt,  "_", paste(metrics, collapse="_"), ".pdf", sep="")
				#plotovertime(datatoplotnow, metrics, xdata=xmetrics, filename=filename, filterscenarios=filterscenarios, plotlabel=paste(paste(gsub("_", " ", metrics), collapse=", "), " with ", abralg, " and ", policy), plot="pdf", plottype=PLOTTYPE, force_abs_x=FORCE_ABS_X, yaxis2 = ifelse("dl_rate_prev" %in% metrics, "dl_rate_prev", ""), verbose=F)
				#cat("Plotted", metrics, "to", filename, "\n")
				if (PRINT_SUMMARY) {
					cat(metrics, "for", startt, "with", abralg, "and", policy, "-- QoE:", qoevalue, " : (", nrow(datatoplotnow), "data points )\n")
					print(summary(datatoplotnow[[metrics]]))
					#summarizedata(datatoplotnow, m, index=ifelse(xmetrics=="segmentindex", "", "interface"))
				}
			}

			#str(datatoplotlist)
			names(datatoplotlist) = datatoplotlabels

			if (PLOT_INDIVIDUAL) {
				filename = paste(OUTPUTDIR, "/", paste("rate_cdfs_individual", metrics, sep="_"), sep="")
				prepare_file(plot="eps", filename=filename, width=10, height=7, cex=1, spacebelow=3)
				plot.cdf.list(datatoplotlist, metrics=metrics, xlabel="download rate on network 2")

				invisible(dev.off())
			}
			datatoplotlist_all = c(datatoplotlist_all, datatoplotlist)
		}
		cat("Got datatoplotlist_all of length", length(datatoplotlist_all), "\n")
		aggregated_data = safer_rbind(datatoplotlist_all)
		aggregated_data$policy = factor(aggregated_data$policy, POLICIES)
		aggregated_data = aggregated_data[with(aggregated_data, order(policy)),]
		save(aggregated_data, file=AGGREGATED_DATA_FILE)
	}

	#str(aggregated_data)
	if (length(SPLIT_BY) > 0) {
		if (length(SEPARATE_PLOT_FOR) > 0) {
			aggregated_data_to_plot = split_dataframe_by_factors(aggregated_data, SEPARATE_PLOT_FOR)$data
		} else {
			aggregated_data_to_plot = list(aggregated_data)
		}
		cat("Got data to plot of length", length(aggregated_data_to_plot), "\n")
		for (aggregated_data in aggregated_data_to_plot) {
			filename = paste(OUTPUTDIR_LAST, "/", paste("rate_cdfs_aggregate", metrics, ifelse(length(SEPARATE_PLOT_FOR) > 0, paste(aggregated_data[1,][[SEPARATE_PLOT_FOR]], collapse="_"), ""), sep="_"), sep="")
			plot.timings.cdfs(aggregated_data, metrics=metrics, splitby=SPLIT_BY, plot="eps", filename=filename, print=F, legendposition="bottomright")
			if (length(SPLIT_BY) > 1) {
				cat("First splitting by", SPLIT_BY[2:length(SPLIT_BY)], "\n")
				splitdata = split_dataframe_by_factors(aggregated_data, SPLIT_BY[2:length(SPLIT_BY)], print=T)$data
				for (sitem in splitdata) {
					cat("For", as.character(sitem[1,][[SPLIT_BY[2:length(SPLIT_BY)]]]), "(", nrow(sitem), "data points )\n")
					summarizedata(sitem, metrics, index=SPLIT_BY[1])
				}
			} else {
				summarizedata(aggregated_data, metrics, index=SPLIT_BY[1])
			}
		}
	}
}
