#!/usr/bin/Rscript
# Plot the stallings per ABR and policy

source("videodata.R")
source("../../web-timing/eval/plottimings.R")

PRINT_SUMMARY = FALSE

SPLIT_SUMMARY_BY = c("abr", "workload")

logprefix="data/"

SEGMENT_DURATION=4

PRINT_STALLINGS=F

POLICY_ORDER=c("eth0", "wlan0", "mptcp", "mptcp_selective", "video", "video_pessimist")
#POLICY_ORDER=c("eth0", "wlan0", "mptcp", "video", "video_pessimist")

#aggregates=c("max", "mean", "median", "count", "percentage")
#aggregates=c("count", "max", "percentage")
aggregates=c("percentage")

filterscenarios=c()

# Parse command line arguments, determine which runs will be read
logruns = runstoread(commandArgs(trailingOnly=TRUE))

logprefix=logruns$logprefix
runs = logruns$runs

OUTPUTDIR=paste(logprefix, runs[length(runs)], "/plots", sep="")

data = read_abr_runs(logprefix=logprefix, runs=runs, print=F)
idata = read_init_runs(logprefix=logprefix, runs=runs, print=F)
framedata = read_frame_runs(logprefix=logprefix, runs=runs, print=F)

data = compute_stallings(data, idata, segment_duration=SEGMENT_DURATION * 1000, verbose=F, summaryprint=PRINT_STALLINGS)
#str(data)
data = actual_stallings(data, framedata, idata, verbose=F, summaryprint=PRINT_STALLINGS)

data$dl_rate_prev = data$dl_rate_prev / 1000

data$downloadtime = data$downloadtime / 1000

data$page = data$workload

#str(data)
filename = paste(gsub("plots", "", OUTPUTDIR), "abrdata_with_stallings.log", sep="")
if (! file.exists(filename)) {
    write.table(data, file=filename, sep=",", col.names=F, row.names=F)
cat("Output data with stallings to", filename, "\n")
}

plotdata = fix.factors(subset(data, stalling > 0))
plotdata = fix.factors(subset(plotdata, plotdata$policy %in% POLICY_ORDER))
plotdata$policy = factor(plotdata$policy, POLICY_ORDER)
plotdata = plotdata[with(plotdata, order(abr, policy)),]

if (nrow(plotdata) > 0) {
    for (agg in aggregates) {
        if (agg == "percentage") {
            plotdata = data
            plotdata = fix.factors(subset(plotdata, plotdata$policy %in% POLICY_ORDER))
            plotdata$policy = factor(plotdata$policy, POLICY_ORDER)
            plotdata = plotdata[with(plotdata, order(abr, policy)),]
        }
        cat("Policies left to plot:", levels(plotdata$policy), "\n")

        cat("Aggregating stallings by", agg, "\n")
        filename = paste(OUTPUTDIR, "/stallingbarplot_", agg, sep="")

        barplottimings(plotdata, metrics="stalling_duration", mainlabel="", splitby=c("workload", "policy"), metricslabel="Mean percentage of segments stalled [%]", filename=filename, plot="eps", aggregate=agg, spacebelow=3, spaceabove=2, print=F, debug_errorbars=F)

		#plotdata$policy = factor(plotdata$policy, POLICY_ORDER)
		#plotdata = plotdata[with(plotdata, order(abr, policy)),]

		filename2 = paste(OUTPUTDIR, "/stallingbarplot_all", sep="")
		barplottimings(plotdata, metrics="stalling_duration", mainlabel="", splitby=c("scenario", "policy"), filename=filename2, plot="eps", scenariolabel=" ", metricslabel="Median percentage of segments stalled [%]", aggregate=agg, plot_errorbars=T, debug_errorbars=F, spacebelow=0.6, spaceleft=4, spaceabove=5, cex=1.5, print=F)
		cat("Plotted stallings (all) to", filename2, "\n")


        #barplottimings(plotdata, metrics="stalling_duration_all", mainlabel="", splitby=c("policy"), metricslabel="Mean percentage of segments stalled [%]", filename=filename, plot="eps", aggregate=agg, spacebelow=3, spaceabove=2, print=F, debug_errorbars=F)
        #barplottimings(plotdata, metrics="stalling_duration", mainlabel=paste(agg, ifelse(agg == "count", "stallings", ifelse(agg == "percentage", "of segments stalled", "stalling durations"))), splitby=c("abr", "policy"), filename=filename, plot="eps", aggregate=agg, spacebelow=3, spaceabove=2, print=F)
    }
} else {
    cat("No stallings -- nothing to plot!\n")
}
