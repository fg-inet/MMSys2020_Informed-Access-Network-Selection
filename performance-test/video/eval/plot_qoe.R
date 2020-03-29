#!/usr/bin/Rscript
# Plot the QoE per ABR and policy

source("videodata.R")
source("../../web-timing/eval/plottimings.R")

PRINT_SUMMARY = FALSE

SPLIT_SUMMARY_BY = c("abr", "workload")

logprefix="data/"

SEGMENT_DURATION=4

#POLICY_ORDER=c("eth0", "wlan0", "mptcp", "mptcp_selective", "video", "video_pessimist")
#POLICY_ORDER=c("eth0", "wlan0", "mptcp", "video", "video_pessimist")
POLICY_ORDER=c("network1", "network2", "optimist", "pessimist")

aggregates=c("median")
#aggregates=c("max", "mean", "median")

filterscenarios=c()

# Parse command line arguments, determine which runs will be read
logruns = runstoread(commandArgs(trailingOnly=TRUE))

logprefix=logruns$logprefix
runs = logruns$runs

OUTPUTDIR=paste(logprefix, runs[length(runs)], "/plots", sep="")

data = read_qoe_runs(logprefix=logprefix, runs=runs, print=F)

data = fix.factors(subset(data, data$policy %in% POLICY_ORDER))

data$policy = factor(data$policy, POLICY_ORDER)
data = data[with(data, order(abr, policy)),]

for (agg in aggregates) {
    cat("Aggregating QoE by", agg, "\n")
    filename = paste(OUTPUTDIR, "/qoebarplot_", agg, ifelse(length(runs) > 1, "_all", ""), sep="")

    barplottimings(data, metrics="qoe", mainlabel="", splitby=c("abr", "policy"), filename=filename, plot="eps", metricslabel=plotlabel.human.readable(paste(agg, "qoe")), aggregate=agg, spacebelow=3, spaceabove=2, print=F)

	filename2 = paste(OUTPUTDIR, "/qoebarplot_all", sep="")
    barplottimings(data, metrics="qoe", mainlabel="", splitby=c("scenario", "policy"), filename=filename2, plot="eps", metricslabel=plotlabel.human.readable(paste(agg, "qoe")), aggregate=agg, metricslimit=c(0, 5), spacebelow=0.6, spaceabove=0.1, spaceleft=4.5, scenariolabel=" ", cex=1.5, print=T)
}
