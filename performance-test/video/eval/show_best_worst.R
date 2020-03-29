#!/usr/bin/Rscript
# Show the runs with the best and worst QoE by ABR, policy, and scenario

source("videodata.R")
source("../../web-timing/eval/plottimings.R")

PRINT_SUMMARY = FALSE

SPLIT_SUMMARY_BY = c("abr", "workload", "scenario")

PRINT_BEST=FALSE

PRINT_WORST=TRUE

logprefix="data/"

ONLY_ABR=c()
ONLY_POLICY=c("wlan0")
ONLY_SCENARIO=c()

SHOW_BY_ABR=F
SHOW_BY_POLICY=F
SHOW_BY_SCENARIO=T

POLICY_ORDER=c("eth0", "wlan0", "mptcp", "mptcp_selective", "video", "video_pessimist")
#POLICY_ORDER=c("eth0", "wlan0", "mptcp", "video", "video_pessimist")

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

data$sess = sapply(as.character(data$scenario), plotlabel.human.readable)

if (length(ONLY_ABR) > 0) {
	data = fix.factors(subset(data, data$abr %in% ONLY_ABR))
}

if (length(ONLY_POLICY) > 0) {
	data = fix.factors(subset(data, data$policy %in% ONLY_POLICY))
}

if (length(ONLY_SCENARIO) > 0) {
	data = fix.factors(subset(data, any(grepl(as.character(ONLY_SCENARIO), data$scenario))))
}

splitfactor = c()

if (SHOW_BY_SCENARIO) {
	splitfactor = c(splitfactor, "scenario")
}
if (SHOW_BY_ABR) {
	splitfactor = c(splitfactor, "abr")
}
if (SHOW_BY_POLICY) {
	splitfactor = c(splitfactor, "policy")
}
splitdata = split_dataframe_by_factors(data, splitfactor, print=T)

showdata = splitdata$data


if (PRINT_BEST) {
	cat("\tBest runs:\n")
	for (dataitem in showdata) {
		print_top_n(dataitem, "qoe", printcolumns=c("starttimestamp", "policy", "abr", "sess"))
	}
}

if (PRINT_WORST) {
	cat("\tWorst runs:\n")
	for (dataitem in showdata) {
		print_top_n(dataitem, "qoe", printcolumns=c("starttimestamp", "policy", "abr", "sess"), rev=T)
	}
}
