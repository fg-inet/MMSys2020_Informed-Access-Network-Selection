#!/usr/bin/Rscript
# Plot the QoE per ABR and policy in heatmaps

source("videodata.R")
source("../../web-timing/eval/plottimings.R")

PRINT_SUMMARY = FALSE

PLOT_BY_ABR = T

PRINT_COLORSCALE=T
PRINT_Y_AXIS=T
PRINT_DEPENDING_ON_SCENARIO=F

SHORTER_DURATION=240


TOTALMAX=4.4
TOTALMIN=1.1

PLOT="eps"

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

QOE_FILENAME = "qoe.log"

if (SHORTER_DURATION < 240) {
	QOE_FILENAME = paste("qoe_", SHORTER_DURATION, "s.log", sep="")
}

data = read_qoe_runs(logprefix=logprefix, runs=runs, filename=QOE_FILENAME, print=F)

data = fix.factors(subset(data, data$policy %in% POLICY_ORDER))

data$policy = factor(data$policy, POLICY_ORDER)
data = data[with(data, order(abr, policy)),]

for (agg in aggregates) {

	plot_heatmap(data, "qoe", x_axis="scenario", y_axis="policy", aggregate = agg, plot=PLOT, filename = paste(OUTPUTDIR, ifelse(SHORTER_DURATION < 240, paste("just_", SHORTER_DURATION, "s", sep=""), "all"), sep="/"), mainlabel="", verbose=T, add_values_as_text=T, totalmin=TOTALMIN, totalmax=TOTALMAX, colorscale="viridis", use_same_colorscale=T, printdata=F, print_ci=T, debug_ci=F, print_colorscale=ifelse((PRINT_COLORSCALE | (PRINT_DEPENDING_ON_SCENARIO & grepl("train|car|tram|shaper_harpoon2_", runs[1]))), T, F), MIDPOINT_OF_COLORSCALE=2.0, plotwidth=10, plot_y_axis=ifelse((PRINT_Y_AXIS | (PRINT_DEPENDING_ON_SCENARIO & grepl("metro|ferry|bus|_harpoon_", runs[1]))), T, F), y_axis_label="Policy", x_axis_label=ifelse(grepl("harpoon", runs[1]), "Concurrent TCP sessions", "Shaped median downstream capacity [kBit/s]"))

}

#cat("Scenario levels of all data:", levels(data$scenario), "\n")
#str(levels(data$scenario))
scenariolevels = levels(data$scenario)

if(!PLOT_BY_ABR) {
	quit()
}

for (abralg in levels(data$abr)) {
    datatoplot = fix.factors(subset(data, abr == abralg))
    cat("For ABR", abralg, "...\n")
    if (PRINT_SUMMARY) {
        summarizedata(datatoplot, "qoe", index="policy")
    }

	datatoplot$scenario=factor(datatoplot$scenario, scenariolevels)
	datatoplot=datatoplot[with(datatoplot, order(abr, policy, scenario)),]
	#cat("Scenario levels for", abralg, ":", levels(datatoplot$scenario), "\n")
	#str(levels(datatoplot$scenario))

	plot_heatmap(datatoplot, "qoe", x_axis="scenario", y_axis="policy", aggregate = agg, plot=PLOT, filename = paste(OUTPUTDIR, paste(abralg, ifelse(SHORTER_DURATION < 240, paste("_", SHORTER_DURATION, "s", sep=""), "")), sep="/"), mainlabel="", verbose=T, add_values_as_text=T, totalmin=TOTALMIN, totalmax=TOTALMAX, colorscale="viridis", use_same_colorscale=T, printdata=F, print_ci=T, debug_ci=F, print_colorscale=ifelse((PRINT_COLORSCALE | (PRINT_DEPENDING_ON_SCENARIO & grepl("train|car|tram|shaper_harpoon2_", runs[1]))), T, F), MIDPOINT_OF_COLORSCALE=2.0, plotwidth=10, plot_y_axis=ifelse((PRINT_Y_AXIS | (PRINT_DEPENDING_ON_SCENARIO & grepl("metro|ferry|bus|_harpoon_", runs[1]))), T, F), y_axis_label="Policy", x_axis_label=ifelse(grepl("harpoon", runs[1]), "Concurrent TCP sessions", "Shaped median downstream capacity [kBit/s]"))

    cat("\n")
}
