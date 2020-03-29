#!/usr/bin/Rscript
# Plot the representations of the loaded segments
# i.e., show what the Adaptive Bitrate algorithm is doing

source("videodata.R")
source("../../web-timing/eval/plottimings.R")

PRINT_SUMMARY = FALSE

PLOT_HISTOGRAMS = TRUE

SPLIT_SUMMARY_BY = c("abr", "workload")

POLICY_ORDER=c("eth0", "wlan0", "mptcp", "mptcp_selective", "video", "video_pessimist")
#POLICY_ORDER=c("eth0", "wlan0", "mptcp", "video", "video_pessimist")

logprefix="data/"

SEGMENT_DURATION=4

#aggregates=c("max", "mean", "median", "count")

filterscenarios=c()

# Parse command line arguments, determine which runs will be read
logruns = runstoread(commandArgs(trailingOnly=TRUE))

logprefix=logruns$logprefix
runs = logruns$runs

OUTPUTDIR=paste(logprefix, runs[length(runs)], "/plots", sep="")

data = read_abr_runs(logprefix=logprefix, runs=runs, print=F)
idata = read_init_runs(logprefix=logprefix, runs=runs, print=F)

splitdata = split(data, data$starttimestamp)
repdiffs = lapply(splitdata, function(x) { x$rep_diff = c(0, diff(x$representation))})
#str(repdiffs)
data$representation_switches = unlist(repdiffs)
#str(data$rep_diffs)

#data = fix.factors(subset(data, abr == "BOLA_O"))

for (abralg in levels(data$abr)) {
    datatoplot = fix.factors(subset(data, abr == abralg))
    #abralg = levels(datatoplot$abr)[1]
    #policy = levels(datatoplot$policy)[1]
    #filename = paste(OUTPUTDIR, "/", startt, "_representations_", abralg, "_", policy, sep="")

    if (PRINT_SUMMARY) {
        summarizedata(datatoplot, "representation", index="policy")
    }
    if (PLOT_HISTOGRAMS) {
        #filename = paste(OUTPUTDIR, "/representations_", abralg, sep="")
        #barplottimings(datatoplot, metrics="representation", mainlabel=paste("Representations for ABR", abralg, "and policy"), splitby=c("workload", "policy"), filename=filename, plot="pdf", aggregate="hist", plot_errorbars=F, spacebelow=6, print=T)

        cat("Plotting representation switches for ABR", abralg, "\n")
        #str(datatoplot$representation_switches)
        #filename = paste(OUTPUTDIR, "/representation_switches_up_", abralg, sep="")
        #datatoplot1 = subset(datatoplot, datatoplot$representation_switches > 0)
        #splitdata = split(datatoplot1, datatoplot1$policy)
        #ymaxlist = lapply(splitdata, function(x) { max(table(x$representation_switches)) })
        #ymax = max(unlist(ymaxlist))

        #datatoplot1$policy = factor(datatoplot1$policy, POLICY_ORDER)
        #datatoplot1 = datatoplot1[with(datatoplot1, order(abr, policy)),]
        #barplottimings(datatoplot1, metrics="representation_switches", mainlabel="", splitby=c("workload", "policy"), filename=filename, plot="eps", metricslabel="Representation Switch", metricslimit=c(0, ymax), aggregate="hist", plot_errorbars=F, spacebelow=3, spaceleft=4, print=F)

        #filename = paste(OUTPUTDIR, "/representation_switches_down_", abralg, sep="")
        #datatoplot2 = subset(datatoplot, datatoplot$representation_switches < 0)
        #datatoplot2$policy = factor(datatoplot2$policy, POLICY_ORDER)
        #datatoplot2 = datatoplot2[with(datatoplot2, order(abr, policy)),]
        #splitdata = split(datatoplot2, datatoplot2$policy)
        #ymaxlist = lapply(splitdata, function(x) { max(table(x$representation_switches)) })
        #ymax = max(unlist(ymaxlist))

        #barplottimings(datatoplot2, metrics="representation_switches", mainlabel="", splitby=c("workload", "policy"), filename=filename, plot="eps", metricslabel="Representation Switch", metricslimit=c(0, ymax), aggregate="hist", plot_errorbars=F, spacebelow=3, spaceleft=4, print=F)

        filename = paste(OUTPUTDIR, "/representation_switches_", abralg, sep="")

        datatoplot = subset(datatoplot, datatoplot$representation_switches != 0)
        datatoplot$policy = factor(datatoplot$policy, POLICY_ORDER)
        datatoplot = datatoplot[with(datatoplot, order(abr, policy)),]
        splitdata = split(datatoplot, datatoplot$policy)
        ymaxlist = lapply(splitdata, function(x) { max(table(x$representation_switches)) })
        ymax = max(unlist(ymaxlist))
        barplottimings(datatoplot, metrics="representation_switches", mainlabel=paste("Representation switches for", abralg), splitby=c("workload", "policy"), filename=filename, plot="eps", metricslabel="Representation Switch Frequency", metricslimit=c(0, ymax), aggregate="hist", plot_errorbars=F, spacebelow=3, spaceleft=4, spaceabove=3, print=F)

       cat("Plotted representations to", filename, "\n")
    }
    # TODO: similar to plot_redirects, plot the median of num_occurences with a certain representation if we have multiple runs
}

filename = paste(OUTPUTDIR, "/representations", sep="")

if (PRINT_SUMMARY) {
    summarizedata(data, "representation", index="policy")
}

data$policy = factor(data$policy, POLICY_ORDER)
data = data[with(data, order(abr, policy)),]

data$representation = data$representation + 1

splitdata = split(data, data$policy:data$abr)
ymaxlist = lapply(splitdata, function(x) { max(x$representation) })
ymax = max(unlist(ymaxlist))

barplottimings(data, metrics="representation", mainlabel="", splitby=c("abr", "policy"), filename=filename, plot="eps", metricslabel="Median representation played out", metricslimit=c(1, ymax), aggregate="median", plot_errorbars=T, spacebelow=3, spaceleft=4, spaceabove=7, print=F)
cat("Plotted representations (by ABR) to", filename, "\n")

filename2 = paste(OUTPUTDIR, "/representations_all", sep="")
barplottimings(data, metrics="representation", mainlabel="", splitby=c("scenario", "policy"), filename=filename2, plot="eps", scenariolabel=" ", metricslabel="Median representation played out", metricslimit=c(0, ymax), aggregate="median", plot_errorbars=T, debug_errorbars=T, spacebelow=0.6, spaceleft=4, spaceabove=5, cex=1.5, print=F)
cat("Plotted representations (all) to", filename2, "\n")

# Different levels of starttimestamps may be present
# (e.g., there is initdata but no abr data because player failed)
idata = fix.factors(subset(idata, idata$starttimestamp %in% levels(data$starttimestamp)))
idata$policy = sapply(idata$starttimestamp, function(x) { return(head(subset(data, starttimestamp == x), n=1)$policy)} )
idata$abr = sapply(idata$starttimestamp, function(x) { return(head(subset(data, starttimestamp == x), n=1)$abr)} )

idata$policy = factor(idata$policy, POLICY_ORDER)
idata = idata[with(idata, order(abr, policy)),]

#str(idata$policy)
filename = paste(OUTPUTDIR, "/initial_playout_delay", sep="")
barplottimings(idata, metrics="initial_playout_delay", mainlabel="", splitby=c("url", "policy"), filename=filename, plot="eps", aggregate="median", plot_errorbars=T, spacebelow=6, print=F)

if (PRINT_SUMMARY) {
    summarizedata(idata, "initial_playout_delay", index=c("policy"))
}
