#!/usr/bin/Rscript
# Plot the QoE estimate for different durations of video content

source("videodata.R")
source("../../web-timing/eval/plottimings.R")

PRINT_SUMMARY = FALSE

SPLIT_SUMMARY_BY = c("abr", "workload")

logprefix="data/"

SEGMENT_DURATION=4

POLICY_ORDER=c("eth0", "wlan0", "mptcp", "mptcp_selective", "video", "video_pessimist")
#POLICY_ORDER=c("eth0", "wlan0", "mptcp", "video", "video_pessimist")

aggregates=c("max")

filterscenarios=c()

# Parse command line arguments, determine which runs will be read
logruns = runstoread(commandArgs(trailingOnly=TRUE))

logprefix=logruns$logprefix
runs = logruns$runs

OUTPUTDIR=paste(logprefix, runs[length(runs)], "/plots", sep="")

qoedata_240 = read_qoe_runs(logprefix=logprefix, runs=runs, filename="qoe.log", print=F)
qoedata_210 = read_qoe_runs(logprefix=logprefix, runs=runs, filename="qoe_210s.log", print=F)
qoedata_180 = read_qoe_runs(logprefix=logprefix, runs=runs, filename="qoe_180s.log", print=F)
qoedata_150 = read_qoe_runs(logprefix=logprefix, runs=runs, filename="qoe_150s.log", print=F)
qoedata_120 = read_qoe_runs(logprefix=logprefix, runs=runs, filename="qoe_120s.log", print=F)

qoedata_full = qoedata_240
qoedata_full$qoe_240 = qoedata_240$qoe

q1 = merge(qoedata_240, qoedata_210, by="starttimestamp")
qoedata_full$qoe_210 = q1$qoe.y

q1 = merge(qoedata_240, qoedata_180, by="starttimestamp")
qoedata_full$qoe_180 = q1$qoe.y

q1 = merge(qoedata_240, qoedata_150, by="starttimestamp")
qoedata_full$qoe_150 = q1$qoe.y

q1 = merge(qoedata_240, qoedata_120, by="starttimestamp")
qoedata_full$qoe_120 = q1$qoe.y

comparemetrics = compute_compare_metrics(qoedata_full, c("qoe_240", "qoe_210", "qoe_180", "qoe_150", "qoe_120"), abs=T)

diffs= comparemetrics[[1]]

diffs$maxdiff =  apply(diffs, 1, function(x) { as.numeric(max(x[comparisonlabels])) })

#rundata = read_abr_runs(logprefix=logprefix, runs=runs, print=F)

#maxseg = tapply(fulldata$segmentindex, INDEX=fulldata$starttimestamp, FUN=max)

#quantile(maxseg, c(0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.15, 0.2))
# the 2% below 30 are not used anyway

#fulldata_potential = fix.factors(subset(fulldata, fulldata$maxseg > 30))

#fulldata_runs = fix.factors(subset(fulldata_potential, fulldata_potential$segmentindex ==1))

#runsbelow60 = fulldata_runs$maxseg[fulldata_runs$maxseg < 60]

#quantile(fulldata_runs$maxseg, c(0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.15, 0.2))

#table(fulldata_runs$maxseg)
#		 31  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51 
#		  2   1   2   9   5   8   8   9  24  28  12  11  15  16   8  14  10  12  19  23 
		
#		 52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  95 
#		 22  41  29  28  48  98  72 115 214 636 681 802 444 242 104  80 301  11   1 

# 		→ only having 35 or less segments occurs for 0.33% of runs
#		40: 1.62%
#		45: 3.56%
#		49: 4.61%
#		50: 5.06%
#		55: 8.46%
#		59: 16.39%

#maxseg.knots = knots(ecdf(fulldata_runs$maxseg))
#str(maxseg.knots)
#  [1] 31 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56
#  [26] 57 58 59 60 61 62 63 64 65 66 67 68 69 95
#maxseg.ecdffunction = ecdf(fulldata_runs$maxseg)
#maxseg.ecdf = maxseg.ecdffunction(maxseg.knots)

#databelow60 = fix.factors(subset(fulldata_runs, fulldata_runs$maxseg < 60))
#sort(table(databelow60$run))
# as expected, this occurs for many scenarios with factor 1 and 1.5, some with factor 2 as well, sometimes for metro with 2.5 but not often

#sort(table(databelow60$policy))

problemtimestamps = databelow60$starttimestamp

problemdiffs = fix.factors(subset(diffs, diffs$starttimestamp %in% problemtimestamps))

merged_problemdiffs = merge(problemdiffs, databelow60, by="starttimestamp")

merged_problemdiffs$scenario = as.factor(apply(merged_problemdiffs, 1, function(x) { return(get_short_scenario_string(x[["run.x"]])) } ))

diffs$scenario = as.factor(apply(diffs, 1, function(x) { return(get_short_scenario_string(x[["run"]])) } ))

# exclude that weird scenario that we do not use anyway
diffs = subset(diffs, diffs$scenario != "lesscapa")

#> diff210.cdf = ecdf(diffs[["qoe_210_-_qoe_240"]])
#> diff180.cdf = ecdf(diffs[["qoe_180_-_qoe_240"]])
#> diff150.cdf = ecdf(diffs[["qoe_150_-_qoe_240"]])
#> diff120.cdf = ecdf(diffs[["qoe_120_-_qoe_240"]])
#> diff210.knots = knots(diff210.cdf)
#> diff180.knots = knots(diff180.cdf)
#> diff150.knots = knots(diff150.cdf)
#> diff120.knots = knots(diff120.cdf)
#> diff210.ecdfdata = diff210.cdf(diff210.knots)
#> diff180.ecdfdata = diff180.cdf(diff180.knots)
#> diff150.ecdfdata = diff150.cdf(diff150.knots)
#> diff120.ecdfdata = diff120.cdf(diff120.knots)
#> diff210.cdf(0.1)
#


