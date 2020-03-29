#!/usr/bin/Rscript

logprefix="data/"

runs=list.files(path=logprefix, pattern="^run*")

usedruns = str(runs[c(seq(5, 19), seq(25, 68), seq(70, 87), seq(89, 91), seq(93, 94), seq(103, 797))])

fulldata = read_abr_runs(logprefix=logprefix, runs = usedruns)

# with 777 runs: 13865 individual experiments

# compute maximum segmentindex for each load (= each starttimestamp)
maxseg = tapply(fulldata$segmentindex, INDEX=fulldata$starttimestamp, FUN=max)
maxseg_df = data.frame (starttimestamp = names(maxseg), maxseg = as.integer(maxseg))
fulldata_merged = merge(fulldata, maxseg_df)

# the loads with segments below 30 are not used anyway -- filter them out
fulldata_potential = fix.factors(subset(fulldata_merged, fulldata_merged$maxseg >= 30))
# now we have 13594 individual loads with at least 30 segments


# want only one datapoint for each load
fulldata_runs = fix.factors(subset(fulldata_potential, fulldata_potential$segmentindex ==1))

# how many loads have less than 60 segments?
runsbelow60 = fulldata_runs$maxseg[fulldata_runs$maxseg < 60]
# 2306 runs ~= 16.96%

quantile(fulldata_runs$maxseg, c(0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.15, 0.2))
#  1%  2%  3%  4%  5% 10% 15% 20% 
#  38  40  42  43  45  56  59  60 
# --> just 1% had 38 or less segments...

# Compute ECDF

maxseg.ecdffunction = ecdf(fulldata_runs$maxseg)
maxseg.knots = knots(ecdf(fulldata_runs$maxseg))
maxseg.ecdf = maxseg.ecdffunction(maxseg.knots)

maxseg.knots # these are the maximum segments that occur
#  [1] 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54
#  [26] 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69

maxseg.ecdf # these are the percentages for each maximum segment
#  [1] 0.002574665 0.002942475 0.003457408 0.003604531 0.004193026 0.005811387
#  [7] 0.006988377 0.009415919 0.011549213 0.014638811 0.020523760 0.028321318
# [13] 0.034426953 0.041341768 0.048330146 0.053405914 0.057893188 0.061644843
# [19] 0.063925261 0.068338973 0.072311314 0.077239959 0.080476681 0.087538620
# [25] 0.093938502 0.099897013 0.108503752 0.126673532 0.145799617 0.169633662
# [31] 0.207150213 0.308003531 0.465352361 0.657054583 0.800941592 0.878475798
# [37] 0.905546565 0.922097984 0.985140503 1.000000000


table(fulldata_runs$maxseg) # number of occurences of each "maxseg" value
#   30   31   32   33   34   35   36   37   38   39   40   41   42   43   44   45 
#   35    5    7    2    8   22   16   33   29   42   80  106   83   94   95   69 
# 
#   46   47   48   49   50   51   52   53   54   55   56   57   58   59   60   61 
#   61   51   31   60   54   67   44   96   87   81  117  247  260  324  510 1371 
# 
#   62   63   64   65   66   67   68   69 
# 2139 2606 1956 1054  368  225  857  202 

databelow60 = fix.factors(subset(fulldata_runs, fulldata_runs$maxseg < 60))

# For which runs does it occur that we have less than 60 segments?
sort(table(databelow60$run))

# For which policies does it occur?
sort(table(databelow60$policy))
#mptcp_selective           mptcp video_pessimist            eth0           video 
#            170             207             264             325             465 
#          wlan0 
#            875 

