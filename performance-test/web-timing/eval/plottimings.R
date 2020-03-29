#!/usr/bin/Rscript
# Plots timings logged for fetching web pages on the multi access testbed
# by Theresa Enghardt

# Functions that need to be sourced - should exist in the top level directory
# of this repository
if (file.exists("../../R_functions.R")) {
    source("../../R_functions.R")
}

# Read timing data from testbed runs
readtimings <- function(logfile = "testdata/debian.org.har.log", print=TRUE) {
	timingdata <- read.csv(logfile, header=F)

	# Set column names
	colnames(timingdata) = c("filesize", "number", "page", "run", "timestamp", "policy", "if1_bw", "if1_rtt", "if2_bw", "if2_rtt", "computed_time", "actual_time", "simulated_time")

	# Make factors from numbers
	timingdata$filesize = factor(timingdata$filesize)
	timingdata$number = factor(timingdata$number)
	timingdata$if1_bw = factor(timingdata$if1_bw)
	timingdata$if2_bw = factor(timingdata$if2_bw)
	timingdata$if1_rtt = factor(timingdata$if1_rtt)
	timingdata$if2_rtt = factor(timingdata$if2_rtt)
	timingdata$policy = factor(timingdata$policy, c("if1", "if2", "eaf"))

	# Set workload factor: either artificial page (kittens.com) or actual web pages
	if (timingdata$page[[1]] == "kittens.com") {
		timingdata$workload = factor(timingdata$filesize:timingdata$number, unique(timingdata$filesize:timingdata$number))
		timingdata$workload = factor(timingdata$workload, rev(c("10k:4", "10k:8", "10k:16", "10k:32", "100k:4", "100k:8", "100k:32", "100k:64", "200k:32", "425k:32", "1mb:2", "1mb:4", "1mb:8", "1mb:32")))
	} else {
		timingdata$workload = timingdata$page
	}

	# Delete columns filesize and number - they were just needed to set the workload factor for artificial pages
	timingdata$filesize = NULL
	timingdata$number = NULL

	# Compute absolute and relative differences of testbed timings and simulated timings
	timingdata = compute_diffs(timingdata)

	# Sort data by workload and policy
	timingdata=timingdata[with(timingdata, order(workload, policy, decreasing=TRUE)),]

    if (print) {
        str(timingdata)
    }
	return(timingdata)
}

readobjecttimings <- function(logprefix="data/run-2017-01-26T14:19-guest-wired/", print=TRUE) {
	timingdata = data.frame()
	timinglist = list()

	files = list.files(path=logprefix, pattern="*-objects.log")
	if (length(files) == 0) {
		cat("No files with object timings found!\n")
		return(NULL)
	}

	if (grepl("shaper", logprefix)) {
		shaperscenarios = getshaperscenarios(logprefix, length(files))
        if (print) {
            cat("Got shaper scenarios:", shaperscenarios, "\n")
        }
	}

	for (i in 1:length(files)) {
		iface = gsub("-objects.log","",gsub("rawtimings-","",files[i]))
        if (print) {
            cat("Interface", i, ":", iface, "\n")
        }
		tdata = read.csv(paste(logprefix,files[i], sep=""), header=FALSE)
		colnames(tdata) = c("file","requesttype","starttimestamp", "resolvetime","connecttime","filesize", "downloadtime","retrievetime","cumulative_retrieve_time")
		tdata$iface = as.factor(iface)
		if (grepl("shaper", logprefix)) {
			tdata$policy = as.factor(shaperscenarios[i])
			#tdata$policy = tdata$policy:tdata$iface
		} else {
			tdata$policy = tdata$iface
		}
        if (print) {
            str(tdata$file)
        }
		numberofrepetitions = table(tdata$file)[1]
		numberofobjects = length(tdata$file) / numberofrepetitions
        if (print) {
            cat("Found", numberofrepetitions, "repetitions and", numberofobjects, "files\n")
        }
		tdata$try = rep(1:numberofrepetitions, rep(numberofobjects, numberofrepetitions))
		tdata$actual_time = tdata$retrievetime

		timinglist[[i]] = tdata
	}
	timingdata = do.call(rbind, timinglist)
    if (print) {
        str(timingdata)
    }
	return(timingdata)
}

readpagetimings <- function(logprefix=list.files(path="data/", pattern="run-*")[1], filename="final_timings.log", print=TRUE) {
	timingdata = data.frame()
	timinglist = list()

    colnames_to_use=c("page", "policy", "scenario", "starttimestamp", "fetchStart", "responseStart", "domInteractive", "domContentLoadedEventStart", "domContentLoadedEventEnd", "domComplete", "loadEventStart", "loadEventEnd", "firstPaint", "domContentFlushed",
        "har_number_of_requests", "har_finished_after_onload", "har_no_reply", "har_status1xx", "har_status200", "har_status_other2xx", "har_status3xx", "har_status4xx", "har_status5xx", "har_unknown_status", "har_non_failed_requests", "har_non_failed_get_requests", "har_starttime",
        "har_first200starttime", "har_redirects_before_first_200", "har_last_request_start_before_onload", "har_last_resource_end_before_onload", "har_onload_time", "har_content_load_time",
		"har_object_index", "har_byte_index_bodysize", "har_byte_index_bodyorcontent", "har_byte_index_transfersize",
        "har_sum_of_respbodysize", "har_sum_of_contentlength", "har_sum_of_contentsize", "har_sum_of_bodyorcontent", "har_sum_of_transfersize",
        "res_number_of_resources", "res_finished_after_onload", "res_number_of_resources_finished_before_onload", "res_last_resource_end_before_onload",
        "res_sum_of_encoded", "res_sum_of_decoded",
		"res_object_index", "res_byte_index",
        "ATF_time", "ATF_imageindex_PLT", "ATF_imageindex_ATF", "ATF_objectindex_PLT", "ATF_objectindex_ATF", "ATF_byteindex_PLT", "ATF_byteindex_ATF",
        "har_byteindex_ATF", "har_objectindex_ATF",
		"smart_total_page_size") # as exported by computetiming.py
	files = list.files(path=logprefix, pattern=filename)
    if (print) {
        cat("Found files matching", filename, ":", files, "\n")
    }
	if(length(files) == 0) {
        cat("No files found under", logprefix, "! Returning NULL \n")
        return(NULL)
	}
	if (grepl("shaper", logprefix)) {
		shaperscenarios = getshaperscenarios(logprefix, length(files))
	}

	for (i in 1:length(files)) {
		filename = paste(logprefix,files[i], sep="")
		fileinfo = file.info(filename)
		if (fileinfo$size == 0) {
			cat("File",filename,"is empty! Returning NULL\n")
			return(NULL)
		}
        if (print) {
            cat("Reading file", paste(logprefix, files[i], sep=""), "\n")
        }
		tdata = read.csv(paste(logprefix,files[i], sep=""), header=FALSE)
		colnames(tdata) = colnames_to_use
        tdata$client = as.factor("browser")
        tdata$iface = tdata$policy
        tdata$actual_time = tdata$loadEventEnd
		if (grepl("shaper", logprefix)) {
			if (print) {
				cat("Policy :", shaperscenarios[i], "\n")
			}
			tdata$policy = as.factor(shaperscenarios[i])
            splitscenariostring = strsplit(shaperscenarios[i], "[-_]")[[1]]
            #cat("Shaperscenario: ", shaperscenarios[i], "\n")
            tdata$shaped_bw = as.factor(splitscenariostring[2])
            tdata$shaped_rtt = as.factor(splitscenariostring[3])
            tdata$shaped_loss = as.factor(splitscenariostring[4])
		} else {
			tdata$policy = tdata$iface
		}

		tdata$workload = gsub("http://.*/pics-","",tdata$page)
		tdata$workload = gsub("*.html","",tdata$workload)
		tdata$workload = as.factor(gsub("-",":",tdata$workload))
        tdata$starttimestamp_numeric = as.numeric(as.POSIXct(gsub("T", " ", tdata$starttimestamp)))
        tdata = tdata[with(tdata, order(tdata[["starttimestamp_numeric"]])),]

        number_of_ifaces = length(levels(tdata$iface))
        number_of_workloads = length(levels(tdata$workload))
        number_of_samples = length(tdata$actual_time)
        number_of_tries = ceiling(number_of_samples / (number_of_ifaces * number_of_workloads))
        if (print) {
            cat("Ifaces:", number_of_ifaces, "workloads:", number_of_workloads, "samples:", number_of_samples, "estimated tries: (x", number_of_tries, ") ", number_of_ifaces * number_of_workloads * number_of_tries, "\n")
        }
        #try_estimate = as.factor(rep(1:number_of_tries, rep(number_of_ifaces * number_of_workloads, number_of_tries)))
        #if (length(try_estimate) == number_of_samples) {
        #    tdata$try = try_estimate
        #} else {
        #    if (length(unique(try_estimate)) == 1) {
        #        tdata$try = as.factor(1)
        #    } else {
                # Get the index for the first page fetched and see where it repeats
                tdata = tdata[with(tdata, order(tdata[["starttimestamp_numeric"]])),]
                if (length(levels(tdata$page)) == 1) {
                    # Only one page -- do runs based on policy instead
                    try_based_on = "policy"
                } else {
                    try_based_on = "page"
                }
                value1 = as.character(tdata[1,][[try_based_on]])
                value1inds = which(tdata[[try_based_on]] == value1)
                w = which(diff(which(tdata[[try_based_on]] == value1)) > 1) + 1
                trylengths = diff(c(1, value1inds[w], number_of_samples+1))
                maxlength = length(levels(tdata[[try_based_on]]))
                if (any(trylengths > maxlength)) {
                    # One of the trylengths is too long -- cap it
                    newtrylengths = c()
                    for (j in trylengths) {
                        if (j <= maxlength) {
                            newtrylengths = c(newtrylengths, j)
                        } else {
                            if (print) {
                                cat("Splitting try", j, "because maxlength is", maxlength, "\n")
                            }
                            while (j > 0) {
                                newtrylengths = c(newtrylengths, min(maxlength, j))
                                j = j - maxlength
                            }
                        }
                    }
                    trylengths = newtrylengths
                }

                if (print) {
                    cat("First", try_based_on, ":", value1, "-- new runs start at", value1inds[w], "and trylengths =", trylengths, "\n")
                }

                try_maybe = rep(1:length(trylengths), trylengths)
                if (all(trylengths == 1)) {
                    # Only trylengths of 1 -- perhaps we split too much and
                    # this is just one try in total
                    try_maybe = rep(1, length(trylengths))
                }
                if (length(try_maybe) == number_of_samples) {
                    tdata$try = as.factor(try_maybe)
                } else {
                    if (print) {
                        cat("Still length mismatch: try_maybe is", length(try_maybe), "and number_of_samples is", number_of_samples, "\n")
                    }
                    tdata$try = NA
                }
            #}
        #}

		timinglist[[i]] = tdata
	}
	timingdata = do.call(rbind, timinglist)

	#policylogfile = paste(logprefix, "policy_log.csv", sep="")
	#fileinfo = file.info(policylogfile)
	#if (is.na(fileinfo$size) || fileinfo$size == 0) {
    #    if (print) {
    #        cat("File",policylogfile,"is empty!\n")
    #    }
	#} else {
    #    pdata = read.csv(policylogfile, header=FALSE)
    #    colnames(pdata) = c("try", "policytimestamp", "ip.dst", "ip.src")
    #    pdata$iface = substitute_addresses_by_interfaces(pdata$ip.src)
    #    #timingdata=merge(timingdata, pdata, by=c("try", "iface"))
    #}

	if (print) {
		str(timingdata)
	}
	return(timingdata)
}

readpageruns <- function(logprefix="data/", runs=list.files(path=logprefix, pattern="^run*"), filename="final_timings.log", print=FALSE) {
	if (print) {
		cat("Got logprefix", logprefix, "\n")
	}

	timingdata = data.frame()
	timinglist = list()

	for (i in 1:length(runs)) {
		run = runs[i]
		cat("  [", i, "/", length(runs), "] Reading in data from", run, "\n")
		data = readpagetimings(paste(logprefix, run, "/", sep=""), filename=filename, print=print)
		if (is.null(data)) {
			next
		}
		gsub("run-", "", run)
		datematch = regexpr("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}-",run)
		dateofrun = substring(run, datematch, datematch+attr(datematch, "match.length")-2)
		data$run = as.factor(run)
		data$methodology = as.factor(get_methodology_label(run))
		data$browser = as.factor(get_browser_label(run))
		data$workload = as.factor(get_workload(run))
		gsub(dateofrun, "", run)
		scenario = as.factor(substring(run, datematch+attr(datematch, "match.length")))
		if (print) {
			cat("  Dateofrun:", dateofrun, "scenario:", scenario, "\n")
		}
		data$scenario = scenario
		s = strsplit(run, "[/_:+-]")[[1]]
		latencyindex = which(grepl("ms", s))
		bandwidthindex = which(grepl("Mbit", s))
		if (length(latencyindex) == 2 && length(bandwidthindex) == 2) {
			data$if1_rtt = as.factor(s[latencyindex[1]])
			data$if2_rtt = as.factor(s[latencyindex[2]])
			data$if1_bw = as.factor(s[bandwidthindex[1]])
			data$if2_bw = as.factor(s[bandwidthindex[2]])
		} else {
			data$if1_rtt = NA
			data$if2_rtt = NA
			data$if1_bw = NA
			data$if2_bw = NA
		}
		#if (!grepl("shaper", run)) {
		#	data$policy = as.factor(paste(data$iface,data$scenario,sep="_"))
		#} else {
		#	data$policy = data$iface:data$policy
		#}
		data$policy = data$iface

		data[["PLT_with_redirects"]] = data$loadEventStart
		data[["PLT_without_redirects"]] = data$loadEventStart - data$fetchStart

		data[["TTFP_with_redirects"]] = data$domContentFlushed
		data[["TTFP_without_redirects"]] = data$domContentFlushed - data$fetchStart

		data[["old_firstPaint_with_redirects"]] = data$firstPaint
		data[["old_firstPaint_without_redirects"]] = data$firstPaint - data$fetchStart

		data[["MOS_PLT"]] = -0.5368 * log(data[["PLT_without_redirects"]]) + 7.9035
		data[["MOS_ByteIndex_ATF"]] = -0.4731 * log(data[["har_byteindex_ATF"]]) + 7.0813

		timinglist[[i]] = data
	}
    timinglistlength1 = length(timinglist[[1]])
    if (all(lapply(timinglist, function(x) length(x)) == timinglistlength1)) {
        # All list items are similar - we can combine them to a single data frame
        timingdata = do.call(rbind, timinglist)
    } else {
        timinglist = timinglist[vapply(timinglist, Negate(is.null), NA)]
        cat("Warning: Lengths of data items do not match (different clients?)\n")
        cat("Some columns were filled with NAs.\n")
        timingdata = do.call(rbind, filldataframes(timinglist))
    }

	if (any(grepl("MAM", runs))) {
		mam = sapply(timingdata$scenario, function(x) {matchobj=regexpr("[A-Z]+", x); return(ifelse(attr(matchobj, "match.length") > 2 , substring(x, matchobj, matchobj+attr(matchobj, "match.length")-1), "MAM"))})
		timingdata$mam = factor(mam)
		timingdata$policy = timingdata$iface:timingdata$policy:timingdata$mam
	}
	if (print) {
		str(timingdata)
	}
	return(timingdata)
}

readobjectruns <- function(logprefix="data/", runs=list.files(path=logprefix, pattern="^run*"), print=TRUE) {

	timingdata = data.frame()
	timinglist = list()

	for (i in 1:length(runs)) {
		run = runs[i]
		cat("  [", i, "/", length(runs), "] Reading in object load times from", run, "\n")
		data = readobjecttimings(paste(logprefix, run, "/", sep=""), print=FALSE)
		if (is.null(data)) {
			next
		}
		gsub("run-", "", run)
		datematch = regexpr("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}-",run)
		dateofrun = substring(run, datematch, datematch+attr(datematch, "match.length")-2)
		data$run = as.factor(run)
		gsub(dateofrun, "", run)
		scenario = as.factor(substring(run, datematch+attr(datematch, "match.length")))
		if (print) {
			cat("  Dateofrun:", dateofrun, "scenario:", scenario, "\n")
		}
		data$scenario = scenario
		if (!grepl("shaper", run)) {
			data$policy = as.factor(paste(data$iface,data$scenario,sep="_"))
		} else {
			data$policy = data$iface:data$policy
		}

		timinglist[[i]] = data
	}
	timingdata = do.call(rbind, timinglist)

	if (any(grepl("MAM", runs))) {
		mam = sapply(timingdata$scenario, function(x) {matchobj=regexpr("[A-Z]+", x); return(ifelse(attr(matchobj, "match.length") > 2 , substring(x, matchobj, matchobj+attr(matchobj, "match.length")-1), "MAM"))})
		timingdata$mam = factor(mam)
		timingdata$policy = timingdata$iface:timingdata$policy:timingdata$mam
	}
	if (print) {
		str(timingdata)
	}
	return(timingdata)
}

exporttimes <- function(logprefix="data/", runs=list.files(path=logprefix, pattern="^run*"), outfile="../../timings.Rdata", print=FALSE) {
    pagedata = readpageruns(logprefix=logprefix, runs=runs, print=print)
    objdata = readobjectruns(logprefix=logprefix, runs=runs, print=print)
    save(pagedata, objdata, file=outfile)
    cat("Exported page and object load times to", outfile, "\n")
}

# For a list of runs, get objects where timings differ based on "split"
which_objects_differ <- function(logprefix="data/", runs=list.files(path=logprefix, pattern="^run*"), split="iface", metrics="actual_time", print=FALSE, diffthreshold=0.01) {
    listofdiffs = sapply(runs, function(x) { cat ("Run", x, "...\n"); findobjdiff(logprefix=logprefix, run=x, split=split, metrics=metrics, print=print, diffthreshold=diffthreshold) } )

    return(listofdiffs[sapply(listofdiffs, length) > 0])
}

# Load object timings of a specific run and check per-object metrics
# Return list of objects where medians differ based on "split" parameter
# CAUTION: Currently only checks first two values of this "split" parameter.
# Also, check for outliers and display them
findobjdiff <- function(logprefix="data/", run=tail(list.files(path=logprefix, pattern="^run*"), n=1), split="iface", metrics="actual_time", print=FALSE, diffthreshold = 0.01) {

    # Read in data of this run, split according to objects/files first
    timingdata = readobjecttimings(logprefix=paste(logprefix, run, "/", sep=""), print=print)
    splittimingdata = split(timingdata, timingdata$file)

    diffobjs = list()

    # Go through objects/files
    for (i in seq(1, length(splittimingdata))) {
        filedata = splittimingdata[[i]]
        file = names(splittimingdata)[i]

        # Split accrording to "split" criterion
        splitdata = split(filedata, filedata[[split]])

        medians = sapply(splitdata, function(x) median(x[[metrics]]))
        medianofmedians = median(medians)
        mediandiffs = sapply(medians, function(x) abs(x - medianofmedians))
        #cat("Medians:", medians, "\n")
        #cat("Median of medians: ", medianofmedians, "\n")
        #cat("Mediandiffs: ", mediandiffs, "\n")

        if (any(mediandiffs > diffthreshold)) {
            cat("\tSignificant difference based on", split, "of median", metrics, "\n\t\tfor", file, ": ")
            for (i in seq(1, length(medians))) {
                if (mediandiffs[i] > diffthreshold) {
                    cat("[", names(splitdata)[[i]], "] -> ", medians[i], " ", sep="")
                }
            }
            cat("\n")
            diffobjs = append(diffobjs, splittimingdata[i])
        }

        outliers = lapply(splitdata, function(x) findoutlier(x, metrics=metrics, print=FALSE))
        for (i in seq(1, length(outliers))) {
            if (!is.null(outliers[[i]])) {
                cat("\n\tFound outlier(s) for", split, names(splitdata)[[i]], "from median", medians[i], ":\n")
                print(outliers[[i]])
                cat("\n")
            }
        }
    }
    if (print) {
        sumbyobj = with(timingdata, tapply(X=actual_time, IND=file:iface, FUN=summary))
        cat("\n")
        print(sumbyobj)
        cat("\n")
    }

    if (length(diffobjs) > 0) {
        cat("\n")
    }

    return (diffobjs)
}

# Make a boxplot of timings in a data set, indexed by workload and policy
boxplottimings <- function(timingdata, filename="boxplot.pdf", log="", pages=NULL, metrics="wallclock_time", splitby=c("policy", "workload"), cex=1, mainlabel="", ylabel="", plot="terminal", exclude="", spacebelow=22) {

	if (is.null(pages)) {
		if (is.null(timingdata$page) && !is.null(timingdata$file)) {
			timingdata$page = timingdata$file
			timingdata$workload = timingdata$file
		}
		pages = levels(timingdata$page)
	}

	if (!is.null(timingdata$page)) {
        pagestosubset = c()
        for (page in pages) {
            if (!(page %in% timingdata$page)) {
                # "Pages" may be just a descriptor
                pagestosubset = c(pagestosubset, get_pages_by_description(page, levels(fix.factor(timingdata$page))))
            } else {
                pagestosubset = c(pagestosubset, page)
            }

        }

        if (mainlabel == "" && length(pages) == 1) {
            mainlabel = paste("Comparison of scenarios for", pages, "pages")
        }
        #cat("Boxplot of", metrics, "for", pagestosubset, "\n")

		# Subset the pages
		timingdata = subset(timingdata, (page %in% pagestosubset))
		timingdata$page = factor(timingdata$page, unique(timingdata$page))
		timingdata$workload = timingdata$page
		pages = levels(timingdata$page)
	} else {
		cat("WARNING: Cannot filter out pages from data!\n")
	}

	if (mainlabel == "") {
        mainlabel = paste("Comparison of scenarios for", paste(pages, collapse=", "))
	}
    if (ylabel == "") {
        ylabel=gsub("_", " ", paste(metrics, "[s]"))
    }
	boxplotmetrics(timingdata, filename=filename, metrics=metrics, log=log, splitby=splitby, cex=cex, mainlabel=mainlabel, ylabel=ylabel, plot=plot, spacebelow=spacebelow, exclude=exclude)
}

# Make a bar plot of timings in a data set
#    Options:
#        metrics		What timing metrics to plot (default: actual_time)
#        aggregate		How to aggregate timings for same workload and policy (default: median)
#        horiz			Draw a horizontal barplot, bars from left to right? (default: FALSE)
#        metricslabel	What to call the timing metrics axis
#        plotmetricslog Draw a logarithmic metrics axis? (default: FALSE)
#        scenariolabel  What to call the scenario axis
#        metricslimit	Limit of the metrics axis
#        plotlegend		Plot a legend explaining the scenarios? (default: TRUE)
#        plotsimplifiedscenarioaxis 	On the scenario axis, plot only one label for every scenario (3 bars)
#        plotscenarioaxis				On the scenario axis, plot a label for every bar
#
barplottimings <- function(timingdata, metrics="actual_time", mainlabel="Timings per scenario", names=c(), policynames=c(), splitby=c("workload", "policy"), cex=1, aggregate="median", plot_errorbars=TRUE, debug_errorbars=F, horiz=FALSE, metricslabel=paste(aggregate, metrics, "[s]"), plotmetricslog=FALSE, scenariolabel="", metricslimit=c(), plotlegend=TRUE, plotsimplifiedscenarioaxis=TRUE, plotscenarioaxis=FALSE, filename="barplot", plot="", spacebelow=4, spaceleft=4, spaceabove=0.3, print=FALSE) {

	if(print) {
		cat(ifelse(horiz, "Horizontal ", "Vertical "))
		cat("Barplot of", metrics, "\n")
	}

	# Compute aggregate timings - default is median - and error bars
	if (aggregate != "" & aggregate != "hist") {
		if (print & "workload" %in% splitby) {
			cat("Order of pages within function:", levels(timingdata$page), "\n")
		}
		if (splitby[1] != "workload") {
			timingdata$page = timingdata[[splitby[1]]]
        }
        if (print) {
            cat("Order of", splitby[1], ":", levels(timingdata[[splitby[1]]]), "\n")
            cat("Order of", splitby[2], ":", levels(timingdata[[splitby[2]]]), "\n")
        }
		plotdata = c()
		errorbar_lower = c()
		errorbar_upper = c()
        splitlevels = levels(timingdata[[splitby[2]]])

		for (thispage in levels(timingdata$page)) {
			thispagedata = fix.factors(subset(timingdata, page == thispage))
			if (print) {
                #str(thispagedata)
				cat("Adding data for", thispage, ":", thispagedata[[metrics]], "\n")
			}
            if (aggregate == "count") {
                newplotdata = tapply(X=thispagedata[[metrics]], IND=thispagedata[[splitby[2]]], FUN = function(x) { length(x[x > 0]) })
                #str(newplotdata)
                plot_errorbars=F
            } else if (aggregate == "percentage") {
				#plotdatabystartt = split(thispagedata, thispagedata$starttimestamp)
				#cat("Plotdata by startt:", plotdatabystartt, "\n")
				segments_stalled = tapply(X=thispagedata[[metrics]], IND=thispagedata[[splitby[2]]]:thispagedata$starttimestamp, FUN = function(x) { length(x[x > 0]) })
				total_segments = tapply(X=thispagedata[[metrics]], IND=thispagedata[[splitby[2]]]:thispagedata$starttimestamp, FUN = function(x) { length(x) })
				levels = levels(thispagedata[[splitby[2]]]:thispagedata$starttimestamp)[!is.na(segments_stalled)]
				segments_stalled = segments_stalled[!is.na(segments_stalled)]
				total_segments = total_segments[!is.na(total_segments)]
				# Only use data if we loaded sufficient segments
				HALF_MAX = max(total_segments) / 2
				levels = levels[total_segments > HALF_MAX]
				segments_stalled = segments_stalled[total_segments > HALF_MAX]
				total_segments = total_segments[total_segments > HALF_MAX]
				policylevels = unlist(sapply(levels, function(x) { x1 = strsplit(x, ":"); return(x1[[1]][1]) }))
				all_percentages = unlist(sapply(levels(thispagedata[[splitby[2]]]), function(x) { thisseg = segments_stalled[x == policylevels]; allsegs = total_segments[x == policylevels]; if (print) { cat(x, "stalled", thisseg, "/", allsegs, "\n") }; return(thisseg / allsegs) }))
				if (print) {
					cat("Segments stalled:", segments_stalled, "\n")
					cat("Total segments:", total_segments, "\n")
					cat("Levels:", levels, "\n")
					cat("Policylevels:", policylevels, "\n")
					cat("All percentages", all_percentages, "\n")
				}
				newplotdata = unlist(sapply(levels(thispagedata[[splitby[2]]]), function(x) { thisseg = segments_stalled[x == policylevels]; allsegs = total_segments[x == policylevels]; return(median(thisseg / allsegs)) })) * 100
                newplotdata_old = tapply(X=thispagedata[[metrics]], IND=thispagedata[[splitby[2]]], FUN = function(x) { length(x[x > 0]) / length(x) }) * 100
				#cat("New plotdata old", newplotdata_old, "\n")
				if (plot_errorbars) {
					# This is for mean:
					#new_errorbar_lower = newplotdata - unlist(sapply(levels(thispagedata[[splitby[2]]]), function(x) { thisseg = segments_stalled[x == policylevels]; allsegs = total_segments[x == policylevels]; percentages = (thisseg / allsegs) * 100; n = length(percentages); if (n < 2) { return(percentages[1]) }; return(mean(percentages) + qnorm(0.975) * sd(percentages) / sqrt(n)) }))
					#new_errorbar_upper = newplotdata - unlist(sapply(levels(thispagedata[[splitby[2]]]), function(x) { thisseg = segments_stalled[x == policylevels]; allsegs = total_segments[x == policylevels]; percentages = (thisseg / allsegs) * 100; n = length(percentages); if (n < 2) { return(percentages[1]) }; return(mean(percentages) - qnorm(0.975) * sd(percentages) / sqrt(n)) }))
					# This is for median:
					new_errorbar_lower = newplotdata - unlist(sapply(levels(thispagedata[[splitby[2]]]), function(x) { thisseg = segments_stalled[x == policylevels]; allsegs = total_segments[x == policylevels]; percentages = (thisseg / allsegs) * 100; n = length(percentages); if (n < 2) { return(percentages[1]) }; n = length(percentages); index= max(1, round((n/2) - (1.96*sqrt(n)/2))); if(debug_errorbars) { cat("length:", n, "lower_index:", index, "-->", sort(percentages)[index], "\n"); }; return(sort(percentages)[index]) }))
					new_errorbar_upper = unlist(sapply(levels(thispagedata[[splitby[2]]]), function(x) { thisseg = segments_stalled[x == policylevels]; allsegs = total_segments[x == policylevels]; percentages = (thisseg / allsegs) * 100; n = length(percentages); if (n < 2) { return(percentages[1]) }; n = length(percentages); index= max(1, round((n/2) + (1.96*sqrt(n)/2))); if(debug_errorbars) { cat("length:", n, "upper_index:", index, "-->", sort(percentages)[index], "\n"); }; return(sort(percentages)[index]) })) - newplotdata
					cat("New errorbar_lower", new_errorbar_lower, "\n")
					cat("New errorbar_upper", new_errorbar_upper, "\n")
				}
			} else if (aggregate != "hist") {
				if (print) {
					cat("Applying function", aggregate, "\n")
				}
				newplotdata = tapply(X=thispagedata[[metrics]], IND=thispagedata[[splitby[2]]], FUN=aggregate)
			}
			if (print) {
				cat("New plotdata", newplotdata, "\n")
			}
			if (plot_errorbars && !aggregate == "percentage") {
				if (aggregate == "median") {
					# Error bar: confidence interval for median
					# (some quantile depending on how many values we have in total)
					new_errorbar_lower = newplotdata - tapply(X=thispagedata[[metrics]], IND=thispagedata[[splitby[2]]], FUN=function(x) { if (length(x) < 2) { return(x[1]) }; n = length(x); index= max(1, round((n/2) - (1.96*sqrt(n)/2))); return(sort(x)[index]) })
					new_errorbar_upper = tapply(X=thispagedata[[metrics]], IND=thispagedata[[splitby[2]]], FUN=function(x) { if (length(x) < 2) { return(x[1]) }; n = length(x); index=min(length(x), round((n/2) + (1.96*sqrt(n)/2))); return(sort(x)[index]) }) - newplotdata
					# alternatively, binomial
					#new_errorbar_lower = newplotdata - tapply(X=thispagedata[[metrics]], IND=thispagedata[[splitby[2]]], FUN=function(x) { sort(x)[qbinom(c(.025,.975), length(x), 0.5)][1] })
					#new_errorbar_upper = tapply(X=thispagedata[[metrics]], IND=thispagedata[[splitby[2]]], FUN=function(x) { sort(x)[qbinom(c(.025,.975), length(x), 0.5)][2] }) - newplotdata
				} else {
					# Error bar: mean +- tinv(1-alpha,n-1)*std/sqrt( n )
					# Assuming normal distribution
					new_errorbar_lower = newplotdata - tapply(X=thispagedata[[metrics]], IND=thispagedata[[splitby[2]]], FUN=function(x) { n = length(x); return(mean(x) + qnorm(0.975) * sd(x) / sqrt(n)) })
					new_errorbar_upper = newplotdata - tapply(X=thispagedata[[metrics]], IND=thispagedata[[splitby[2]]], FUN=function(x) { n = length(x); return(mean(x) - qnorm(0.975) * sd(x) / sqrt(n)) })
				}
				if (debug_errorbars) {
					cat("Error bars for", aggregate, ":\n")
					for (lev in levels(thispagedata[[splitby[2]]])) {
						cat("\t", as.character(lev), ":\n")
						dataforerrorbar = subset(thispagedata, thispagedata[[splitby[2]]] == lev)[[metrics]]
						cat("\t", dataforerrorbar, "\n")
						n = length(dataforerrorbar)
						cat("\tlength:", n, "\n")
						if (aggregate == "median") {
							sorted_data = sort(dataforerrorbar)
							lower_index = max(1, round((n/2) - (1.96*sqrt(n)/2)))
							upper_index = min(length(dataforerrorbar), round((n/2) + (1.96*sqrt(n)/2)))
							cat("\t\tlower index:", lower_index, "-->", sorted_data[lower_index], "\n")
							cat("\t\tupper index:", upper_index, "-->", sorted_data[upper_index], "\n")
						} else {
							cat("\t\tmean:", mean(dataforerrorbar), "sd:", sd(dataforerrorbar), "\n")
						}
					}
				}
				new_errorbar_lower[is.na(new_errorbar_lower)] = 0
				new_errorbar_upper[is.na(new_errorbar_upper)] = 0
			}
            #cat("Length newplotdata:", length(newplotdata), "levels:", length(splitlevels), "\n")

            if (plot_errorbars && print) {
                cat("New Errorbar lower:", new_errorbar_lower, "\n")
                cat("New Errorbar upper:", new_errorbar_upper, "\n")
            }
            if (length(newplotdata) < length(splitlevels)) {
                #str(newplotdata)
                newnewplotdata = rep(0, length(splitlevels))
                names(newnewplotdata) = splitlevels
                newnewplotdata[names(newplotdata)] = newplotdata
                if (print) {
                    cat("Too short because", length(newplotdata), "for", length(splitlevels), "levels - have to extend\n")
                    cat(newnewplotdata, "\n")
                    cat(names(newnewplotdata), "\n")
                }
                if (plot_errorbars) {
                    #str(names(newplotdata))
                    newerrorbarupper = rep(0, length(splitlevels))
                    newerrorbarlower = rep(0, length(splitlevels))
                    names(newerrorbarupper) = splitlevels
                    names(newerrorbarlower) = splitlevels
                    newerrorbarupper[names(newplotdata)] = new_errorbar_upper
                    newerrorbarlower[names(newplotdata)] = new_errorbar_lower
                    new_errorbar_upper = newerrorbarupper
                    new_errorbar_lower = newerrorbarlower
                    #str(newerrorbarupper)
                    #str(newerrorbarlower)
                }
                newplotdata = newnewplotdata
                #missinglevels = splitlevels[! splitlevels %in% names(newplotdata)]
                #missingindex = which(splitlevels == missinglevels)
                #str(missinglevels)
                #str(missingindex)
                #newplotdata[missingindex] = 0
                #str(newplotdata)
                #errorbar_lower[missingindex] = 0
                #errorbar_upper[missingindex] = 0
            }
			plotdata = c(plotdata, newplotdata)
			#cat("Plotdata now", plotdata, "\n")
            if (plot_errorbars) {
                errorbar_lower = c(errorbar_lower, new_errorbar_lower)
                errorbar_upper = c(errorbar_upper, new_errorbar_upper)
                if (print) {
                    cat("Errorbar_lower", errorbar_lower, "\n")
                    cat("Errorbar_upper", errorbar_upper, "\n")
                }
            }
		}
		#plotdata = tapply(X=timingdata[[metrics]], IND=timingdata[[splitby[1]]]:timingdata[[splitby[2]]], FUN=aggregate)
		if (length(names) == 0) {
			names = unique(factor(timingdata[[splitby[1]]]:timingdata[[splitby[2]]]))
		}
	} else {
		if (aggregate == "hist") {
			breaks = seq(min(timingdata[[metrics]]) - 1, max(timingdata[[metrics]]))
			if (length(names) == 0) {
				names = seq(min(timingdata[[metrics]]), max(timingdata[[metrics]]))
			}
			if (print) {
				cat("Histogram of", metrics, "with min =", min(timingdata[[metrics]]), "and max =", max(timingdata[[metrics]]), "\n")
				cat("Breaks:", breaks, "\n")
				cat("Names:", names, "\n")
			}
			histogram_of_data = tapply(X=timingdata[[metrics]], IND=timingdata[[splitby[1]]]:timingdata[[splitby[2]]], FUN=hist, breaks=breaks, plot=F)
			plotdata = c()
			for (n in seq(1, length(names))) {
				if (print) {
					cat("Adding data for", n, ": ")
				}
				for (h in histogram_of_data) {
					if (print) {
						cat(h$counts[n], " ")
					}
					plotdata = c(plotdata, h$counts[n])
				}
				if (print) {
					cat("\n")
				}
			}

			if (print) {
				cat("Histogram plotdata: ", plotdata, ", names: ", names, "\n")
			}
		} else {
			plotdata = timingdata[[metrics]]
			if (length(names) == 0) {
				names = factor(timingdata[[splitby[1]]]:timingdata[[splitby[2]]])
			}
		}
		errorbar_upper = c()
		errorbar_lower = c()
	}
	if (print) {
		cat(length(plotdata), "items to plot:", plotdata, "\n")
		cat(levels(names), "\n")
	}

	if (all(grepl(":", as.character(names)))) {
		# Make labels for scenario axis
		names = strsplit(levels(names), split=":")
		names = sapply(names, function(x) {ifelse((length(x)>2), paste(toupper(x[3]), " (", x[2], " * ", toupper(x[1]), ")", sep=""), paste(toupper(x[2]), " (", x[1], ")", sep=""))})
	}

	if (length(metricslimit) == 0) {
		if (length(errorbar_upper) > 0) {
			metricslimit = c(0, max(plotdata + max(errorbar_upper, 0),na.rm=T))
		} else {
			metricslimit=c(0, max(plotdata))
		}
	}

	x_las = 2
	xaxt = "s"
	yaxt = "s"
	log=""
	numberofpolicies = length(levels(timingdata[[splitby[2]]]))

	# If we plot the simplified scenario axis (one label for three bars, indicating their shared scenario),
	# do not plot the usual scenario axis with one label for every bar
	if (plotsimplifiedscenarioaxis == TRUE) {
		plotscenarioaxis = FALSE
	}

	# For horizontal barplot (bars go from left to right)
	if (horiz) {

		# Set x axis for metrics
		xlabel = metricslabel
		if (plotmetricslog) {
			log="x"
			if (metricslimit[1] == 0) {
				metricslimit[1] = min(plotdata)
			}
		} else {
			metricslimit[1]=0
		}
		xlimit = metricslimit

		# Set y axis for scenario
		if (!plotscenarioaxis) {
			yaxt = "n"
		}
		if (plotscenarioaxis || plotsimplifiedscenarioaxis) {
			margins=par()$mar
			par(mar=c(margins[1], 5+cex*2, margins[3], margins[4]))
		}
		ylabel = scenariolabel
		ylimit = c(0, length(names) + 0.5)

	} else {
	# Vertical barplot (default), bars go from bottom to top

		# Set x axis for scenario
		xlabel = scenariolabel
		xlimit = c(0, length(names) + 0.5)
		if (!plotscenarioaxis) {
			xaxt = "n"
		} else {
			margins=par()$mar
			par(mar=c(5+cex*2, margins[2], margins[3], margins[4]))
		}

		# Set y axis for metrics
		ylabel = metricslabel
		if (plotmetricslog) {
			log="y"
			if (metricslimit[1] == 0) {
				metricslimit[1] = min(plotdata)
			}
		#} else {
		#	metricslimit[1] = 0
		}
		ylimit = metricslimit
	}

	#policycolors=c("azure4","azure3","white")
	#policycolors=gray(1:7 / 8)[seq(1,7, length.out=numberofpolicies)]
	policycolors=c("#202020", "#606060", "#A0A0A0", "#C0C0C0", "#DFDFDF")[1:length(levels(timingdata[[splitby[2]]]))]
	#cat("Policy colors:", policycolors, "\n")
	if (length(policynames) == 0) {
		policynames = as.character(levels(timingdata[[splitby[2]]]))
	}
	#policynames=c("Use Only Interface 1", "Use Only Interface 2", "Earliest Arrival First")
    #otherlegendnames=c("Simulated", "Testbed (with proxy)", "Testbed (without proxy)")
	policynames=sapply(policynames, plotlabel.human.readable)
	spaceinbarplot=c(0.2,rep(0,numberofpolicies-1))
	#spaceinbarplot=c(0.5, 0, 0.5, 0, 0.5, 0)
	if (print) {
		cat("space in barplot:", spaceinbarplot, "\n")
		cat("metricslim:",metricslimit,"\n")
		cat("ylim:",ylimit,"\n")
		cat("error bar upper:", errorbar_upper, "\n")
		cat("error bar lower:", errorbar_lower, "\n")
        cat(names, "\n")
	}
	if (all(nchar(names)) < 4) {
		x_las = 1
	}

    plot_legend_outside = 0
    if (plotlegend) {
        # Look if/where we can plot legend
		if (length(errorbar_upper) > 0) {
			wheretoplotlegend = find_empty_plotarea(rbind(plotdata+errorbar_upper, plotdata-errorbar_lower), metricslimit, plotmetricslog, fallback=F, dmins=c(0,0,0), verbose=print)
		} else {
			wheretoplotlegend = find_empty_plotarea(plotdata, metricslimit, plotmetricslog, dmins=c(0,0,0), fallback=F, verbose=print)
		}
		if (print) {
			cat("Empty plotarea?", wheretoplotlegend, "\n")
        }

		if (wheretoplotlegend == "") {
			#wheretoplotlegend = "topright"
			wheretoplotlegend = "topright"
            plot_legend_outside = ylimit[2] + (ylimit[2] - ylimit[1]) * 4/9
            #plot_legend_outside = ylimit[2] + max((ylimit[2] - ylimit[1]) / 3, 2)
            spaceabove = spaceabove + 5
			if (print) {
				cat("Cannot find an empty area to plot legend - using", wheretoplotlegend, "but", plot_legend_outside - ylimit[2], "outside, with ypos =", plot_legend_outside, ", increasing spaceabove\n")
			}
		} else {
            spaceabove = 1
        }
    }

    if (print) {
        cat("Preparing", plot, "with space below", spacebelow, "left", spaceleft, "above", spaceabove, "\n")
        str(plotdata)
    }
	prepare_file(plot=plot, filename=filename, spacebelow=spacebelow, spaceleft=spaceleft, spaceabove=spaceabove, width=14, height=10)

	# Draw the barplot
	bp = barplot(plotdata, log=log, cex.axis=cex, cex.names=cex, cex.lab=cex, horiz=horiz, space=spaceinbarplot, names.arg=rep(names, length.out=length(plotdata)), las=x_las, xlab=xlabel, ylab=plotlabel.human.readable(ylabel), main=mainlabel, col=policycolors, xaxt=xaxt, yaxt=yaxt, ylim=ylimit)

	# Draw error bars
	if (length(errorbar_upper) > 0 && any(errorbar_upper > 0)) {
        if(print) {
            cat("Drawing errorbars:\n")
            cat("Plotdata:\t", plotdata, "\n")
            cat("Errorbar upper:\t",plotdata+errorbar_upper,"\n")
            cat("Errorbar lower:\t",plotdata-abs(errorbar_lower), "\n")
        }
		options(warn=-1)
		if (horiz) {
            cat("=== Drawing errorbar upper:\t",plotdata+errorbar_upper,"\n")
			arrows(plotdata+errorbar_upper, bp, plotdata-abs(errorbar_lower), bp, angle=90, code=3, length=0.05)
		} else {
            cat("=== Drawing errorbar upper:\t",plotdata+errorbar_upper,"\n")
			arrows(bp, plotdata+errorbar_upper, bp, plotdata-abs(errorbar_lower), angle=90, code=3, length=0.1)
		}
		options(warn=0)
	} else {
		cat("No errorbars\n")
	}

	# Make the simplified scenario axis: One label for every 3 bars, indicating the scenario
	if (plotsimplifiedscenarioaxis ) {
		if ("page" %in% splitby | "workload" %in% splitby | "url" %in% splitby) {
			workloadnames = levels(unique(timingdata$page))

			workloadnames = gsub("http://researchvm.inet.tu-berlin.de/video/BigBuckBunny/4sec/", "", workloadnames)
			workloadnames = gsub("BigBuckBunny", "BBB", workloadnames)
			workloadnames = gsub("_simple_2014_05_09.mpd", "", workloadnames)

			workloadnames = gsub("http://130.149.221.201/pics-", "", workloadnames)
			workloadnames = gsub(".html", "", workloadnames)
			#cat(length(workloadnames),"workloadnames:",workloadnames,"\n")
			workloadnames = gsub("1kb:16_10kb:8_100kb:4", "mixed", workloadnames)
			workloadnames = gsub("1kb:32_10kb:16_100kb:2_200kb:2", "mixed", workloadnames)
			workloadnames = gsub("1kb-32_10kb-16_100kb-2_200kb-2", "mixed", workloadnames)
			#workloadnames = gsub("1kb:32_10kb:16_100kb:2_200kb:2", "mixed", workloadnames)
			workloadnames = strsplit(workloadnames, split="-")
			workloadnames = sapply(workloadnames, function(x) {ifelse((length(x)>1), paste(x[2], "*", toupper(gsub("k","K",x[1])), sep=""), paste(x))})
			workloadnames = gsub("/", "", workloadnames)

		} else {
			workloadnames = levels(timingdata[[splitby[1]]])
		}
		names = rep(names, length.out=length(plotdata))
		if (length(workloadnames) == 0) {
			if (aggregate == "hist") {
				workloadnames = seq(min(names), max(names))
			} else {
				workloadnames = names
			}
		}
		if (print) {
			cat(length(names),"names:",names,"\n")
			cat(length(policynames),"policynames:",policynames,"\n")
			cat(length(workloadnames),"workloadnames:",workloadnames,"\n")
		}
		if (horiz) {
			axis(side=2, bp[seq(2,length(names),length.out=length(workloadnames))], labels=workloadnames, las=1, cex.axis=cex)
		} else {
			if (all(nchar(workloadnames) < 11)) {
				wlas = 1
			} else {
				wlas = 2
			}

			if (length(workloadnames) > 8 & any(nchar(workloadnames) > 11)) {
				wlas = 2
			}

			if (length(policynames) == 1) {
				axis_at = bp[seq(1,length(names),length.out=length(workloadnames))]
			}
			if (length(policynames) > 1) {
				axis_at = bp[seq(1,length(names),length.out=length(workloadnames))]+0.5
				axis_at[1] = axis_at[1] + 2
				axis_at[length(axis_at)] = axis_at[length(axis_at)] - 3
			}
			#axis_at[1] = axis_at[1] - 1
			if (print) {
				cat("Plotting axis labels", workloadnames, "at", axis_at, "with las", wlas, "\n")
			}
			axis(side=1, axis_at, tick=F, labels=workloadnames, las=wlas, cex.axis=cex)
		}
	}

	# Plot a legend of policies
	if (plotlegend) {
		bty='o'
		inset=0.01
		if (plotmetricslog) {
			bty='n'
		} else {
			inset = 0.05
		}
		legendnames = policynames[1:numberofpolicies]
		if (plot_legend_outside > 0) {
			par(xpd=T)
			x_legend = 0
			if (wheretoplotlegend == "topright") {
				x_legend = 12
			}
			l = legend(x=x_legend, y=plot_legend_outside, inset=inset, legend=legendnames, fill=policycolors, cex=cex, bty=bty)
		} else {
			l = legend(wheretoplotlegend, inset=inset, legend=legendnames, fill=policycolors, cex=cex, bty=bty)
        }
		if (print) {
			cat("Plotted legend with names", legendnames, ifelse(plot_legend_outside > 0, paste("at", plot_legend_outside), ""), "to", wheretoplotlegend, "\n")
		}

		if (plotmetricslog) {
			l = l$rect
			xmid = l$left + 0.5* l$w
			#str(l)
			#rect(xleft=xmid-0.47*l$w, xright=xmid+0.5*l$w, ytop=16, ybottom=2.4)
		}
	}
	if (plot == "pdf" | plot == "eps" | plot == "png") {
		invisible(dev.off())
	}
}

# Read timings, subset them and produce barplot
subsetbarplot <- function(data=NULL, logfile="runs-sym.log", metric="actual_time", mainlabel="", cex=1, aggregate="median", subset=c())
{
	if (is.null(data)) {
		timings = readtimings(paste(logprefix, logfile, sep=""))
	} else {
		timings=data
	}

	if (length(subset) > 0) {
		timings = subset(timings, timings$workload %in% subset)
		timings$workload = factor(timings$workload, unique(timings$workload))
	}

	#barplottimings(timings, metric=metric, mainlabel=mainlabel, cex=cex, aggregate=aggregate, horiz=F, plotscenarioaxis=T, metricslabel="Median page load time [s]", metricslimit=c(min(timings[[metric]], max(timings[[metric]]))))
	barplottimings(timings, metrics=metric, mainlabel=mainlabel, cex=cex, aggregate=aggregate)
}

# Make three barplots stacked over one another, for symmetric, asymmetric and highly asymmetric
#	Options:
#		data			Data frame to produce plots from
#		logprefix		Path to file to read data from ("sym.log", "asym.log" and "high-asym.log")
#		metric			Timing metrics to use
#		aggregate		How to aggregate timings for same workload and policy
#		subset			Only plot for these workloads
#		plotmetricslog	Plot logarithmic metrics axis
#		plotmainlabels	Plot labels for the three scenarios
#		plot			Where to plot: pdf, eps, terminal
threebarplots <- function(data=NULL, logprefix="", metric="actual_time", mainlabel="", cex=1, aggregate="median", subset=c(), plotmetricslog=T, plotmainlabels=T, mainlabels=c("Symmetric scenario", "Asymmetric scenario", "Highly asymmetric scenario"), plot="terminal", filename="threebarplots")
{
	if (plot == "pdf") {
		cat("Plotting pdf\n")
		prepare_pdf(paste(filename, ".pdf", sep=""), width=20, height=40)
		if (cex == 1) {
			cex=1.5
		}
		par(mfrow=c(3,1), oma=c(0,1,1,0), mar=c(4,4,2,0.5))
	} else if (plot == "eps") {
		cat("Plotting eps\n")
		setEPS()
		postscript(paste(filename, ".eps", sep=""), height=13, width=14)
		par(mfrow=c(3,1), oma=c(0,0,0.2,0), mar=c(2,5.5,1,0))
		cex=1.5
	} else {
		cat("Plotting to terminal\n")
		par(mfrow=c(3,1), oma=c(0,1,1,0), mar=c(4,4,2,0.5))
	}
	par(cex=cex, cex.axis=cex, cex.lab=cex, cex.main=cex, cex.sub=cex)

	if (logprefix == "" & !is.null(data)) {
		symtimings=data$sym
		asymtimings=data$asym
		highasymtimings=data$hasym
	} else {
		symtimings = readtimings(paste(logprefix, "sym.log", sep=""))
		asymtimings = readtimings(paste(logprefix, "asym.log", sep=""))
		highasymtimings = readtimings(paste(logprefix, "high-asym.log", sep=""))
	}

	# Subset: Filter data set for specific workloads
	if (length(subset) > 0) {
        cat("Filtering for", subset, "\n")
		symtimings = fix.factors(subset(symtimings, symtimings$page %in% subset))
		asymtimings = fix.factors(subset(asymtimings, asymtimings$page %in% subset))
		highasymtimings = fix.factors(subset(highasymtimings, highasymtimings$page %in% subset))
		#symtimings$workload = factor(symtimings$page, unique(symtimings$page))
		#asymtimings$workload = factor(asymtimings$page, unique(asymtimings$page))
		#highasymtimings$workload = factor(highasymtimings$page, unique(highasymtimings$page))
	}
	ymin = min(symtimings[[metric]], asymtimings[[metric]], highasymtimings[[metric]])
	ymax = max(symtimings[[metric]], asymtimings[[metric]], highasymtimings[[metric]])

	if (plotmainlabels) {
		symlabel = mainlabels[1]
		asymlabel = mainlabels[2]
		highasymlabel = mainlabels[3]
	} else {
		symlabel=""
		asymlabel=""
		highasymlabel = ""
	}
	metricslabel = paste(toupper(substring(aggregate, 1,1)), substring(aggregate, 2), " page load time [s]", sep="")

	# Make three bar plots
	par(mar=c(0.5,5.5,1,0))
	barplottimings(symtimings, metrics=metric, mainlabel=symlabel, cex=cex, aggregate=aggregate, horiz=F, plotscenarioaxis=F, metricslabel="", metricslimit=c(ymin, ymax), plotmetricslog=plotmetricslog, plotsimplifiedscenarioaxis=F, plotlegend=T)
	par(mar=c(0.5,5.5,1,0))
	barplottimings(asymtimings, metrics=metric, mainlabel=asymlabel, cex=cex, aggregate=aggregate, horiz=F, plotscenarioaxis=F, plotlegend=F, metricslabel="", metricslimit=c(ymin, ymax), plotmetricslog=plotmetricslog, plotsimplifiedscenarioaxis=F)
	mtext(side=2, text=metricslabel, line=4, cex=2.5)
	par(mar=c(2,5.5,1.5,0))
	barplottimings(highasymtimings, metrics=metric, mainlabel=highasymlabel, cex=cex, aggregate=aggregate, horiz=F, plotlegend=F, plotscenarioaxis=T, metricslabel="", metricslimit=c(ymin, ymax), plotmetricslog=plotmetricslog)
	
	# Close file
	if (plot == "pdf" | plot == "eps") {
		dev.off()
	}
}

threebarplots_workloads <- function(data, plot="terminal", w1=NULL, w2=NULL, w3=NULL, cex=1)
#threebarplots <- function(data=NULL, logprefix="", metric="actual_time", mainlabel="", cex=1, aggregate="median", subset=c(), plotmetricslog=T, plotmainlabels=T, plot="terminal")
{
	if (is.null(w1) || is.null(w2) || is.null(w3)) {
		w1 = c("1kb:32", "10kb:4", "10kb:8", "10kb:16", "10kb:32", "100kb:4")
		w2 = c("100kb:8", "1mb:2", "100kb:32", "1mb:4", "200kb:32")
		w3 = c("100kb:128", "425kb:32", "1mb:32", "100kb:64", "1mb:8")
	}

	if (plot=="pdf")
	{
		prepare_pdf("threebarplots.pdf", width=10*cex, height=16*cex)
	}
	par(mfrow=c(3,1), oma=c(0,1,1,0), mar=c(4,4,2,0.5))

	par(cex=cex, cex.axis=cex, cex.lab=cex, cex.main=cex, cex.sub=cex)

	subsetbarplot(data=data, subset=w1)
	subsetbarplot(data=data, subset=w2)
	subsetbarplot(data=data, subset=w3)

	if (plot=="pdf") {
		dev.off()
	}
}

# Read all data into a list
listalldata <- function(logprefixes=c("kittens/", "websites/"))
{
	symlist = list()
	asymlist = list()
	hasymlist = list()
	for (logprefix in logprefixes) {
		symtimings = readtimings(paste(logprefix, "sym.log", sep=""))
		asymtimings = readtimings(paste(logprefix, "asym.log", sep=""))
		hasymtimings = readtimings(paste(logprefix, "high-asym.log", sep=""))
		symlist[[logprefix]] = symtimings
		asymlist[[logprefix]] = asymtimings
		hasymlist[[logprefix]] = hasymtimings
	}
	str(symlist)
	str(asymlist)
	str(hasymlist)
	listoffalldata = list()
	listoffalldata$sym = do.call(rbind, symlist)
	listoffalldata$asym = do.call(rbind,asymlist)
	listoffalldata$hasym = do.call(rbind,hasymlist)

	return(listoffalldata)
}

# Compute absolute and relative difference of page load time on the actual testbed (as computed from HAR file) and simulated time
compute_diffs <- function(data)
{
	data$abs_diff = data$computed_time - data$simulated_time
	data$rel_diff = with(data, 100*(computed_time - simulated_time)/ifelse((computed_time > simulated_time), computed_time, simulated_time))
	return(data)
}

# Plot density of the difference of actual and simulated page load times
plot.validation <- function(data, cex=1, plotfile="no", mainlabel="")
{
	par(mar=c(4.5,3.5,0.1,0.1))
	data = compute_diffs(data)
	with(data, plot(density(abs_diff), ann=FALSE, col="darkgoldenrod", lwd=2, xlim=c(-20,20)))
	with(ksym, lines(density(rel_diff), lwd=1))
    mtext(side=2, text="ECDF", line=2.5, cex=cex)
    # Add label for relative difference
    mtext(side=1, text="Actual - simulated page load time [s]", line=2.5, cex=cex, col="darkgoldenrod")
    mtext(side=1, text="Relative difference of page load times [%]", line=3.5, cex=cex)
}

plot.timings.cdfs <- function(data, metrics="wallclock_time", splitby = "policy", print=F, plot="terminal", filename = "cdfs", cex=1, mainlabel="", xlabel="", log="", xlim=NULL, subsetdata=F, legendposition="topleft", draw_vertical_line=F, abline=1, order=c(), colors=c("red", "darkorchid3", "cadetblue", "blue", "darkolivegreen4", "gray45", "black", "darkgoldenrod", "brown", "blueviolet", "darkorange2", "aquamarine4", "forestgreen", "deeppink"), pointtypes=c(4, 8, 18, 17, 16, 15, 14, 9, 12, 0, 1, 2, 3, 19)) {
    prepare_file(plot=plot, filename=filename, width=10, height=7, cex=cex, spacebelow=3)
    if (length(splitby) == 1) {
        splitdata = split(data, data[[splitby]])
    } else if (length(splitby) > 1) {
        splitresult = split_dataframe_by_factors(data, splitby, print=print)
        splitdata = split(data, splitresult$splitfactor)
    }
	if (print) {
		cat("Split data by", splitby, "- got list of length", length(splitdata), "\n")
	}

    if (xlabel == "") {
        xlabel = plotlabel.human.readable(metrics)
    }

    firstdata = splitdata[[1]][[metrics]]
    firstdata = firstdata[!is.na(firstdata)]
    if (length(firstdata) == 0) {
        cat("First data of length 0 -- aborting\n")
        return()
    }

    plot.cdf.list(splitdata, metrics=metrics, mainlabel=mainlabel, xlabel=xlabel, log=log, xlim=xlim, subsetdata=subsetdata, legendposition=legendposition, order=order, colors=colors, pointtypes=pointtypes)
    if (draw_vertical_line) {
        abline(v=abline, col="grey")
    }

	if (plot == "pdf" | plot == "eps" | plot == "png") {
		invisible(dev.off())
	}
}

# Compute a matrix of speedups of all policies against a baselinepolicy
compute.speedup.matrix <- function(data, metrics="wallclock_time", quantile=0, baselinepolicy="wlan0", sortpolicy="eaf_threshold", absolute=F, slowdown=F)
{
	data = data[with(data, order(starttimestamp)),]
	if (quantile > 0) {
		websitedata = as.list(split(data, data$page))
		# Split data by policy, creating a list of data frames, each for one policy
		policydata = lapply(websitedata, function(x) split(x[[metrics]], x$policy))
		q = lapply(policydata, function(x) compute.quantiles(x, quantiles=c(quantile)))
		if (absolute) {
			s1 = lapply(q, function(x) compute.abs.speedups(x, baselinepolicy))
		} else {
			s1 = lapply(q, function(x) compute.rel.speedups(x, baselinepolicy))
		}
		speedupmatrix = matrix(unlist(s1), ncol=length(colnames(s1[[1]])), byrow=T, dimnames = list(names(s1),colnames(s1[[1]])))
	} else {
		if (absolute) {
			speedupmatrix = compute.abs.speedups(datamatrix(data, metrics=metrics), baselinepolicy)
		} else {
			if (slowdown) {
				speedupmatrix = compute.rel.slowdowns(datamatrix(data, metrics=metrics), baselinepolicy)
			} else {
				speedupmatrix = compute.rel.speedups(datamatrix(data, metrics=metrics), baselinepolicy)
			}
		}
	}
	return(speedupmatrix[order(speedupmatrix[,sortpolicy], decreasing=T),])
}

compute.abs.speedups <- function(quantilematrix, baselinepolicy="wlan0")
{
    if (! baselinepolicy %in% colnames(quantilematrix)) {
        cat("Warning: Baseline policy", baselinepolicy, "not in data -- using", colnames(quantilematrix)[1], "\n")
        baselinepolicy = colnames(quantilematrix)[1]
    }
	baselinetimings = quantilematrix[,baselinepolicy]
	diffs=matrix(NA, nrow=nrow(quantilematrix), ncol=ncol(quantilematrix))
	for (i in 1:ncol(quantilematrix)) {
		diffs[,i] = baselinetimings - quantilematrix[,i]
	}
	dimnames(diffs) = dimnames(quantilematrix)
	return(diffs)
}

compute.rel.speedups <- function(quantilematrix, baselinepolicy="wlan0")
{
    if (! baselinepolicy %in% colnames(quantilematrix)) {
        cat("Warning: Baseline policy", baselinepolicy, "not in data -- using", colnames(quantilematrix)[1], "\n")
        baselinepolicy = colnames(quantilematrix)[1]
    }

	baselinetimings = quantilematrix[,baselinepolicy]
	diffs=matrix(NA, nrow=nrow(quantilematrix), ncol=ncol(quantilematrix))
	for (i in 1:ncol(quantilematrix)) {
		diffs[,i] = baselinetimings / quantilematrix[,i]
	}
	dimnames(diffs) = dimnames(quantilematrix)
	return(diffs)
}

compute.rel.slowdowns <- function(quantilematrix, baselinepolicy="wlan0")
{
    if (! baselinepolicy %in% colnames(quantilematrix)) {
        cat("Warning: Baseline policy", baselinepolicy, "not in data -- using", colnames(quantilematrix)[1], "\n")
        baselinepolicy = colnames(quantilematrix)[1]
    }

	baselinetimings = quantilematrix[,baselinepolicy]
	cat("baseline:",head(baselinetimings),"\n")
	diffs=matrix(NA, nrow=nrow(quantilematrix), ncol=ncol(quantilematrix))
	for (i in 1:ncol(quantilematrix)) {
		cat("policy:",head(quantilematrix[,i]),"\n")
		diffs[,i] = quantilematrix[,i] / baselinetimings
		cat("diffs:",head(diffs[,i]),"\n")
	}
	dimnames(diffs) = dimnames(quantilematrix)
	return(diffs)
}

# Put simulator output data from a data frame into a matrix where rows = run:page:try and column=policy
datamatrix <- function(data, metrics="wallclock_time", aggregatefactors = c("run", "page", "try"))
{
    number_of_policies = length(levels(data$policy))
    repetitions = replications(data[,aggregatefactors])
    exclude_factor = list()
    exclude_level = list()
    for (af in aggregatefactors) {
        rep_af = repetitions[[af]]
        #str(rep_af)
        if (length(rep_af) == 1) {
            next
        }
        mismatched_levels = which((rep_af %% number_of_policies) > 0)
        if (length(mismatched_levels) > 0) {
            exclude_factor = c(exclude_factor, list(af))
            exclude_level = c(exclude_level, list(names(rep_af)[mismatched_levels]))
        }
    }

    data_to_exclude = data

    if (length(exclude_factor) > 0) {
        for (i in seq(1, length(exclude_factor))) {
            cat("Excluding", exclude_factor[[i]], "==", exclude_level[[i]], "\n")
            data_to_exclude = fix.factors(subset(data_to_exclude, data_to_exclude[[exclude_factor[[i]]]] == exclude_level[[i]]))
        }
        #str(data_to_exclude)
        data = fix.factors(subset(data, !(data$starttime  %in% data_to_exclude$starttime)))
    }
	timingmatrix = matrix(data[[metrics]], ncol=length(levels(data$policy)), byrow=T, dimnames=list(unique(data$run:data$page:data$try), as.character(unique((data$policy)))))
	return(timingmatrix)
}

find.comparison.policy <- function(datamatrix) {
	for (i in seq(1, dim(datamatrix)[2])) {
		if (length(unique(datamatrix[,i])) == 1) {
			return(colnames(datamatrix)[i])
		}
	}
	return("")
}

plot.speedup.cdfs <- function(speedupmatrix, quantile="", comparisonpolicy="", plot="terminal", filename="speedup_cdfs", mainlabel="", cex=1, line1=F, log="", xlim=c(0, max(speedupmatrix)), ylim=c(0,1), absolute=F, slowdown=F, ccdf=F)
{
    prepare_file(plot=plot, filename=filename, width=10, height=7, spacebelow=5)

	colors=c("red", "darkorchid3", "cadetblue", "blue", "darkolivegreen4", "gray45", "black")
	pointtypes=c(4, 8, 18, 17, 16, 15, 14)
	legendlabels = colnames(speedupmatrix)

	if (log == "x" && xlim[[1]] == 0) {
		xlim[[1]] = min(speedupmatrix)
	}
	if (comparisonpolicy == "") {
		comparisonpolicy = find.comparison.policy(head(speedupmatrix, n=100))
	}
	if (min(speedupmatrix) < 0) {
		absolute = T
	}
	if (comparisonpolicy == colnames(speedupmatrix)[1]) {
        if (ncol(speedupmatrix) > 2) {
            speedupmatrix = speedupmatrix[,c(2,1,3:ncol(speedupmatrix))]
            colors=colors[c(2,1,3:length(colors))]
            pointtypes=pointtypes[c(2,1,3:length(pointtypes))]
            legendlabels=legendlabels[c(2,1,3:length(legendlabels))]
        } else {
            speedupmatrix = speedupmatrix[,c(2,1)]
            colors=colors[c(2,1)]
            pointtypes=pointtypes[c(2,1)]
            legendlabels=legendlabels[c(2,1)]
        }
	}
	colors_legend = colors
	pointtypes_legend = pointtypes

	plot.cdf(speedupmatrix[,1], xlab=paste(quantile,ifelse(slowdown==T,"Page load time",ifelse(quantile=="","Speedup","speedup")),ifelse(absolute,"absolute compared","relative"),"to",comparisonpolicy, ifelse(absolute,"[s]","[factor]")), log=log, xlim=xlim, ylim=ylim, color=colors[1], pch=pointtypes[1], cex=cex, ccdf=ccdf, mainlabel=mainlabel)

	for (i in seq(2, dim(speedupmatrix)[2])) {
		if (length(unique(speedupmatrix[,i])) > 1) {
			add.cdf(speedupmatrix[,i], color=colors[i], pch=pointtypes[i], ccdf=ccdf)
		}
		else {
			colors_legend=colors_legend[-i]
			pointtypes_legend=pointtypes_legend[-i]
			legendlabels=legendlabels[-i]
		}
	}

	if (line1) {
		abline(v=1)
	}
	wheretoplotlegend="bottomright"
	if(slowdown) {
		wheretoplotlegend="topleft"
	}
	if(ccdf) {
		wheretoplotlegend="bottomleft"
	}
	legend(wheretoplotlegend, legend=legendlabels, col=colors_legend, pch=pointtypes_legend, bty="n", cex=cex)

	if (plot == "pdf" | plot == "eps" | plot == "png") {
		invisible(dev.off())
	}
}

filter.speedup.matrix <- function(datamatrix, policy="eaf_threshold", min = -1, max = -1) {
	matrix.sample=datamatrix
	if (min != -1) {
		matrix.sample = subset(matrix.sample, subset=(matrix.sample[,policy] >= min))
	}
	if (max != -1) {
		matrix.sample = subset(matrix.sample, subset=(matrix.sample[,policy] <= max))
	}
	cat("Returning",length(rownames(matrix.sample)),"out of",length(rownames(datamatrix)),"rows (",length(rownames(matrix.sample))/length(rownames(datamatrix))*100,"% )\n")
	return(matrix.sample)
}

filter.speedup.dataframe <- function(datamatrix, policy="eaf_threshold", min = -1, max = -1) {
	matrixsample = filter.speedup.matrix(datamatrix, policy, min, max)
	dfsample = as.data.frame(matrixsample)
	dfsample$page = as.factor(sapply(rownames(matrixsample), function(x) return(strsplit(x, ":")[[1]][[1]])))
	dfsample$try = as.factor(sapply(rownames(matrixsample), function(x) return(strsplit(x, ":")[[1]][[2]])))
	#dfsample$if1_bw = as.factor(sapply(rownames(matrixsample), function(x) return(strsplit(x, ":")[[1]][[3]])))
	#dfsample$if1_rtt = as.factor(sapply(rownames(matrixsample), function(x) return(strsplit(x, ":")[[1]][[4]])))
	#dfsample$if2_bw = as.factor(sapply(rownames(matrixsample), function(x) return(strsplit(x, ":")[[1]][[5]])))
	#dfsample$if2_rtt = as.factor(sapply(rownames(matrixsample), function(x) return(strsplit(x, ":")[[1]][[6]])))
	#dfsample$scenario = dfsample$if1_bw:dfsample$if1_rtt:dfsample$if2_bw:dfsample$if2_rtt
	#fix.factors(dfsample, "scenario")
	return(dfsample)
}
speeduplist <- function(speedupmatrix, comparepolicy="eaf_threshold", levels=matrix(c(c(-1, 0.999), c(1,1), c(1.00001, 2), c(2.00001, 5), c(5.00001, 10), c(10.000001, -1)), ncol=2, byrow=T))
{
	if (dim(levels)[1] == 2 && dim(levels)[2] < 2) {
		levels = t(levels)
	}
	baselinepolicy = find.comparison.policy(head(speedupmatrix, n=100))
	cat(baselinepolicy,"vs",comparepolicy,"\n")
	datalist=list()
	for (i in 1:dim(levels)[1]) {
		datalist[[i]] = filter.speedup.dataframe(speedupmatrix, min=levels[i,1], max=levels[i,2])
		level.human.readable(levels[i,])
		names(datalist)[i] = paste(policyname.human.readable.short(comparepolicy), level.human.readable(levels[i,]), baselinepolicy)
		cat("Finished", names(datalist)[i], "\n")
	}
	return(datalist)
}

# Read data on whether page loads failed or succeeded
read_success_or_fail <- function(logprefix=list.files(path="data/", pattern="run-*")[1], filename="success_or_fail.log", print=TRUE)
{
	data <- read.csv(paste(logprefix, filename, sep=""), header=T)

	# Do not have to set column names -- they are in the file
	#	colnames(data) = c("page", "starttime", "does_navtiming_exist", "does_restiming_exist", "does_harfile_exist", "last_event_in_failed_navtiming", "num_dnsreplies", "num_ssl", "num_http", "num_https", "num_httpGET", "num_http301or302", "num_http200")

    if (print) {
        str(data)
    }
	return(data)
}

read_runs_success_or_fail <- function(logprefix="data/", runs=list.files(path=logprefix, pattern="^run*"), filename="success_or_fail.log", print=FALSE) {

	rundata = data.frame()
	rundatalist = list()

	for (i in 1:length(runs)) {
		run = runs[i]
		cat("  [", i, "/", length(runs), "] Reading in data from", run, "\n")
		data = read_success_or_fail(paste(logprefix, run, "/", sep=""), filename=filename, print=print)
		if (is.null(data)) {
			next
		}
		gsub("run-", "", run)
		datematch = regexpr("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}-",run)
		dateofrun = substring(run, datematch, datematch+attr(datematch, "match.length")-2)
		data$run = as.factor(run)
		data$workload = as.factor(get_workload(run))
		data$methodology = as.factor(get_methodology_label(run))
		data$browser = as.factor(get_browser_label(run))
		gsub(dateofrun, "", run)
		scenario = as.factor(substring(run, datematch+attr(datematch, "match.length")))
		if (print) {
			cat("  Dateofrun:", dateofrun, "scenario:", scenario, "\n")
		}
		data$scenario = scenario
		#if (!grepl("shaper", run)) {
		#	data$policy = as.factor(paste(data$iface,data$scenario,sep="_"))
		#} else {
		#	data$policy = data$iface:data$policy
		#}
		#data$policy = data$iface

		rundatalist[[i]] = data
	}
    rundatalistlength1 = length(rundatalist[[1]])
    if (all(lapply(rundatalist, function(x) length(x)) == rundatalistlength1)) {
        # All list items are similar - we can combine them to a single data frame
        rundata = do.call(rbind, rundatalist)
    } else {
        rundatalist = rundatalist[vapply(rundatalist, Negate(is.null), NA)]
        cat("Warning: Lengths of data items do not match\n")
        cat("Some columns were filled with NAs.\n")
        rundata = do.call(rbind, filldataframes(rundatalist))
    }

	if (print) {
		str(rundata)
	}
	return(rundata)
}

# Read data object size validation, i.e., "ground truth" from the trace
read_objectsize_validation_data <- function(logprefix=list.files(path="data/", pattern="run-*")[1], filename="object_sizes_trace.log", print=TRUE)
{
	data <- read.csv(paste(logprefix, filename, sep=""), header=F)

	# Set column names
	colnames(data) = c("page", "starttime", "resource_starttimestamp", "uri", "http_status", "trace_tcplen", "trace_headerlen", "trace_bodylen", "har_transfersize", "har_headerlen", "har_bodylen", "har_contentlengthheader", "res_bodylen")

    if (print) {
        str(data)
    }
	return(data)
}

read_runs_objectsize_validation <- function(logprefix="data/", runs=list.files(path=logprefix, pattern="^run*"), filename="object_sizes_trace.log", print=FALSE) {

	rundata = data.frame()
	rundatalist = list()

	for (i in 1:length(runs)) {
		run = runs[i]
		cat("  [", i, "/", length(runs), "] Reading in data from", run, "\n")
		data = read_objectsize_validation_data(paste(logprefix, run, "/", sep=""), filename=filename, print=print)
		if (is.null(data)) {
			next
		}
		gsub("run-", "", run)
		datematch = regexpr("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}-",run)
		dateofrun = substring(run, datematch, datematch+attr(datematch, "match.length")-2)
		data$run = as.factor(run)
		data$methodology = as.factor(get_methodology_label(run))
		data$browser = as.factor(get_browser_label(run))
		data$workload = as.factor(get_workload(run))
		gsub(dateofrun, "", run)
		scenario = as.factor(substring(run, datematch+attr(datematch, "match.length")))
		if (print) {
			cat("  Dateofrun:", dateofrun, "scenario:", scenario, "\n")
		}
		data$scenario = scenario
		#if (!grepl("shaper", run)) {
		#	data$policy = as.factor(paste(data$iface,data$scenario,sep="_"))
		#} else {
		#	data$policy = data$iface:data$policy
		#}
		#data$policy = data$iface

		rundatalist[[i]] = data
	}
    rundatalistlength1 = length(rundatalist[[1]])
    if (all(lapply(rundatalist, function(x) length(x)) == rundatalistlength1)) {
        # All list items are similar - we can combine them to a single data frame
        rundata = do.call(rbind, rundatalist)
    } else {
        rundatalist = rundatalist[vapply(rundatalist, Negate(is.null), NA)]
        cat("Warning: Lengths of data items do not match\n")
        cat("Some columns were filled with NAs.\n")
        rundata = do.call(rbind, filldataframes(rundatalist))
    }

	if (print) {
		str(rundata)
	}
	return(rundata)
}

# Read data comparing resource timing and HAR objects
read_compare_objects <- function(logprefix=list.files(path="data/", pattern="run-*")[1], filename="compare_har_res.log", print=TRUE)
{
	fullfilename = paste(logprefix, filename, sep="")
	data <- read.csv(fullfilename, header=F)

	colnames(data) = c("page", "starttime", "http_status", "http_version", "found_where", "har_transfersize", "har_bodylen", "har_headerlen", "har_contentlengthheader", "har_contentsize", "res_bodylen", "res_decoded", "url", "blocked_time", "dns_time", "connect_time", "ssl_time", "send_time", "wait_time", "receive_time")

    if (print) {
        str(data)
    }
	return(data)
}

read_runs_compare_objects <- function(logprefix="data/", runs=list.files(path=logprefix, pattern="^run*"), filename="compare_har_res.log", print=FALSE) {

	rundata = data.frame()
	rundatalist = list()

	for (i in 1:length(runs)) {
		run = runs[i]
		cat("  [", i, "/", length(runs), "] Reading in data from", run, "\n")
		data = read_compare_objects(paste(logprefix, run, "/", sep=""), filename=filename, print=print)
		if (is.null(data)) {
			next
		}
		gsub("run-", "", run)
		datematch = regexpr("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}-",run)
		dateofrun = substring(run, datematch, datematch+attr(datematch, "match.length")-2)
		data$run = as.factor(run)
		data$methodology = as.factor(get_methodology_label(run))
		data$browser = as.factor(get_browser_label(run))
		data$workload = as.factor(get_workload(run))
		gsub(dateofrun, "", run)
		scenario = as.factor(substring(run, datematch+attr(datematch, "match.length")))
		if (print) {
			cat("  Dateofrun:", dateofrun, "scenario:", scenario, "\n")
		}
		data$scenario = scenario
		#if (!grepl("shaper", run)) {
		#	data$policy = as.factor(paste(data$iface,data$scenario,sep="_"))
		#} else {
		#	data$policy = data$iface:data$policy
		#}
		#data$policy = data$iface

		rundatalist[[i]] = data
	}
    rundatalistlength1 = length(rundatalist[[1]])
    if (all(lapply(rundatalist, function(x) length(x)) == rundatalistlength1)) {
        # All list items are similar - we can combine them to a single data frame
        rundata = do.call(rbind, rundatalist)
    } else {
        rundatalist = rundatalist[vapply(rundatalist, Negate(is.null), NA)]
        cat("Warning: Lengths of data items do not match\n")
        cat("Some columns were filled with NAs.\n")
        rundata = do.call(rbind, filldataframes(rundatalist))
    }

	if (print) {
		str(rundata)
	}
	return(rundata)
}
