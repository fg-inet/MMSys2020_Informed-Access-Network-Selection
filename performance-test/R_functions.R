#!/usr/bin/Rscript 
# R functions needed by multiple scripts
# by Theresa Enghardt

# Disable scientific notation
options(scipen=999)

# maximum realistic value -- reset everything higher to this value
MAX_VALUE=10^32

sortdataframe <- function(data, sortby) {
    return(data[order(data[[sortby]]),])
}

capitalize <- function(string) {
    return(paste(toupper(substring(string, 1, 1)), substring(string, 2), sep=""))
}

capitalize_protocol_names <- function(string) {
    return(capitalize((gsub("[Tt]cp", "TCP", gsub("[Dd]ns", "DNS", gsub("[Hh]ttp", "HTTP", gsub("[Ss]sl", "SSL", string)))))))
}

split_dataframe_by_factors <- function(data, factors, print=F) {
	if (all(is.na(factors)) | length(factors) <= 0) {
		return(list(data = data, splitfactor = c()))
	}
	splitfactor = data[[factors[1]]]
	if (print) {
		cat("Splitting summary by", factors[1], "- levels:", as.character(unique(splitfactor)), "\n")
	}
	data$splitfactor = data[[factors[1]]]

	if (length(factors) > 1) {
		for (factor in factors[2:length(factors)]) {
			if (print) {
				cat("Also splitting by", as.character(factor), "- levels:", as.character(unique(data[[factor]])), "\n")
			}
			splitfactor = splitfactor:data[[factor]]
			data$splitfactor = data$splitfactor:data[[factor]]
		}
	}
	return(list(data = split(data, splitfactor), splitfactor = splitfactor))
}

check_all_equal <- function(data, metrics_to_check=c(), splitfactor = "page") {

	# Check if all runs for the same page have the same value for some metrics
	splitdata = fix.factors(split(data, data[[splitfactor]]))

	if (length(metrics_to_check) > 0) {
		for (sdata in splitdata) {
			invalid = check_metrics(sdata, metrics = metrics_to_check, allequal=TRUE)
			if (invalid) {
				stop("Broken value - not plotting\n")
			}
		}
	}
}


# Process command line arguments, return a list with the following items:
# logprefix - the directory containing ^run-* directories, which in turn contain
#             the run data
# runs      - a vector of the names of the individual ^run-* directories to read
#
runstoread <- function(args, logprefix="data/") {

    USAGE="Usage:\n \
          <script> [<logprefix>] [<index of first run> [<number of runs to read>]]\n\n"

    USAGE=paste(USAGE, "\t\tdefault logprefix =", logprefix, "\n")

	if (length(args) > 0) {
        # Argument is a file, might be a directory - take as logprefix
		if (file.exists(args[1])) {
			logprefix=args[1]
			if (substring(logprefix, nchar(logprefix)) != "/") {
				logprefix = paste(logprefix, "/", sep="")
				cat("Using logprefix", logprefix, "\n")
				args = args[-1]
			}
		}
	}

	runs = addruns(args, logprefix, print=FALSE)

	return(list(logprefix=logprefix, runs=runs))
}

addruns <- function(args, logprefix, print=F) {
	runs=c()
	runs_and_args = c(runs, add_runs_from_args(runs, args, logprefix))
	runs = runs_and_args$runs
	if (print) {
	cat("Runs after first call:", runs, "\n")
	}

	while (length(runs_and_args$args) < length(args) && length(runs_and_args$args) > 0) {
		args = runs_and_args$args
		runs_and_args = c(runs, add_runs_from_args(runs, args, logprefix))
		if (print) {
			cat("Adding runs:", runs_and_args$runs, "\n")
		}
		runs = c(runs, runs_and_args$runs)
		if (print) {
			cat("Args now:", args, "and runs_and_args$args:", runs_and_args$args, "\n")
		}
	}
	return(runs)
}

add_runs_from_args <- function(runs, args, logprefix) {

	startfromrun=1
	numberofruns=255

	if (length(args) > 0) {
		# Argument is not a directory, but a number - take as index of the run
		# to start with within the logprefix directory
		if (!is.na(suppressWarnings(as.numeric(args[1])))) {
			startfromrun = as.numeric(args[1])
			cat("Starting from run", startfromrun, "in", logprefix, "\n")
			args = args[-1]
		} else {
			cat("Did not understand argument", args[1], "\n")
			cat(USAGE, "\n")
			stop("Fix your arguments\n")
		}
	}
	# Another argument which is a number - take as the number of runs to read
	if (length(args) > 0) {
		if (!is.na(suppressWarnings(as.numeric(args[1])))) {
			numberofruns = as.numeric(args[1])
			cat("Only reading", numberofruns, "runs\n")
			args = args[-1]
		}
	}

	runs = list.files(path=logprefix, pattern="^run*")
	runs = runs[startfromrun:length(runs)]

	if (numberofruns < length(runs)) {
		runs = runs[1:numberofruns]
	}
	return(list(runs=runs, args=args))
}

check_fieldcounts <- function(dataframe, filename, print=FALSE, debugprint=FALSE) {
    # Count how many fields we see on each line in the file
    cf = count.fields(filename, sep=",")
    desiredlength = length(colnames(dataframe))
	if (debugprint) {
		cat("Before any removal: lines_too_short =", which(cf < desiredlength), "and lines_too_long =", which(cf > desiredlength), "\n")
	}

    # Any lines too short? Get their indices
    lines_too_short = which(cf < desiredlength)
    lines_too_long = which(cf > desiredlength)
	offset = 0
	pastlongline = 0
	if (length(lines_too_short) > 0) {
		for (shortline in lines_too_short) {
			if (debugprint) {
				cat("Processing short line", shortline, "\n")
			}
			for (longline in lines_too_long) {
				if (longline < shortline && longline > pastlongline) {
					offset = offset + 1
					pastlongline = longline
					if (debugprint) {
						cat(longline, "<", shortline, "--> offset =", offset, "\n")
					}
				}
			}
			# Lines are too short - remove them from the data frame
			lines_to_delete_here = shortline + offset
			if (print) {
				cat("Removed", length(lines_to_delete_here), "lines that were too short:", paste(lines_to_delete_here, sep=", "), "(offset =", offset, ")\n")
			}
			if (debugprint) {
				cat("removing items: ", as.character(dataframe[lines_to_delete_here,]$timestamp), "\n")
				cat("before (item", lines_to_delete_here[1] - 1, "): ", as.character(dataframe[(lines_to_delete_here[1] - 1),]$timestamp), "\n")
				cat("after (item", lines_to_delete_here[length(lines_to_delete_here)] + 1, "): ", as.character(dataframe[(lines_to_delete_here[length(lines_to_delete_here)] + 1),]$timestamp), "\n")
			}
			dataframe = dataframe[-lines_to_delete_here,]
			offset = offset - length(lines_to_delete_here)
		}
    }

	if (debugprint) {
		cat("\n\n")
	}
    # Any lines too long? Get their indices
    lines_too_long = which(cf > desiredlength)
    #offset=length(lines_too_short)
    offset = 0
    if (length(lines_too_long) > 0) {
        for(l in lines_too_long) {

			# Increment offset counter, so we delete the right lines
			for (shortline in lines_too_short) {
				if (l > shortline) {
					offset = offset + 1
					lines_too_short = lines_too_short[-1]
					if (debugprint) {
						cat(l, ">", shortline, "--> offset =", offset, "\n")
					}
				}
			}

            # Line too long - delete it and the following lines
            num_lines_to_delete_here = ceiling(cf[l] / desiredlength)
			if (debugprint) {
				cat("Computing what lines to delete: l=", l, ", l + num_lines_to_delete_here - 1 =", l + num_lines_to_delete_here - 1, ", subtracting offset =", offset , "\n")
			}
            lines_to_delete_here = seq(l, l + num_lines_to_delete_here - 1 , 1) - (offset )
			#str(dataframe[lines_to_delete_here,])
            if (print) {
                cat("Removed", num_lines_to_delete_here, "lines that were too long: ", paste(lines_to_delete_here, sep=", "), "(offset", offset, ")\n")
            }
			if (debugprint) {
				cat("removing items: ", as.character(dataframe[lines_to_delete_here,]$timestamp), "\n")
				cat("before (item", lines_to_delete_here[1] - 1, "): ", as.character(dataframe[(lines_to_delete_here[1] - 1),]$timestamp), "\n")
				cat("after (item", lines_to_delete_here[length(lines_to_delete_here)] + 1, "): ", as.character(dataframe[(lines_to_delete_here[length(lines_to_delete_here)] + 1),]$timestamp), "\n")
			}
            dataframe = dataframe[-lines_to_delete_here,]
            offset = offset + num_lines_to_delete_here - 1
			if (debugprint) {
				cat("\n\n")
			}
        }
    }
    if (length(lines_too_short) > 0 || length(lines_too_long) > 0) {
        for(column in colnames(dataframe)) {
            dataframe[[column]] = type.convert(as.character(dataframe[[column]]))
        }
    }
    return(fix.factors(dataframe))
}

gettimewindow <- function(data, xdata="timestamp", xmin=0, xmax=NA, n=NA) {
    if (class(data) == "list") {
        subsetlist = lapply(data, function(element) gettimewindow(element, xdata=xdata, xmin=xmin, xmax=xmax, n=n))

        return(subsetlist[vapply(subsetlist, Negate(is.null), NA)])
    } else if (class(data) == "data.frame") {
        if (!is.na(xmax)) {
            data = subset(data, data[[xdata]] < xmax)
        }
        subsetdata = fix.factors(subset(data, data[[xdata]] >= xmin))
        if (!is.na(n)) {
            if (!is.na(xmax)) {
                subsetdata = fix.factors(tail(subsetdata, n = n))
            } else {
                subsetdata = fix.factors(head(subsetdata, n = n))
            }
        }
        if (nrow(subsetdata) == 0) {
            return(NULL)
        } else {
            return(subsetdata)
        }
    }
}

gettimewindowaround <- function(data, xdata="timestamp", xmid=NA, around=10) {
    if (is.na(xmid)) {
        xmid=data[[xdata]][ceiling(length(data)/2)]
    }
    return(gettimewindow(data, xdata=xdata, xmin=xmid-around, xmax=xmid+around))
}

gettimestamps <- function(data, key, value, xdata="timestamp") {
    subsetdata = subset(data, data[[key]] == value)
    return(subsetdata[[xdata]])
}

gettimewindowfor <- function(data, tdata, key, value, around=0, xdata="timestamp") {
    timestamps = gettimestamps(tdata, key, value, xdata)
    cat("Data for", key, "==", value, "(", min(timestamps), "=<", xdata, "=<", max(timestamps), ", +-", around, ")\n")
    return(gettimewindow(data, xdata=xdata, xmin=min(timestamps)-around, xmax=max(timestamps)+around))
}

lastreadingbefore <- function(data, x, xdata="timestamp", print=FALSE) {
    timewindow = gettimewindow(data, xdata=xdata, xmax=as.numeric(x))
    if (print) {
        cat("Range:", paste(range(timewindow[xdata])), "xmax=", format(x, nsmall=3), "\n")
    }
    if (class(timewindow) == "data.frame") {
        timewindow = tail(timewindow, n=1)
    }
    return(timewindow)
}


# Call rbind on a list, but first filter out NULLs and check if all list items
# have the same length
safer_rbind <- function(datalist) {
    # Filter out all list items that are NULL
    datalist = datalist[vapply(datalist, Negate(is.null), NA)]
    if (length(datalist) == 0) {
        return(NULL)
    }

    datalistlength1 = length(datalist[[1]])
    if (all(lapply(datalist, function(x) length(x)) == datalistlength1)) {
        # All list items are similar - we can combine them to a single data frame
        data = do.call(rbind, datalist)
    } else {
        cat("Warning: Lengths of data items in list do not match.\n")
        cat("Some columns were filled with NAs.\n")
        data = do.call(rbind, filldataframes(datalist))
    }
    return(data)
}

filldataframes <- function(datalist) {
    for(i in 1:length(datalist)) {
        for (j in 1:length(datalist)) {
            datalist[[i]] = filldataframe(datalist[[i]], datalist[[j]])
        }
    }
    return(datalist)
}

filldataframe <- function(data, model) {
    for (colname in colnames(model)) {
        if (is.null(data[[colname]])) {
            data[[colname]] = NA
        }
    }
    return(data)
}

compute_rollmax <- function(data, metrics, n) {
    data[[paste(metrics, "max", n, sep="_")]] = rollmaxr(c(rep(0, n-1), data[[metrics]]), n)
    return(data)
}

compute_quantile_of_last_n <- function(data, metrics, n, q=0.9) {
    #str(data)
    data[[paste(metrics, paste("q", q*100, sep=""), n, sep="_")]] = runquantile(data[[metrics]], k=n, probs=c(q), endrule="quantile", align="right")
    #cat("After\n")
    #str(data)
    return(data)
}

# Prepare a PNG file of a given font size (=cex), width and height
# After invoking this function, plot and then invoke dev.off()
prepare_png <- function(filename="out.png", cex=2, width=cex*600, height=cex*400) {
	png(filename, width=width, height=height)
	cat("\n\tPlotting to", filename, "dimensions:", width, "x", height, "cex:", cex, "\n")
	par(mar=c(3+cex, 3+cex, cex*1.5, cex))
	par(cex=cex, cex.axis=cex, cex.lab=cex, cex.main=cex, cex.sub=cex)
}

# Prepare a PDF file of a given font size (=cex), width and height
# After invoking this function, plot and then invoke dev.off()
prepare_pdf <- function(filename="out.pdf", cex=2, width=cex*10, height=cex*7) {
	pdf(filename, width=width, height=height)
	cat("\n\tPlotting to", filename, "dimensions:", width, "x", height, "cex:", cex, "\n")
	par(mar=c(3+cex, 3+cex, cex*1.5, cex))
	par(cex=cex, cex.axis=cex, cex.lab=cex, cex.main=cex, cex.sub=cex)
}

get_ymax <- function(data, metrics) {
	if (class(data) == "list") {
		ymax=data[[1]][[metrics[1]]][[1]]
		for (d in data) {
			for (metric in metrics) {
				ymax = max(ymax, d[[metric]][!is.infinite(d[[metric]])], na.rm=TRUE)
			}
		}
	} else if (class(data) == "data.frame") {
		ymax=data[[metrics[1]]][[1]]
		for (metric in metrics) {
			ymax = max(ymax, data[[metric]][!is.infinite(data[[metric]])], na.rm=TRUE)
		}
	} else {
		ymax = max(data[[metrics[[1]]]][!is.infinite(data[[metrics[[1]]]])], na.rm=TRUE)
	}
	return(ymax)
}

get_ymin <- function(data, metrics) {
	if (class(data) == "list") {
		ymin=data[[1]][[metrics[1]]][[1]]
		for (d in data) {
			for (metric in metrics) {
				ymin = min(ymin, d[[metric]], na.rm=TRUE)
			}
		}
	} else if (class(data) == "data.frame") {
		ymin=data[[metrics[1]]][[1]]
		for (metric in metrics) {
			ymin = min(ymin, data[[metric]], na.rm=TRUE)
		}
	} else {
		ymin = min(data[[metrics[[1]]]], na.rm=TRUE)
	}
	return(ymin)
}

get_xmax <- function(data, xdata) {
	if (class(data) == "list") {
		xmax=data[[1]][[xdata]][[1]]
		for (d in data) {
			xmax = max(xmax, d[[xdata]][!is.infinite(d[[xdata]])], na.rm=TRUE)
		}
	} else if (class(data) == "data.frame") {
		xmax=data[[xdata[1]]][[1]]
		metrics = xdata
		for (metric in metrics) {
			xmax = max(xmax, data[[xdata]][!is.infinite(data[[xdata]])], na.rm=TRUE)
		}
	} else {
		xmax = max(data[[xdata[[1]]]][!is.infinite(data[[xdata[[1]]]])], na.rm=TRUE)
	}
	return(xmax)
}

get_min <- function(data1, data2=NA, na.rm=TRUE, log=FALSE) {
	if (log) {
		data1 = data1[data1 > 0]
		if (any(!is.na(data2))) {
			data2 = data2[data2 > 0]
		} else {
			data2 = data1
		}
	}
	return(min(data1, data2, na.rm=na.rm))
}

get_xmin <- function(data, xdata, metrics=xdata, log=FALSE) {
	if (class(data) == "list") {
		if (log) {
			firstdata = data[[1]][[xdata]][[1]]
			firstdata = firstdata[firstdata > 0]
			if (length(firstdata) > 0) {
				xmin = min(firstdata)
			} else {
				xmin = Inf
			}
		} else {
			xmin=data[[1]][[xdata]][[1]]
		}
		for (d in data) {
			xmin = get_min(xmin, d[[xdata]], na.rm=TRUE, log=log)
		}
	} else if (class(data) == "data.frame") {
		if (log) {
			firstdata = data[[xdata[1]]][[1]]
			firstdata = firstdata[firstdata > 0]
			xmin = min(firstdata)
		} else {
			xmin=data[[xdata[1]]][[1]]
		}
		for (metric in metrics) {
			xmin = get_min(xmin, data[[xdata]], na.rm=TRUE, log=log)
		}
	} else {
		xmin = get_min(data[[xdata[[1]]]], na.rm=TRUE, log=log)
	}
	cat("\n")
	return(xmin)
}
get_datalimits <- function(data)
{
	xmin_left = NA
	xmax_left = NA
	xmin_center = NA
	xmax_center = NA
	xmin_right = NA
	xmax_right = NA

	if (is.null(data) || all(is.na(data))) {
		cat("Cannot get data limits!\n")
	}

	if (class(data) == "numeric") {
		# Data is a vector
		l = length(data)
		# first third goes until this index
		l1 = ceiling(l/3)
		# second third goes until this index
		l2 = ceiling(l/3) * 2

		xmax_left = max(data[1:l1], na.rm=T)
		xmin_left = min(data[1:l1], na.rm=T)
		xmax_center = max(data[l1:l2], na.rm=T)
		xmin_center = min(data[l1:l2], na.rm=T)
		xmax_right = max(data[l2:l], na.rm=T)
		xmin_right = min(data[l2:l], na.rm=T)
	} else if (class(data) == "matrix") {
		# Data is a matrix
		l = max(dim(data))
		l1 = ceiling(l/3)
		# second third goes until this index
		l2 = ceiling(l/3) * 2
		if (dim(data)[1] > dim(data)[2]){
			data = t(data)
		}
		xmax_left = max(data[,1:l1], na.rm=T)
		xmin_left = min(data[,1:l1], na.rm=T)
		xmax_center = max(data[,l1:l2], na.rm=T)
		xmin_center = min(data[,l1:l2], na.rm=T)
		xmax_right = max(data[,l2:l], na.rm=T)
		xmin_right = min(data[,l2:l], na.rm=T)
	}

	return(c(xmin_left, xmin_center, xmin_right, xmax_left, xmax_center, xmax_right))
}


find_empty_plotarea <- function(data = NULL, ylim = c(min(dmins), max(dmaxs)), metricslog = FALSE, dmins = c(NA, NA, NA), dmaxs = c(NA, NA, NA), fallback=TRUE, numlegends=1, verbose=F)
{
	# Compute data mins and maxs
	dlims = get_datalimits(data)
	if (any(is.na(dmins))) {
		dmins = dlims[1:3]
	}
	if (any(is.na(dmaxs))) {
		dmaxs = dlims[4:6]
	}
	if (all(is.na(dmins)) || all(is.na(dmaxs))) {
		return("")
	}

	if (any(is.na(ylim))) {
		ylim = c(min(data), max(data))
	}

	threshold1 = ylim[1] + (ylim[2] - ylim[1]) * 4/9
	threshold2 = ylim[1] + (ylim[2] - ylim[1]) * 5/9

    if (verbose) {
        cat("ylims:", ylim[1], ylim[2], "\n")
        cat("Thresholds:", threshold1, threshold2, "\n")
        cat("dmins:", dmins[1], dmins[2], dmins[3], "\n")
        cat("dmaxs:", dmaxs[1], dmaxs[2], dmaxs[3], "\n")
    }

	choices = c()

	# Trying to find areas where legend will definitely fit
	if (dmaxs[3] < threshold2 ) {
		choices = c(choices, "topright")
	}
	if (dmaxs[1] < threshold2) {
		choices = c(choices, "topleft")
	}
	if (dmins[3] > threshold1 ) {
		choices = c(choices, "bottomright")
	}
	if (dmins[1] > threshold1 ) {
		choices = c(choices, "bottomleft")
	}
	if (dmaxs[2] < threshold2) {
		choices = c(choices, "top")
	}
	if (dmins[2] > threshold1) {
		choices = c(choices, "bottom")
	}

	if (length(choices) < numlegends && fallback) {
		# Sort data maxima, append the corresponding position to choices
		inds = sort(dmaxs, index.return=T)$ix
		positions = c("topleft", "top", "topright")
		choices = c(choices, positions[inds])
		choices = unique(choices)
        if (verbose) {
            cat("Fallback choices:", choices, "\n")
        }
	}

	if (length(choices) >= numlegends) {
        if (verbose) {
            cat("Choices:",choices,"- choosing no.",numlegends,"\n\n")
        }
		return(choices[numlegends])
	} else {
        if (verbose) {
            cat("Choices:",choices,"- not enough, needed" , numlegends, "\n\n")
        }
		return("")
	}
}

get_pages_by_description <- function(description, inputpages) {
    lastpart = unlist(lapply(strsplit(inputpages, "/"), function(x) return(x[length(x)])))
    pagedataframe = do.call(rbind, lapply(strsplit(lastpart, "[-.]"), function(y) return(data.frame(size=y[2], times=y[3], type=y[4]))))
    
    if (description == "all") {
        return(inputpages)
    }
    if (description == "small") {
        pages1 = subset(pagedataframe,  (size %in% c("1kb") & times %in% c("32")))
        pages2 = subset(pagedataframe,  (size %in% c("10kb") & times %in% c("16", "32")))
        pagesubset = rbind(pages1, pages2)
        return(inputpages[as.numeric(rownames(pagesubset))])
    }
    if (grepl("med", description)) {
        pages1 = subset(pagedataframe,  (size %in% c("100kb") & times %in% c("4", "8", "32")))
        pages2 = subset(pagedataframe,  (size %in% c("1mb") & times %in% c("2")))
        pagesubset = rbind(pages1, pages2)
        return(inputpages[as.numeric(rownames(pagesubset))])
    }
    if (grepl("mixed", description)) {
        workloads = c("pics-1kb-16_10kb-8_100kb-4.html", "pics-1kb-32_10kb-16_100kb-2_200kb-2.html", "pics-10kb-4_100kb-4_1mb-2.html", "pics-10kb-16_100kb-8_200kb-4.html")
        pageindices = sapply(workloads, function(x) which(grepl(x, inputpages)))
        pageindices = pageindices[is.integer(pageindices)]
        if (length(pageindices) > 0) {
            pagesubset = inputpages[pageindices]
        } else {
            pagesubset = c()
        }
        return(pagesubset)
    }

    if (grepl("big", description) | grepl("large", description)) {
        pages1 = subset(pagedataframe,  (size %in% c("100kb") & times %in% c("64")))
        pages2 = subset(pagedataframe,  (size %in% c("200kb") & times %in% c("32")))
        pages3 = subset(pagedataframe,  (size %in% c("1mb") & times %in% c("4", "8")))
        pagesubset = rbind(pages1, pages2, pages3)
        return(inputpages[as.numeric(rownames(pagesubset))])
    }
    if (grepl("websites", description)) {
        return(inputpages[!grepl(".html", inputpages)])
    }
    return(inputpages[grepl(description, inputpages)])
}

substitute_addresses_by_interfaces <- function(label, addresses=c("10.1.1.3", "10.1.1.4", "10.1.2.3", "10.1.2.4", "130.149.221.201", "10.0.1.2", "10.0.2.2", "eth1.550", "eth6"), interfaces=c("wlan0", "eth0", "wlan1", "eth0", "server", "if1", "if2", "if1", "if2")) {
    for (i in seq(1, length(addresses))) {
        label = gsub(addresses[i], interfaces[i], label)
    }
    return(label)
}

get_crosstraffic_from_run_string <- function(label, print=FALSE) {
    scenariopart = ""
	s=strsplit(label, "[/_:+]")[[1]]
	shaperindex = which(grepl("shaper", s))
	if (length(shaperindex) > 0) {
		scenariopart = paste(ifelse(grepl("test-", s[shaperindex]) || grepl("^[[:digit:]]", s[shaperindex]), substring(s[shaperindex], 4), s[shaperindex]), ifelse(grepl("indoor", s[shaperindex+1]), paste("_", gsub("CROSS", "_", gsub("shaper", "", gsub("sameAP", "", s[shaperindex+1]))), sep=""), ""), sep="")
		scenariopart = gsub("shaper_indoor-03", "", scenariopart)
		scenariopart = gsub("_", "", scenariopart)
        scenariopart = paste("CROSS-", scenariopart, sep="")
	} else {
		scenariopart = ""
	}
    return(scenariopart)
}

get_qsize_from_run_string <- function(label, print=FALSE) {
    qsizestring = ""
	s=strsplit(label, "[/_:+]")[[1]]
    qsizeindex = which(grepl("qsize", s))
    if (length(qsizeindex) > 0) {
        qsizematch = regexpr("qsize", s[qsizeindex])
        qsizestring = paste("qsize", gsub("qsize", "", substring(s[qsizeindex], qsizematch+attr(qsizematch, "match.length"))), sep="")
    }
    return(qsizestring)
}

get_rtt_from_run_string <- function(label, pos=1, print=FALSE) {
    if(print) {
        cat("-- Getting RTT", pos, "from:", label, "\n")
    }
    rttstring = ""
	s=strsplit(label, "[-/_:+]")[[1]]
    foundindex = which(grepl("ms", s))
    if (length(foundindex) >= pos) {
        rttstring = s[foundindex[pos]]
    }
    if (print) {
        cat("Shaped RTT:", rttstring, "\n")
    }
    return(rttstring)
}

get_bw_from_run_string <- function(label, pos=1, print=FALSE) {
    if(print) {
        cat("-- Getting BW", pos, "from:", label, "\n")
    }
    returnstring = ""
	s=strsplit(label, "[-/_:+]")[[1]]
    foundindex = which(grepl("[kM]bit", s))
    if (length(foundindex) >= pos) {
        returnstring = s[foundindex[pos]]
    }
    if (print) {
        cat("BW:", returnstring, "\n")
    }
    return(returnstring)
}

get_loss_from_run_string <- function(label, pos=1, print=FALSE) {
    if(print) {
        cat("-- Getting loss", pos, "from:", label, "\n")
    }
    returnstring = ""
	s=strsplit(label, "[-/_:+]")[[1]]
    foundindex = which(grepl("loss", s))
    if (length(foundindex) >= pos) {
        returnstring = s[foundindex[pos]]
    }
    if (print) {
        cat("Shaped loss:", returnstring, "\n")
    }
    return(returnstring)
}



# Make more advanced plot label out of workload label, policy label and scenario label
make.plotlabel <-function(s, print=FALSE, exclude="", breaklineafter=30) {
    if (length(s) == 1) {
        s = strsplit(s, "[/_:+]")[[1]]
    }
    if (print) {
        cat(s, "\n")
    }
    indicatorpart = ""
    indicatorindex = which(grepl("simulated", s) | grepl("actual", s))
    if (length(indicatorindex) > 0) {
        indicatorpart = s[indicatorindex]
    }

	workloadindex = which(grepl("[0-9]+[k|m]b-[0-9]+", s))
	if (length(workloadindex) > 0) {
		workloadpart = paste(s[workloadindex], collapse="_")
        workloadpart = gsub(".html", "", workloadpart)
        workloadpart = gsub("pics-", "", workloadpart)
	} else {
		workloadindex = which(grepl("^[0-9]+[k|m]b", s))
		if (length(workloadindex) > 0) {
			workloadpart = paste(s[workloadindex], ':', s[workloadindex+1], sep='', collapse="_")
		} else {
			workloadindex = which(grepl("http", s))
			if (length(workloadindex) > 0) {
				workloadpart = gsub("www.", "", s[workloadindex+3])
			} else {
				workloadpart = ""
			}
		}
	}
	if (grepl("workload", exclude)) {
		workloadpart = ""
	}

	policyindex = which(grepl("eaf", s) | grepl("eth0", s) | grepl("wlan0", s) | grepl("earliest", s) | grepl("if1", s) | grepl("if2", s))
	if (length(policyindex) > 0) {
		policypart = paste(s[policyindex], ifelse(grepl("eaf", s[policyindex]), ifelse(is.na(s[policyindex+1]) | s[policyindex+1] == "", "", paste("_", s[policyindex+1], sep="")), ""), sep="")
        policypart = gsub("_shaper-simu", "", policypart)
	} else {
		policypart = ""
	}
	if (grepl("policy", exclude)) {
		policypart = ""
	}

	shaperindex = which(grepl("shaper", s)) + 1
	if (length(shaperindex) > 0) {
		scenariopart = paste(ifelse(grepl("test-", s[shaperindex]) || grepl("^[[:digit:]]", s[shaperindex]), substring(s[shaperindex], 4), s[shaperindex]), ifelse(grepl("indoor", s[shaperindex+1]), paste("_", gsub("CROSS", "_", gsub("shaper", "", gsub("sameAP", "", s[shaperindex+1]))), sep=""), ""), sep="")
		if (grepl("sym", scenariopart)) {
			s = strsplit(scenariopart, "-")[[1]]
			scenariopart = s[which(grepl("sym", s))]
		}
		scenariopart = gsub("shaper_indoor-03", "", scenariopart)
		scenariopart = gsub("_", "", scenariopart)
	} else {
		scenariopart = ""
	}
    if (indicatorpart != "") {
        scenariopart = paste(scenariopart, indicatorpart, sep="_")
    }

    crossindex = which(grepl("CROSS", s))
    if (scenariopart == "" && length(crossindex) > 0) {
        scenariopart = paste(scenariopart, s[crossindex], sep="")
    }

    qsizeindex = which(grepl("qsize", s))
    if (length(qsizeindex) > 0 & !grepl("qsize", exclude)) {
        qsizematch = regexpr("qsize", s[qsizeindex])
        scenariopart = paste(scenariopart, ifelse(scenariopart=="", "", ", "), "queue size = ", substring(s[qsizeindex], qsizematch+attr(qsizematch, "match.length")), sep="")
    }
	if (grepl("scenario", exclude) || grepl("run", exclude)) {
		scenariopart=""
	}

	if (print) {
		cat("Workload part:", workloadpart, ", policy part:", policypart, ", scenario part:", scenariopart, "\n")
	}

	lastlinebreak=0
	sep1 = " "
	sep2 = " "
	if (sum(nchar(workloadpart)) > breaklineafter) {
		sep1 = "\n"
		lastlinebreak = nchar(workloadpart)
	}
	chars_of_workload_and_policy = sum(sum(nchar(workloadpart)) + nchar(policypart))
	linebreak_threshold_2 = lastlinebreak + breaklineafter
	if (chars_of_workload_and_policy > linebreak_threshold_2 && scenariopart != "") {
		sep2 = "\n"
	}
	return(paste(ifelse(workloadpart == "", "", paste(workloadpart , ",", sep1, sep="")), policypart, sep2, ifelse(scenariopart == "", "", paste("(", scenariopart, ")", sep="")), sep="", collapse=""))
}

get_workload <- function(label) {
    if (grepl("alexa1000", label)) {
        return("Alexa 1000")
    }
    if (grepl("alexa10k", label)) {
        return("Alexa 10001-11000")
    }
	return(label)
}

get_browser_label <- function(label) {
    if (grepl("selenium", label)) {
        return("Firefox")
    }
    if (grepl("marionette", label)) {
        return("Firefox")
    }
    if (grepl("chrome", label)) {
        return("Chrome")
    }
	return(label)
}



get_methodology_label <- function(label) {
    #if (grepl("wait5s", label)) {
    #    return("Firefox + Selenium (wait 5s)")
    #}
    #if (grepl("wait10s", label)) {
    #    return("Firefox + Selenium (wait 10s)")
    #}
    if (grepl("selenium", label)) {
        return("Firefox + Selenium")
    }
    if (grepl("marionette", label)) {
        return("Firefox + Marionette")
    }
    if (grepl("chrome", label)) {
        return("Chrome DevTools")
    }
	return(label)
}

metricsnames.human.readable <- function(label) {
    if (grepl("PLT", label)) {
        return(gsub("PLT_without_redirects", "PLT [s]", label))
    }
    if (grepl("TTFP", label)) {
        return(gsub("TTFP_without_redirects", "TTFP [ms]", label))
    }
    if (grepl("ATF", label)) {
        cat("label: ", label, "\n")
        return(gsub("ATF_time", "ATF [s]", label))
    }
}

policynames.human.readable <- function(label, additionaldata="") {
	if (grepl("absdiff", label)) {
		label = gsub("^", "Absolute difference of ", label)
		label = gsub("_absdiff", "", label)
	}
	if (grepl("bypass-testbed-wlan0", label)) {
		return(paste(gsub("bypass-testbed-wlan0", "Using only WiFi", label), additionaldata))
	}
	if (grepl("eaf_max_stick", label)) {
		return(paste(gsub("eaf_max_stick", "Using Socket Intents", label), additionaldata))
	}
	if (grepl("stick", label)) {
		return(paste(gsub("stick", "Using only LTE", label), additionaldata))
	}
    if (grepl("eaf", label)) {
        label = gsub("eaf_max", "eaf", label)
        return(gsub("eaf", "IANS", label))
    }
    if (grepl("eth0_or_wlan0", label)) {
        return(gsub("eth0_or_wlan0", "Only lower PLT network", label))
    }
    if (grepl("eth0", label)) {
        #return(paste(gsub("eth0", "Only network 1 (no cross-traffic) ", label), additionaldata))
        return(paste(gsub("eth0", "Only network 1 (constant) ", label), additionaldata))
        #return(paste(gsub("eth0", "Only network 1\n (constant) ", label), additionaldata)) # FOR HEATMAP
        #return(paste(gsub("eth0", "Only network 2 ", label), additionaldata))
        #return(paste(gsub("eth0", "Only network 1 (10 ms, 2 Mbit/s)", label), additionaldata))
        #return(paste(gsub("eth0", "Using only low latency network", label), additionaldata))
    }
    if (grepl("wlan0", label)) {
        #return(paste(gsub("wlan0", "Only network 2 (with harpoon) ", label), additionaldata))
        #return(paste(gsub("wlan0", "Net2 ", label), additionaldata))
        return(paste(gsub("wlan0", "Only network 2 (variable) ", label), additionaldata))
        #return(paste(gsub("wlan0", "Only network 2\n(variable) ", label), additionaldata)) # FOR HEATMAP
        #return(paste(gsub("wlan0", "Only network 1 ", label), additionaldata))
        #return(paste(gsub("wlan0", "Only network 2 (100 ms, 20 Mbit/s)", label), additionaldata))
        #return(paste(gsub("wlan0", "Using only high bandwith network", label), additionaldata))
    }
    if (grepl("mptcp_selective", label)) {
        #return(paste(gsub("mptcp_selective", "IANS (Selective MPTCP)", label), additionaldata))
        return("IANS (Selective MPTCP)") # for barplot
        #return("IANS\n(Selective MPTCP)") # FOR HEATMAP
    }
    if (grepl("mptcp", label)) {
        return(gsub("mptcp", "MPTCP", label))
    }
    if (grepl("rr", label)) {
        return("Round Robin of eth0, wlan0")
    }
	if (grepl("video_pessimist", label)) {
		#return(paste(gsub("video_pessimist", "Pess", label), additionaldata))
		return(paste(gsub("video_pessimist", "IANS (Pessimist)", label), additionaldata))
		#return(paste(gsub("video_pessimist", "IANS\n(Pessimist)", label), additionaldata)) # FOR HEATMAP
	}
	if (grepl("video", label)) {
		#return(paste(gsub("video", "Opt", label), additionaldata))
		return(paste(gsub("video", "IANS (Optimist)", label), additionaldata))
		#return(paste(gsub("video", "IANS\n(Optimist)", label), additionaldata)) # FOR HEATMAP
	}
}

get_short_scenario_string <- function(label) {
	#cat("Getting for", label, "\n")
    s = strsplit(label, "[/_:+-]")[[1]]
    variableindex = which(grepl("metro|bus|tram|train|car|ferry", s))[1]
    factor = as.numeric(s[variableindex + 1])
	if (is.na(variableindex) | is.na(factor)) {
		harpoonfactor = get_harpoon_factor(label)
		if (is.null(harpoonfactor) ) {
			return("lesscapa")
		}
		return(paste("harpoon", harpoonfactor, sep=""))
	}
    return(paste(s[variableindex], factor, sep=""))
}

get_harpoon_factor <- function(label) {
	#cat("Getting harpoon factor for ", label, "\n")
    if (grepl("shaper_harpoon2_", label)) {
        s = strsplit(label, "[/_:+-]")[[1]]
        variableindex = which(grepl("harpoon2", s))[1]
        factor = s[variableindex + 1]
		if (grepl("shaper", factor)) {
			factor=2
		}
		#cat(label, "factor:", factor, "\n")
        return(factor)
    }
    if (grepl("shaper_harpoon_", label)) {
        s = strsplit(label, "[/_:+-]")[[1]]
        variableindex = which(grepl("harpoon", s))[1]
        factor = gsub("harpoon", "", s[variableindex])
		#cat(label, "factor:", factor, "\n")
        return(factor)
    }
}

plotlabel.human.readable <- function(label, exclude="", additionaldata="") {
    #cat("Label:", label, "\n")
    if (is.null(label)) {
        return("NULL")
    }
	if (label == "if1_bw") {
		return("Downstream capacity shaped on network 1\n(Network 2 fixed at 20 Mbit/s)")
	}
	if (label == "if1_rtt") {
		return("Latency shaped on network 1\n(Network 2 fixed at 100ms)")
	}
	if (label == "if2_bw") {
		return("Downstream capacity shaped on network 2\n(Network 1 fixed at 2 Mbit/s)")
		#return("Downstream capacity shaped on both networks\n ")
	}
	if (label == "if2_rtt") {
		return("Latency shaped on network 2\n(Network 1 fixed at 10 ms)")
		#return("Latency shaped on both networks\n ")
	}
	if (label == "value_index") {
		return("Segment index")
	}
	if (label == "second") {
		return("Time [s]")
	}
	if (label == "bufferlevel") {
		return("Buffer level known at ABR decision time [s]")
	}
	if (label == "representation") {
		return("Representation played")
	}
	if (grepl("qoe", label)) {
		return(capitalize(gsub("qoe", "MOS", gsub(" [s]", "", label))))
	}
	if (label == "dl_rate_prev") {
		return("Previous segment download rate known to ABR [kbit/s]")
	}
	if (label == "downloadtime") {
		return("Download time of segment [s]")
	}
	if (label == "count stalling_duration [s]") {
		return("Number of stallings")
	}
	if (label == "stalling_duration") {
		return("Stalling duration [s]")
	}
	if (grepl("10.1.2.4_None_run-", label) | grepl("eth0_run-", label)) {
        return("eth0")
    }
	if (grepl("10.1.1.3_None_run-", label) | grepl("wlan0_run-", label)) {
        return("wlan0")
    }
	if (label == "median_of_rtt_vars") {
		#return("Latency shaped on network 2\n(Network 1 fixed at 10 ms)")
		return("SRTT variation within connections ")
	}
	if (label == "srtt_var") {
		#return("Latency shaped on network 2\n(Network 1 fixed at 10 ms)")
		return("SRTT variation among connections ")
	}
	if (label == "eth0:run-2019-04-02T09:35-mirror_shufflepolicies_shaper_symmetric_test-10ms-2Mbit-0loss_test-10ms-2Mbit-0loss-qsize25-qsize25") {
		return("No cross-traffic")
	}
	if (label == "eth0:run-2019-05-05T07:34-mirror_shufflepolicies_cross200M_shaper_symmetric_test-10ms-2Mbit-0loss_test-10ms-2Mbit-0loss-qsize25-qsize25") {
		return("No cross-traffic")
	}
	if (label == "eth0:run-2019-05-01T16:42-mirror_shufflepolicies_crossHARPOON_shaper_symmetric_test-10ms-2Mbit-0loss_test-10ms-2Mbit-0loss-qsize25-qsize25") {
		return("No cross-traffic")
	}
	if (label == "wlan0:run-2019-04-02T09:35-mirror_shufflepolicies_shaper_symmetric_test-10ms-2Mbit-0loss_test-10ms-2Mbit-0loss-qsize25-qsize25") {
		return("No cross-traffic")
	}
	if (label == "wlan0:run-2019-03-13T10:55-mirror_shufflepolicies_shaper_test-10ms-2Mbit-0loss_test-100ms-20Mbit-0loss-qsize25-qsize250") {
		return("No cross-traffic")
	}

	if (label == "wlan0:run-2019-05-05T07:34-mirror_shufflepolicies_cross200M_shaper_symmetric_test-10ms-2Mbit-0loss_test-10ms-2Mbit-0loss-qsize25-qsize25") {
		return("UDP cross-traffic")
	}
	if (label == "wlan0:run-2019-05-01T16:42-mirror_shufflepolicies_crossHARPOON_shaper_symmetric_test-10ms-2Mbit-0loss_test-10ms-2Mbit-0loss-qsize25-qsize25") {
		return("TCP cross-traffic")
	}
	if (label == "wlan0:run-2019-04-15T10:20-mirror_shufflepolicies_cross200M_shaper_test-10ms-2Mbit-0loss_test-100ms-20Mbit-0loss-qsize25-qsize250") {
		return("UDP cross-traffic")
	}
	if (label == "wlan0:run-2019-04-26T14:38-mirror_shufflepolicies_crossHARPOON_shaper_test-10ms-2Mbit-0loss_test-100ms-20Mbit-0loss-qsize25-qsize250") {
		return("TCP cross-traffic")
	}
	if (grepl("eth0:run-|wlan0:run-", label)) {
        label = policynames.human.readable(label, additionaldata)
        s = strsplit(label, "[/_:+-]")[[1]]
		latencyindex = which(grepl("ms", s))
		bandwidthindex = which(grepl("Mbit", s))
		scenariopart = ""
		for (i in seq(1, length(latencyindex))) {
			scenariopart = paste(scenariopart, s[latencyindex[i]], "-", s[bandwidthindex[i]], sep="")
			if (i < length(latencyindex)) {
				scenariopart = paste(scenariopart, ", ", sep="")
			} else {
				scenariopart = paste(scenariopart, "", sep="")
			}
		}
		interfacepart = s[which(grepl("network", s))]
		crosstrafficindex = which(grepl("cross", s))
		crosstrafficpart = s[crosstrafficindex]
		#cat("Interfacepart:", interfacepart, "crosstrafficpart:", crosstrafficpart, "scenariopart:", scenariopart, "\n")
		return(paste(interfacepart, crosstrafficpart, scenariopart))
	}
    if (grepl("videotest_shaper_variable", label)) {
        s = strsplit(label, "[/_:+-]")[[1]]
        variableindex = which(grepl("metro|bus|tram|train|car|ferry", s))[1]
        factor = as.numeric(s[variableindex + 1])
		if (factor == 1) {
			return("218")
		}
		if (factor == 1.5) {
			return("327")
		}
		if (factor == 2) {
			return("436")
		}
		if (factor == 2.5) {
			return("545")
		}
		if (factor == 4) {
			return("872")
		}
		if (factor == 5) {
			return("1090")
		}
		if (factor == 10) {
			return("2180")
		}
        return(paste(s[variableindex], factor, sep=""))
    }
    if (grepl("wlan0:cross|video:cross|video_pessimist:cross", label)) {
		#cat("match!\n")
        policyname = policynames.human.readable(gsub(":cross.*", "", label))
		harpoonfactor = get_harpoon_factor(label)
		#cat("Got policyname", policyname, "and harpoonfactor", harpoonfactor, "\n")
		return(paste(policyname, harpoonfactor))
    }
    if (grepl("BOLA_O:cross|BOLA_BASIC:cross|BBA-0:cross", label)) {
		#cat("match!\n")
        abrname = (gsub(":cross.*", "", label))
		harpoonfactor = get_harpoon_factor(label)
		#cat("Got policyname", policyname, "and harpoonfactor", harpoonfactor, "\n")
		return(paste(harpoonfactor, " (", abrname, ")", sep=""))
    }
    if (grepl("shaper_harpoon2_", label)) {
        s = strsplit(label, "[/_:+-]")[[1]]
        variableindex = which(grepl("harpoon2", s))[1]
        factor = s[variableindex + 1]
		if (grepl("shaper", factor)) {
			factor=2
		}
		#cat(label, "factor:", factor, "\n")
        return(factor)
        #return(gsub("shaper", "", paste(factor, " harpoon", sep="")))
        #return(gsub("shaper", "", paste("100ms-2/5Mbit\n+", factor, "*harpoon", sep="")))
    }
    if (grepl("shaper_harpoon_", label)) {
        s = strsplit(label, "[/_:+-]")[[1]]
        variableindex = which(grepl("harpoon", s))[1]
        factor = gsub("harpoon", "", s[variableindex])
		#cat(label, "factor:", factor, "\n")
        return(factor)
        #return(gsub("shaper", "", paste(factor, " harpoon", sep="")))
        #return(gsub("shaper", "", paste("10ms-2/5Mbit\n+", factor, "*harpoon", sep="")))
    }
	if (grepl("shufflepolicies_shaper", label)) {
        s = strsplit(label, "[/_:+-]")[[1]]
		latencyindex = which(grepl("ms", s))
		bandwidthindex = which(grepl("Mbit", s))
		scenariopart = ""
		for (i in seq(1, length(latencyindex))) {
			scenariopart = paste(scenariopart, s[latencyindex[i]], "-", s[bandwidthindex[i]], sep="")
			if (i < length(latencyindex)) {
				scenariopart = paste(scenariopart, ", ", sep="")
			} else {
				scenariopart = paste(scenariopart, "", sep="")
			}
		}
		return(scenariopart)
	}
    if (grepl("eth0|wlan0|eaf_max|mptcp|rr|stick|video", label)) {
        label = policynames.human.readable(label, additionaldata)
        #cat("substituted:", label, "\n")
    }
	if (label == "min_PLT_without_redirects_absdiff") {
		return("Best achieved PLT speedup [s] using IANS compared to a single network")
	}
    if (grepl("PLT_without_redirects_absdiff", label)) {
        return("Absolute difference of PLT [ms]")
    }
    if (any(c("PLT_without_redirects", "PLT_with_redirects", "TTFP_without_redirects", "ATF_time") %in% label)) {
        return(metricsnames.human.readable(label))
    }
    if (grepl("MOS_ByteIndex_ATF", label)) {
		if (grepl("diff", label)) {
			return(gsub("MOS_ByteIndex_ATF", "MOS", label))
		} else {
			return(gsub("MOS_ByteIndex_ATF", "MOS based on ByteIndex up to ATF", label))
		}
    }
	if (label == "median_page_size") {
		return("Page size [MB]")
	}
	if (label == "min_ATF_time_absdiff") {
		return("Best achieved ATF speedup [s] using IANS compared to a single network")
	}
    if (grepl("res_number_of_resources_finished_before_onload_-_har_non_failed_requests_Alexa_10001-11000", label)) {
        return("HAR - Res (Alexa 10001-11000)")
    }
    if (grepl("res_number_of_resources_finished_before_onload_-_har_non_failed_requests_Alexa_1000", label)) {
        return("HAR - Res (Alexa 1000)")
    }
	if (grepl("Firefox", label) | grepl("Chrome", label)) {
		return(label)
	}
    if (grepl("Redirect_share_of_PLT", label)) {
        return("PLT")
    }
    if (grepl("median_PLT_with_redirects_per_page", label)) {
        return("PLT with redirects")
    }
    if (grepl("median_PLT_without_redirects_per_page", label)) {
        return("PLT without redirects")
    }
    if (grepl("Redirect_share_of_domContentLoaded", label)) {
        return("domContentLoaded")
    }
    if (grepl("Redirect_share_of_TTFB", label)) {
        return("TTFB")
    }
    if (grepl("Redirect_share_of_firstPaint", label)) {
        return("TTFP")
    }
    if (grepl("trace_headerlen_-_har_headerlen", label)) {
        return("Trace - HAR header size")
    }
    if (grepl("har_headerlen_-_trace_headerlen", label)) {
        return("HAR - Trace header size")
    }
    if (grepl("trace_bodylen_-_har_bodylen", label)) {
        return("Trace - HAR")
    }
    if (grepl("har_bodylen_-_trace_bodylen", label)) {
        return("HAR - Trace")
    }
    if (grepl("trace_bodylen_-_har_contentlengthheader", label)) {
        return("Trace - Content-Length")
    }
    if (grepl("har_contentlengthheader_-_trace_bodylen", label)) {
        return("Content-Length - Trace")
    }
    if (grepl("trace_bodylen_-_res_bodylen", label)) {
        return("Trace - Res")
    }
    if (grepl("res_bodylen_-_trace_bodylen", label)) {
        return("Res - Trace")
    }
    if (grepl("har_requests_before_onload_-_res_number_of_resources_finished_before_onload", label)) {
        return("HAR - Resource Timings Entries")
    }
    if (grepl("res_number_of_resources_finished_before_onload_-_har_requests_before_onload", label)) {
        return("HAR - Resource Timings Entries")
    }
    if (grepl("res_number_of_resources_finished_before_onload_-_har_non_failed_requests", label)) {
        return("HAR - Resource Timings Entries")
    }
    if (grepl("har_non_failed_requests_-_res_number_of_resources_finished_before_onload", label)) {
        return("HAR (non failed) - Resource Timings Entries")
    }
    if (grepl("har_bodylen_-_har_contentlengthheader", label)) {
        return("HAR - Content-Length")
    }
    if (grepl("res_bodylen_-_har_contentlengthheader", label)) {
        return("Res - Content-Length")
    }
    if (grepl("har_contentlengthheader_-_har_bodylen", label)) {
        return("Content-Length - HAR")
    }
    if (grepl("res_bodylen_-_har_bodylen", label)) {
        return("Res - HAR")
    }
    if (grepl("har_contentlengthheader_-_res_bodylen", label)) {
        return("Content-Length - Res")
    }
    if (grepl("har_bodylen_-_res_bodylen", label)) {
        return("HAR - Res")
    }
    if (grepl("res_decoded_-_har_contentsize", label)) {
        return("Res - HAR decoded")
    }
    if (grepl("har_contentsize_-_res_decoded", label)) {
        return("HAR - Res decoded")
    }
    if (grepl("har_byte_index_bodyorcontent_-_har_byte_index_bodysize_Alexa_10001-11000", label)) {
        return("CL - HAR (Alexa 10001-11000)")
    }
    if (grepl("har_byte_index_bodyorcontent_-_har_byte_index_bodysize_Alexa_1000", label)) {
        return("CL - HAR (Alexa 1000)")
    }
    if (grepl("har_byte_index_bodyorcontent_-_har_byte_index_bodysize", label)) {
        return("Content-Length - HAR")
    }
    if (grepl("har_byte_index_bodysize_-_har_byte_index_bodyorcontent", label)) {
        return("HAR - Content-Length")
    }
    if (grepl("res_byte_index_-_har_byte_index_bodysize_Alexa_10001-11000", label)) {
        return("Res - HAR (Alexa 10001-11000)")
    }
    if (grepl("res_byte_index_-_har_byte_index_bodysize_Alexa_1000", label)) {
        return("Res - HAR (Alexa 1000)")
    }
    if (grepl("res_byte_index_-_har_byte_index_bodysize", label)) {
        return("Res - HAR")
    }
    if (grepl("res_byte_index_-_har_byte_index_bodyorcontent_Alexa_10001-11000", label)) {
        return("Res - CL (Alexa 10001-11000)")
    }
    if (grepl("res_byte_index_-_har_byte_index_bodyorcontent_Alexa_1000", label)) {
        return("Res - CL (Alexa 1000)")
    }
    if (grepl("res_byte_index_-_har_byte_index_bodyorcontent", label)) {
        return("Res - Content-Length")
    }
    if (grepl("res_object_index_-_har_object_index_Alexa_10001-11000", label)) {
        return("Res - HAR (Alexa 10001-11000)")
    }
    if (grepl("res_object_index_-_har_object_index_Alexa_1000", label)) {
        return("Res - HAR (Alexa 1000)")
    }
    if (grepl("res_object_index_-_har_object_index", label)) {
        return("Res - HAR")
    }
    if (grepl("byte_index_rel_diff_res_-_bodyorcontent", label)) {
        return("Res - Content-Length")
    }
    if (grepl("byte_index_rel_diff_res_-_har", label)) {
        return("Res - HAR")
    }
    if (grepl("byte_index_rel_diff_bodyorcontent_-_har", label)) {
        return("HAR - Content-Length")
    }
    if (grepl("byte_index_rel_diff_transfersize_-_bodyorcontent", label)) {
        return("HAR transfer size - Content-Length")
    }
    if (grepl("domContentLoadedEventStart", label)) {
        return("DOM Content Loaded")
    }
    if (grepl("loadEventStart", label)) {
        return("onLoad")
    }
    if (grepl("har_sum_of_bodyorcontent_-_har_sum_of_respbodysize", label)) {
        return("Content-Length - HAR")
    }
    if (grepl("har_sum_of_respbodysize_-_har_sum_of_bodyorcontent", label)) {
        return("HAR - Content-Length")
    }
    if (grepl("har_sum_of_respbodysize_-_res_sum_of_encoded", label)) {
        return("HAR - Res")
    }
    if (grepl("res_sum_of_encoded_-_har_sum_of_respbodysize", label)) {
        return("Res - HAR")
    }
    if (grepl("har_sum_of_contentsize_-_res_sum_of_decoded", label)) {
        return("HAR - Res decoded")
    }
    if (grepl("res_sum_of_decoded_-_har_sum_of_contentsize", label)) {
        return("Res - HAR decoded")
    }
    if (grepl("smart_total_page_size_-_har_sum_of_bodyorcontent", label)) {
        return("Page size - Content-Length")
    }
    if (grepl("har_sum_of_bodyorcontent_-_smart_total_page_size", label)) {
        return("Content-Length \n- Page size\n")
    }
    if (grepl("smart_total_page_size_-_har_sum_of_respbodysize", label)) {
        return("Page size - HAR")
    }
    if (grepl("har_sum_of_respbodysize_-_smart_total_page_size", label)) {
        return("HAR \n- Page size")
    }
    if (grepl("smart_total_page_size_-_res_sum_of_encoded", label)) {
        return("Page size - Res")
    }
    if (grepl("res_sum_of_encoded_-_smart_total_page_size", label)) {
        return("Res - Page size")
    }
	if (grepl("ADSL-Low-fast", label)) {
		return("RTT = 10 ms, bitrate = 2 Mbits/s down, 300 kbits/s up")
	}
	if (grepl("ADSL-Medium-fast", label)) {
		return("RTT = 10 ms, bitrate = 6 Mbits/s down, 700 kbits/s up")
	}
	if (grepl("ADSL-Mediumhigh-fast", label)) {
		return("RTT = 10 ms, bitrate = 12 Mbits/s down, 3 Mbits/s up")
	}
	if (grepl("ADSL-High-fast", label)) {
		return("RTT = 10 ms, bitrate = 25 Mbits/s down, 8 Mbits/s up")
	}

	if (grepl("3G-Medium-slow", label)) {
		return("RTT = 200 ms, bitrate = 1 Mbits/s down, 300 kbits/s up")
	}
	if (grepl("3G-high-slow", label)) {
		return("RTT = 100 ms, bitrate = 2 Mbits/s down, 700 kbits/s up")
	}
	if (grepl("4G-Medium-moderate", label)) {
		return("RTT = 50 ms, bitrate = 5 Mbits/s down, 1.5 Mbits/s up")
	}
	if (grepl("4G-High-fast", label)) {
		return("RTT = 20 ms, bitrate = 20 Mbits/s down, 5 Mbits/s up")
	}

	if (grepl("shaper-simu_hasym-20mbit-500kbit-50mbit-20mbit-5ms-5ms-100ms-100ms:if1", label)) {
		return("one interface (RTT = 10 ms, Bandwidth = 0.5 Mbits/s)")
	}
	if (grepl("shaper-simu_hasym-20mbit-500kbit-50mbit-20mbit-5ms-5ms-100ms-100ms:if2", label)) {
		return("one interface (RTT = 200 ms, Bandwidth = 50 Mbits/s)")
	}
	if (grepl("shaper-simu_asym-20mbit-2mbit-20mbit-20mbit-10ms-10ms-50ms-50ms:if1", label)) {
		return("one interface (RTT = 20 ms, Bandwidth = 2 Mbits/s)")
	}
	if (grepl("shaper-simu_asym-20mbit-2mbit-20mbit-20mbit-10ms-10ms-50ms-50ms:if2", label)) {
		return("one interface (RTT = 100 ms, Bandwidth = 20 Mbits/s)")
	}
	if (grepl("shaper-simu_sym-20mbit-6mbit-5mbit-20mbit-25ms-25ms-25ms-25ms:if1", label)) {
		return("one interface (RTT = 50 ms, Bandwidth = 6 Mbits/s)")
	}
	if (grepl("shaper-simu_sym-20mbit-6mbit-5mbit-20mbit-25ms-25ms-25ms-25ms:if2", label)) {
		return("one interface (RTT = 50 ms, Bandwidth = 5 Mbits/s)")
	}
	if (grepl("shaper-simu_hasym-20mbit-500kbit-50mbit-20mbit-5ms-5ms-100ms-100ms", label)) {
		return("EAF with highly asymmetric shaping (if1: RTT = 10 ms, bandwidth = 0.5 Mbits/s, if2: RTT = 200 ms, bandwidth = 50 Mbits/s)")
	}
	if (grepl("shaper-simu_sym-20mbit-6mbit-5mbit-20mbit-25ms-25ms-25ms-25ms", label)) {
		return("EAF with symmetric shaping (if1: RTT = 50 ms, bandwidth = 6 Mbits/s, if2: RTT = 50 ms, bandwidth = 5 Mbits/s)")
	}
	if (grepl("shaper-simu_asym-20mbit-2mbit-20mbit-20mbit-10ms-10ms-50ms-50ms", label)) {
		return("EAF with asymmetric shaping (if1: RTT = 20 ms, bandwidth = 2 Mbits/s, if2: RTT = 100 ms, bandwidth = 20 Mbits/s)")
	}
	if (grepl("selenium_alexa1000", label)) {
		return(gsub("run-", "", gsub("_noproxy_bypass-testbed-eth0", "", label)))
	}
    if (grepl("_-_", label)) {
        return(gsub("_", " ", label))
    }
    # dynamic human readable label
    print=F
	label = substitute_addresses_by_interfaces(label)
	if (print) {
		str(label)
	}
	s=strsplit(label, "[/_:+]")[[1]]
	if (grepl("from-anywhere-to-", label)) {
		if (print) {
			str(s)
		}
		return(paste(gsub("-", " ", s[3]), " (", s[7], ")", sep=""))
	} else {
		#return(make.plotlabel(s, exclude=exclude, print=print))
        return(gsub("_", " ", label))
	}
}

getshaperscenarios <- function(runstring, measurements) {
	#cat("We're dealing with a shaper in ",runstring,",", measurements, "measurements!\n")
	splitrunstring = strsplit(gsub("/","", runstring),"_")[[1]]
	l = length(splitrunstring)
	#cat(splitrunstring, "length", l, "\n")
	shaperscenarios = splitrunstring[(l - measurements+1):l]
	#str(shaperscenarios)
	if (length(shaperscenarios) < measurements) {
		missing = measurements - length(shaperscenarios)
		cat("WARNING: Number of measurements smaller than number of specified shaper scenarios - missing",missing,"\n")
		shaperscenarios = c(shaperscenarios, paste("Shaping", seq(1, missing), sep="-"))
		}
	return(shaperscenarios)
}

matchshaperscenarios <- function(path, runstring, interfaces) {
	measurementfiles = list.files(path, pattern="*pages.log")
	shaperscenarios = getshaperscenarios(runstring, length(measurementfiles))
	orderedshaperscenarios = c()
	j = 1
	for (i in (1:length(interfaces))) {
		iface = interfaces[i]
		if (any(grepl(iface, measurementfiles))) {
			orderedshaperscenarios[i] = shaperscenarios[j]
			j = j+1
		} else {
			orderedshaperscenarios[i] = "None"
		}
	}
	return(orderedshaperscenarios)
}

datematchregex = "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}-"

getdateofrun <- function(run) {
     datematch = regexpr(datematchregex, run)
     dateofrun = substring(run, datematch, datematch+attr(datematch, "match.length")-2)
	 return(dateofrun)
}

getrunscenario <- function(run) {
     datematch = regexpr(datematchregex, run)
     dateofrun = substring(run, datematch, datematch+attr(datematch, "match.length")-2)
     gsub(dateofrun, "", run)
     scenario = as.factor(substring(run, datematch+attr(datematch, "match.length")))
	 return(scenario)
}

fix.factors <- function(data) {
    if (class(data) == "factor") {
        return(fix.factor(data))
    } else if (class(data) == "data.frame") {
        for (i in seq(1, length(data))) {
            if (is.factor(data[[i]])) {
                data[[i]] = fix.factor(data[[i]])
            }
        }
    } else if (class(data) == "list") {
        for (i in seq(1, length(data))) {
            data[[i]] = fix.factors(data[[i]])
        }
    } else {
        str(data)
        cat("Warning: Data of class" + class(data) + "is neither dataframe nor list\n")
    }
    return(data)
}

fix.factor <- function(inputfactor) {
	return(factor(inputfactor, unique(inputfactor)))
}

compute_relative <- function(data, xdata, print=F) {
    if (print) {
        cat("Computing relative ", xdata, ": ", str(data[[xdata]]), "\n")
    }

	if (class(data) == "data.frame") {
        if (!is.na(data[[xdata]]) && !is.null(data[[xdata]])) {
            xdata0 = get_ymin(data,xdata)
            data[[paste(xdata, "rel", sep="_")]] = data[[xdata]] - xdata0
        } else {
            cat("Warning: Could not compute relative ", xdata, "\n")
        }
    } else if (class(data) == "list") {
        for (i in seq(1, length(data))) {
            if (!is.na(data[[i]][[xdata]]) && !is.null(data[[i]][[xdata]]) && nrow(data[[i]]) > 0) {
                xdata0 = get_ymin(data[[i]],xdata)
                data[[i]][[paste(xdata, "rel", sep="_")]] = data[[i]][[xdata]] - xdata0
            }
        }
    }
    return(data)
}

compute_rel_diff <- function(data, xdata) {

	if (class(data) == "data.frame") {
        data[[paste(xdata, "diff", sep="_")]] = c(0, diff(data[[xdata]]))
    } else if (class(data) == "list") {
        for (i in seq(1, length(data))) {
            data[[i]][[paste(xdata, "diff", sep="_")]] = c(0, diff(data[[i]][[xdata]]))
        }
    }
    return(data)
}

compute_rel_diff_metrics <- function(data, xdata1, xdata2) {

	if (class(data) == "data.frame") {
        data[[paste(xdata1, "diff", xdata2, sep="_")]] = data[[xdata1]] - data[[xdata2]]
    } else if (class(data) == "list") {
        for (i in seq(1, length(data))) {
            data[[paste(xdata1, "diff", xdata2, sep="_")]] = data[[xdata1]] - data[[xdata2]]
        }
    }
    return(data)
}

bitratetoint <- function(inputstring) {
	s = strsplit(inputstring, "bit")[[1]]

	bitratebase=1
	if (grepl("Mbit", s[1], ignore.case=T)) {
		bitratebase=1024 * 1024
		s[1] = gsub("Mbit","", s[1], ignore.case=T)
	}
	if (grepl("Kbit", s[1], ignore.case=T)) {
		bitratebase=1024
		s[1] = gsub("Kbit","", s[1], ignore.case=T)
	}

	if (grepl("K", s[1])) {
		bitratebase=1024
		s[1] = gsub("K","", s[1])
	}
	if (grepl("M", s[1])) {
		bitratebase=1024 * 1024
		s[1] = gsub("M","", s[1])
	}
	return(as.numeric(s[1]) * bitratebase)
}

bytestoint <- function(inputstring) {
	s = strsplit(inputstring, "b")[[1]]

	bytesbase=1
	if (grepl("Kbyte", s[1], ignore.case=T)) {
		bytesbase=1024
		s[1] = gsub("KByte","", s[1], ignore.case=T)
	}
	if (grepl("Mbyte", s[1], ignore.case=T)) {
		bytesbase=1024 * 1024
		s[1] = gsub("MByte","", s[1], ignore.case=T)
	}
	if (grepl("K", s[1])) {
		bytesbase=1024
		s[1] = gsub("K","", s[1])
	}
	if (grepl("M", s[1])) {
		bytesbase=1024 * 1024
		s[1] = gsub("M","", s[1])
	}
	return(as.numeric(s[1]) * bytesbase)
}

timetoint <- function(inputstring) {
	timebase=1
	if (grepl("ms", inputstring)) {
		inputstring = gsub("ms","", inputstring)
	} else if (grepl("s", inputstring)) {
        timebase=1000
		inputstring = gsub("s","", inputstring)
    }
	return(as.numeric(inputstring) * timebase)
}


check_metrics <- function(data, metrics, greater=FALSE, lower=FALSE, notequal=FALSE, allequal=FALSE, comparevalue=0, verbose=FALSE) {
    for (metric in metrics) {
        if (verbose) {
            cat("Checking", metrics, "\n")
        }
        if (class(data) == "list") {
            if (greater) {
                result = sapply(data, function(d) any(d[[metric]] > comparevalue))
            }
            if (lower) {
                result = sapply(data, function(d) any(d[[metric]] < comparevalue))
            }
            if (notequal) {
                result = sapply(data, function(d) any(d[[metric]] != comparevalue))
            }
            if (allequal) {
                cat("Checking if all", metric, "are equal\n")
                result = sapply(data, function(d) any (len(unique(d[[metric]])) > 1))
            }
        } else {
            if (greater) {
                result = any(data[[metric]] > comparevalue)
            }
            if (lower) {
                result = any(data[[metric]] < comparevalue)
            }
            if (notequal) {
                result = any(data[[metric]] != comparevalue)
            }
            if (allequal) {
                result = length(unique(data[[metric]])) > 1
            }
        }
        if (verbose) {
            cat("Result:", result, " (FALSE is good, it means that no value breaks the specified assumption)\n")
        }
        if (any(result)) {
            cat("\n!!! Non-conforming", metric, "for:\n")
            if (class(data) == "list") {
                print(names(result[result]))
            } else {
                cat(unique(levels(data$scenario)), "\n")
            }
            returnvalue = TRUE
        } else {
            returnvalue = FALSE
        }
    }
    if (verbose) {
        cat("\n")
    }
    return(returnvalue)
}


findoutlier <- function(data, metrics="actual_time", index=c(), listindex=c(), threshold=0.95, absdiff = 0.1, order="try", print=TRUE) {
    if (class(data) == "list") {
        data = do.call(rbind, data)
    }
    if (length(index) > 0) {
        if (!is.null(data[[index[1]]])) {
            indexfactor = data[[index[1]]]
        } else {
            indexfactor = factor()
            cat("WARNING: Tried to split based on", index[1], "but data is NULL\n")
        }
        if (length(index) > 1) {
            for (i in seq(2, length(index))) {
                if (!is.na(index[i])) {
                    indexfactor = data[[index[i]]]:indexfactor
                }
            }
        }
        if (length(levels(indexfactor)) > 0) {
            if (print) {
                cat("Splitting data based on", index)
                if (length(unique(levels(indexfactor))) < 20) {
                    cat(":", unique(levels(indexfactor)), "\n")
                } else {
                    cat("\n")
                }
                cat("\n")
            }
            data = split(data, indexfactor)
            outlierlist = list()
            for (j in seq(1, length(data))) {
                if (print) {
                    cat("For", names(data)[j], "...\n")
                }
                result = findoutlier(data[[j]], metrics=metrics, threshold=threshold, absdiff=absdiff, print=print, listindex=c(index, listindex), order=order)
                outlierlist[[j]] = result
            }
            outlierdata = fix.factors(do.call(rbind, outlierlist))
        }
    } else {
        if (all(is.null(data[[metrics]]))) {
            cat("Data is NULL -- cannot process\n")
            return(NULL)
        }
        if (all(is.na(data[[metrics]])) | all(is.infinite(data[[metrics]]))) {
            cat("Data is", str(data[[metrics]]), " -- cannot process\n")
            return(NULL)
        }
        if (print) {
            cat("Finding outliers in data", metrics, "(max =", max(data[[metrics]]), ",", threshold, "q = ", quantile(data[[metrics]], threshold), "), absdiff >= ", absdiff, "...\n")
        }
        if (threshold >= 0.5) {
            subsetdata = subset(data, (data[[metrics]] > (quantile(data[[metrics]], threshold) + absdiff)))
        } else {
            cat("Finding values smaller than", threshold, "quantile...\n")
            subsetdata = subset(data, (data[[metrics]] < (quantile(data[[metrics]], threshold) - absdiff)))
        }
        if (is.null(subsetdata) | length(subsetdata[[metrics]]) == 0) {
            if (print) {
                cat("\tNo outliers found\n\n")
            }
            return(NULL)
        }
        outlierdata = data.frame(metrics=subsetdata[[metrics]])
        outlierdata[[metrics]] = subsetdata[[metrics]]
        outlierdata$metrics = NULL
        outlierdata[[paste(threshold, "q", metrics, sep="_")]] = quantile(data[[metrics]], threshold)
        outlierdata$diff = outlierdata[[metrics]] - outlierdata[[paste(threshold, "q", metrics, sep="_")]]
        for (indexfactor in c(index, listindex)) {
            outlierdata[[indexfactor]] = subsetdata[[indexfactor]]
        }
        if (!is.null(subsetdata[[order]])) {
            outlierdata[[order]] = subsetdata[[order]]
        }
    }
    if (!is.null(outlierdata[[order]])) {
        outlierdata = outlierdata[order(as.factor(outlierdata[[order]])),]
    }
    if (print) {
        cat("    Found", length(outlierdata[[metrics]]), "outlier(s)\n")
        cat("\n")
    }
    return(fix.factors(outlierdata))
}

print_around_quantile <- function(data, metrics, printcolumns, q=0.5, n=10) {
    if (is.null(data[[metrics]]) | all(is.na(data[[metrics]]))) {
        cat("No data -- Nothing to print...\n")
        return
    }
    subsetdata = subset(data, !is.na(data[[metrics]]))
	data = subsetdata[with(subsetdata, order(subsetdata[[metrics]])),]
	q_value = quantile(data[[metrics]], q)
	q_index = which(abs(data[[metrics]] - q_value) == min(abs(data[[metrics]] - q_value)))
	if (length(q_index) > 1) {
		q_index = q_index[1]
	}
	cat("Quantile", q, "is (closest to)", q_value, "at index", q_index, "\n")
	printdata = data[max(round(q_index-n/2), 1):min(round(q_index+n/2), nrow(data)),]
	print(printdata[,c(metrics, printcolumns)])
}

top_n <- function(data, top, n=10, reverse=F) {
    data = fix.factors(data[with(data, order(data[[top]], decreasing=ifelse(reverse, F, T))),])
    return(head(data, n=min(nrow(data), n)))
}

print_top_n <- function(data, metrics, printcolumns, ifbiggerthan=0, n=10, reverse=F) {
    if (is.null(data[[metrics]]) | all(is.na(data[[metrics]]))) {
        cat("No data -- Nothing to print...\n")
        return
    }
    for (metric in metrics) {
        subsetdata = subset(data, !is.na(data[[metric]]))
        if (reverse) {
            if (any(subsetdata[[metric]] < ifbiggerthan)) {
                subsetdata = fix.factors(subset(subsetdata, subsetdata[[metric]] < ifbiggerthan))
            }
        } else if (any(subsetdata[[metric]] > ifbiggerthan)) {
			subsetdata = fix.factors(subset(subsetdata, subsetdata[[metric]] > ifbiggerthan))
        }
        if (!is.null(subsetdata)) {
            print(top_n(subsetdata, metric, reverse=reverse, n=n)[,c(metric, printcolumns)])
        } else {
            cat("Nothing to print!\n")
        }
    }
}

adjust_y_label <- function(ymax, ylabel) {
	if (ymax > 100000) {
		if (grepl("Bit", ylabel)) {
			ylabel=gsub("Bit","MBit",ylabel)
		} else {
			ylabel=paste(ylabel, "[M]")
		}
	} else if (ymax > 1000) {
		if (grepl("Bit", ylabel)) {
			ylabel=gsub("Bit","kBit",ylabel)
		} else {
			if (grepl("ms", ylabel)) {
				ylabel=gsub("ms","s",ylabel)
			} else {
				ylabel=paste(ylabel, " [k]")
			}
		}
    }
    return(ylabel)
}

draw_y_axis <- function(ymax, numlabels=5) {
	if (ymax > 100000) {
		yunit=10^6
	} else if (ymax > 1000) {
		yunit=10^3
	} else {
        yunit=1
    }

	labelmax = signif(ymax, 1)
	labels_at = seq(0, labelmax, length.out=numlabels)
	labels=labels_at/yunit

	axis(2, at=labels_at, labels=labels, las=1)
}

prepare_file <- function(plot = "terminal", filename="plot", cex=1, width=10, height=7, spacebelow=22, spaceabove=0.3, spaceleft=3, spaceright=0.1) {
	if (plot == "pdf") {
		cat("Plotting pdf to", paste(filename, ".pdf", sep=""), "\n")
		prepare_pdf(paste(filename, ".pdf", sep=""), width=width, height=height, cex=cex)
		par(mar=c(spacebelow,5,2,1))
		if (cex == 1) {
			cex=1.5
		}
	} else if (plot == "eps") {
		cat("Plotting eps to", paste(filename, ".eps", sep=""), ", with width =", width, ", height =", height, ", and cex =", cex, "\n")
		setEPS()
		postscript(paste(filename, ".eps", sep=""), height=height/2, width=width/1.5)
		par(mar=c(spacebelow,spaceleft,spaceabove,spaceright))
		cex=1.5
    } else if (plot=="png") {
        # If font size "1" is given, set to 2, otherwise too small in PNG
        if (cex == 1) {
            cex=2
        }
        # Set PNG size based on given font size
        prepare_png(filename=paste(filename, ".png", sep=""), cex=cex, width=cex*600, height=cex*1000)
	} else {
		cat("Plotting to terminal\n")
		par(mar=c(spacebelow,4,2,1))
	}
}

boxplotmetrics <- function(data, metrics="actual_time", filename="boxplot", splitby=c("policy", "workload"), filterscenarios=c(), cex=1, spacebelow=22, mainlabel="", plot="terminal", ylabel="", yaxisincludezero=FALSE, can_omit_plotlabels=FALSE, log="", exclude="") {
    prepare_file(plot=plot, filename=filename, cex=cex, spacebelow=spacebelow)
    if (length(filterscenarios) > 0) {
        data = filter_scenarios(data, filterscenarios, split=splitby)
    }

	if (class(data) == "list") {
        data = do.call(rbind, data)
    }
    if (all(is.na(data[[metrics]]))) {
        cat("All data is NA -- cannot plot.\n")
        return()
    }
    splitfactors = data[[splitby[1]]]
    factor1length = length(levels(splitfactors))
    additionalsplitfactor = 1

    if (length(splitby) > 1) {
        for (i in seq(2, length(splitby))) {
            splitfactors = data[[splitby[i]]]:splitfactors
            additionalsplitfactor = additionalsplitfactor * length(levels(data[[splitby[i]]]))
        }
    }

	plotlabels=sapply(levels(splitfactors), function(x) plotlabel.human.readable(x, exclude=exclude))
	cat("Generated", length(plotlabels), "plot labels of maximum length", max(nchar(plotlabels)), ", spacebelow =", spacebelow, "\n")

	# If plot labels are too long and also multiline plotlabels will not fit
	if (max(nchar(plotlabels)) > 2*spacebelow && length(plotlabels) > 50 && can_omit_plotlabels) {
		factorlevels = sapply(splitby, function(x) length(levels(data[[x]])))
		excludefactor = splitby[which.min(factorlevels)]
		if (excludefactor == splitby[1]) {
			# Do not exclude first factor, it's probably too important
			excludefactor = splitby[2]
		}
		cat("These plot labels are too long. Trying to omit factor", excludefactor, "in the labels\n")
		plotlabels=sapply(levels(splitfactors), function(x) plotlabel.human.readable(x, exclude=excludefactor))
	}

	n = (factor1length + 1)*additionalsplitfactor
	xat = rep( c( rep(1, factor1length), 0), additionalsplitfactor) * 1:n
	xat = xat[xat>0]

	yaxt="s"

    ymax = max(data[[metrics]], na.rm=TRUE)
	ylim = c(min(data[[metrics]], na.rm=TRUE), ymax)
    if (yaxisincludezero && ylim[1] > 0) {
        ylim[1]=0
    }

    if (ylabel == "") {
        ylabel=paste(gsub("_", " ", metrics))
    }
	if (ymax > 10000) {
		yaxt="n"
        ylabel = adjust_y_label(ymax, ylabel)
        if (ymax > MAX_VALUE) {
            cat("Warning: ymax is unrealistically high:", ymax, "\nSetting max value to", MAX_VALUE, "\n")
            data[[metrics]][data[[metrics]] > MAX_VALUE] = 0
            ymax = max(data[[metrics]], na.rm=TRUE)
            ylim[2] = ymax
        }
	}

	boxplot(data[[metrics]] ~ splitfactors, at=xat, ylab=ylabel, yaxt=yaxt, ylim=ylim, xlab="", names=plotlabels, main=mainlabel, cex.lab=cex, cex.main=cex, cex.axis=cex, outcex=cex, las=2, log=log)

    if (yaxt == "n") {
        draw_y_axis(ymax)
    }

	if (plot == "pdf" | plot == "eps") {
		dev.off()
	}
}

summarizedata <- function(data, metrics, index="", onlynonzero=F, filterscenarios=c()) {
    if (length(filterscenarios) > 0) {
        data = filter_scenarios(data, filterscenarios)
    }
    for (metric in metrics) {
        cat("Summary of", metric)
        if (index != "") {
            cat(" indexed by", index, ":\n")
        } else {
            cat("\n")
        }

        if (class(data) == "data.frame") {
            if (!is.null(data[[index]])) {
                data = split(data, index)
            } else {
                data = list(data)
            }
        }
        if (class(data) == "list") {

            for (i in seq(1, length(data))) {
                dataitem = data[[i]]
                if (onlynonzero) {
                    if (all(dataitem[[metric]] == 0)) {
                        next
                    }
                }
                if (length(dataitem[[metric]]) == length(dataitem[[index]])) {
                    cat(metric, "for", plotlabel.human.readable(names(data)[i]), "\n")
                    summarylist = tapply(X=dataitem[[metric]], IND=fix.factors(dataitem[[index]]), FUN=summary);
                    print(summarylist[!sapply(summarylist, is.null)])
                } else {
                    if (index == "") {
                        print(summary(dataitem[[metric]]))
                    } else {
                        cat("Error: Metric", metric, "of length", length(dataitem[[metric]]), "but", index, "of length", length(dataitem[[index]]), "\n")
                    }
                }
            }
        }
        cat("\n")
    }
}

filter_scenarios <- function(data, filterscenarios=c(), direction="down", splitby=c("policy")) {
    splitlist=FALSE
    if (class(data) == "data.frame") {
        splitfactors = data[[splitby[1]]]
        if (length(splitby) > 1) {
            for (i in seq(2, length(splitby))) {
                splitfactors = data[[splitby[i]]]:splitfactors
            }
        }
        data = split(data, splitfactors)
        splitlist=TRUE
    }
    if (length(filterscenarios) > 0) {
        prevdatalength = length(data)
        datatoplot = list()
        for (filter in filterscenarios) {
            datatoplot = c(datatoplot, data[grepl(filter, names(data))])
        }
        if (length(datatoplot) < 1) {
            cat("Data set is empty now -- perhaps mismatching filter?\nFilter:", filterscenarios, "\nData names:", paste(names(data), collapse="\n\t"), "\n\n--> Returning original data, unfiltered\n\n")
            return(data)
        }
        # Only include data list items with more than 0 rows
        data = datatoplot[sapply(datatoplot, function(x) nrow(x) > 0)]
    }
    uniquenames = unique(names(data))
    data = fix.factors(unique(data))
    if (length(uniquenames) == length(data)) {
        names(data) = uniquenames
    } else {
        cat("Mismatch between length of uniquenames", length(uniquenames), "and length of data", length(data), "\n")
    }
    cat("Filtered data from length", prevdatalength, "to length", length(data), "based on", filterscenarios, "\n")
    if (splitlist) {
        data = do.call(rbind, data)
    }
    return(data)
}

plotovertime <- function(data, metrics, xdata="timestamp", cols=c("black", "darkgoldenrod"), filterscenarios=c(), cex=1, log="", ylabel="", plotlabel="", plot="terminal", filename="plotovertime", plottype="l", force_abs_x=FALSE, yaxis2="", verbose=F, rev_metrics_labels=F, ymax=NULL, data_labels=names(data), force_plot_data_labels=F) {
	if (plot == "pdf") {
		cat("Plotting pdf\n")
		if (!grepl(".pdf", filename)) {
			filename = paste(filename, ".pdf", sep="")
		}
		prepare_pdf(filename, width=10, height=7, cex=cex)
		par(mar=c(4,5,2,4))
	} else if (plot == "eps") {
		cat("Plotting eps with cex", cex, "\n")
		setEPS()
		if (!grepl(".eps", filename)) {
			filename = paste(filename, ".eps", sep="")
		}
		postscript(filename, height=5, width=7)
		#postscript(filename, height=10, width=10, cex=cex)
		par(mar=c(4,5,0.1,0.1))
		cex=1.5
	} else {
		cat("Plotting to terminal\n")
		par(mar=c(4,4,2,1))
	}

    if (length(filterscenarios) > 0) {
        data = filter_scenarios(data, filterscenarios)
    }

    comparetimeseries(data=data, metrics=metrics, xdata=xdata, cols=cols, log=log, ylabel=ylabel, plotlabel=plotlabel, cex=cex, plottype=plottype, force_abs_x=force_abs_x, yaxis2=yaxis2, verbose=verbose, plot_mainlabel=ifelse(plot == "eps", F, T), rev_metrics_labels=rev_metrics_labels, ymax=ymax, data_labels=data_labels, force_plot_data_labels=force_plot_data_labels)
	if (plot == "pdf" | plot == "eps") {
		dev.off()
	}

}

plotn <- function(n=3, filenameprefix="plot", plot="terminal", cex=1) {
	cat("Output to",filenameprefix,plot,"\n")
    if (plot=="png") {
        # If font size "1" is given, set to 2, otherwise too small in PNG
        if (cex == 1) {
            cex=2
        }
        # Set PNG size based on given font size
        prepare_png(filename=paste(filenameprefix, ".png", sep=""), cex=cex, width=cex*600, height=cex*1000)
    } else if (plot=="pdf") {
        # Set PDF size based on given font size
        prepare_pdf(filename=paste(filenameprefix, ".pdf", sep=""), cex=cex, width=cex*10, height=cex*16)
    }

    # Configure the plot to have n rows
    par(mfrow=c(n,1))
    # Set the font size in the plots again, since it was overwritten by setting mfrow
    par(cex=cex, cex.axis=cex, cex.lab=cex, cex.main=cex, cex.sub=cex)
    cat("Canvas has been prepared. Now issue", n, "plotting commands and then call dev.off()\n")
}

plotthree <- function(data, data2=data, data3=data, metrics1, metrics2=metrics1, metrics3=metrics1, filter1=c(), filter2=filter1, filter3=filter1, cols1=c("black", "darkgoldenrod"), cols2=cols1, cols3=cols1, ylabel1="", ylabel2="", ylabel3="", plotlabel1="", plotlabel2="", plotlabel3="", plot="terminal", cex=1) {
	filenameprefix=paste("stats", paste(metrics1, collapse="_"), sep="-")
    plotn(3, filenameprefix, plot, cex)
    cat("Plotting over time\n")
    plotovertime(data, metrics=metrics1, filterscenarios=filter1, cols=cols1, ylabel=ylabel1, plotlabel=plotlabel1)
    plotovertime(data2, metrics=metrics2, filterscenarios=filter2, cols=cols2, ylabel=ylabel2, plotlabel=plotlabel2)
    plotovertime(data3, metrics=metrics3, filterscenarios=filter3, cols=cols3, ylabel=ylabel3, plotlabel=plotlabel3)

	if (plot=="png" || plot=="pdf") {
    # Close the file
	invisible(dev.off())
	}
}

plot.cdf <- function(data, subsetdata=F, xlim=c(min(data), max(data)), xlabel="data", color="black", pch=15, cex=1, log="", ccdf=F, ylim=c(0,1), mainlabel="", xaxt="s") {
	data.ecdf=ecdf(data)
	data.knots=knots(data.ecdf)
	data.sel=round(seq(1, length(data.knots), length=10))
	data.sel2=round(seq(1, length(data.knots), length=10000))
	if (subsetdata) {
		data.plot=data.knots[data.sel2]
	} else {
		data.plot = data.knots
	}

	if (grepl("y",log) && ylim[1] == 0) {
		ylim[1] = min(data.ecdf(data.plot))
	}
	if (grepl("x", log) && xlim[1] <= 0) {
		xlim[1] = min(data.plot[data.plot > 0])
	}

	if (!ccdf) {
		plot(data.plot, data.ecdf(data.plot), log=log, xlim=xlim, type="l", col=color, ylim=ylim, main=NA, ann=FALSE, cex.axis=cex, xaxt=xaxt)
	} else {
		plot(sort(data.plot), 1-data.ecdf(sort(data.plot)), log=log, xlim=xlim, type="l", col=color, ylim=ylim, main=NA, ann=FALSE, cex.axis=cex, xaxt=xaxt)
	}
	if (xaxt == "n") {
        cat("xlim: ", xlim, "\n")
		# Plot x axis
		x_at = 10^seq(1, log(xlim[2]), 10)
        if (xlim[2] > 20000000) {
            x_at = c(10, 100, 500, 1000, 1000, 5000, 10000, 50000, 100000, 250000, 500000, 1000000, 2000000, 5000000, 10000000, 20000000)
		} else if (xlim[2] > 1000000) {
            x_at = c(10, 100, 500, 1000, 5000, 10000, 25000, 50000, 100000, 500000, 1000000, 2000000)
		} else if (xlim[2] > 100000) {
            x_at = c(10, 100, 500, 1000, 2000, 5000, 10000, 50000, 100000, 250000, 500000)
        } else if (xlim[2] > 10000) {
            x_at = c(10, 100, 500, 1000, 2000, 5000, 10000, 20000, 50000)
        } else if (xlim[2] > 1000) {
            x_at = c(10, 100, 500, 1000, 2000, 5000)
        } else if (xlim[2] > 100) {
            x_at = c(1, 10, 50, 100, 200, 500)
        } else if (xlim[2] > 10) {
            x_at = c(0.5, 1, 2, 5, 10, 20, 50)
        } else if (xlim[2] < 10 && xlim[2] > 1) {
            x_at = c(0.5, 1, 2, 5)
        }
        #cat("x axis: ", x_at, "\n")
        axis(1, at = x_at)
	}

	abline(h = c(0, 1), col = "gray45", lty = 2)
	#axis(1, at=c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2), labels=c("0.5", "0.75", "1", "1.25", "1.5", "1.75", "2"))

	line=2
	if (cex > 1) {
		line = cex+1
	}
	mtext(side=1, text=xlabel, line=line, cex=cex)
	mtext(side=2, text=ifelse(ccdf,"CCDF","ECDF"), line=line, cex=cex)
	if (mainlabel != "") {
		mtext(side=3, text=mainlabel, line=1, cex=cex)
	}
	if (!ccdf) {
		points(data.knots[data.sel], data.ecdf(data.knots[data.sel]), col=color, pch=pch)
	} else {
		points(data.knots[data.sel], 1-data.ecdf(data.knots[data.sel]), col=color, pch=pch)
	}
}

add.cdf <- function(data, subsetdata=F, color="gray45", pch=14, ccdf=F) {
    data = data[!is.na(data)]
    if (length(data) == 0) {
        cat("Length of data is 0 -- skipping\n")
        return()
    }
	data.ecdf=ecdf(data)
	data.knots = knots(data.ecdf)
	data.sel = round(seq(1, length(data.knots), length=10))

	if (subsetdata) {
		data.sel2 = round(seq(1, length(data.knots), length=10000))
		data.plot = data.knots[data.sel2]
	} else {
		data.plot = data.knots
	}
	#data.sel = round(exp(seq(log(min(data.knots)), log(max(data.knots)), length=10)))
	if (!ccdf) {
		lines(data.plot, data.ecdf(data.plot), col=color)
		points(data.knots[data.sel], data.ecdf(data.knots[data.sel]), pch=pch, col=color)
	} else {
		lines(sort(data.plot), 1-data.ecdf(sort(data.plot)), col=color)
		points(data.knots[data.sel], 1-data.ecdf(data.knots[data.sel]), pch=pch, col=color)
	}
}

reorder.cdf.list <- function(data, order=c("eth0", "wlan0", "mptcp", "eaf_max")) {
    newdata = list()
    newnames = c()
    if(all(names(data) == order)) {
        # Already ordered
        return(data)
    } else {
        for (item in order) {
            if (!is.null(data[[item]])) {
                newdata = c(newdata, list(data[[item]]))
                newnames = c(newnames, item)
            }
        }
        if (length(newdata) == length(data)) {
            names(newdata) = newnames
            #cat("Ordered: ", names(newdata), "\n")
            return(newdata)
        } else {
            cat("Could not order ", names(data), " to ", order, "\n")
            return(data)
        }
    }
}

plot.cdf.list <- function(data, metrics="d", xlim=NULL, cex=1, log="", ccdf=F, xlabel="data", mainlabel="", subsetdata=F, legendposition="topleft", order=c(), colors=c("red", "darkorchid3", "cadetblue", "blue", "darkolivegreen4", "gray45", "black", "darkgoldenrod", "brown", "blueviolet", "darkorange2", "aquamarine4", "forestgreen", "deeppink"), pointtypes=c(4, 8, 18, 17, 16, 15, 14, 9, 12, 0, 1, 2, 3, 19)) {
    if (is.null(xlim)) {
		if (!grepl("x", log)) {
			xlim = c(get_xmin(data, metrics), get_xmax(data, metrics))
		} else {
			xlim = c(get_xmin(data, metrics, log=T), get_xmax(data, metrics))
		}
    }
	xaxt="s"
	if (grepl("x", log)) {
		xaxt = "n"
	}
    if (length(order) > 0) {
        data = reorder.cdf.list(data, order=order)
    }

    firstdata = data[[1]][[metrics]]
    firstdata = firstdata[!is.na(firstdata)]
    if (length(firstdata) == 0) {
        cat("First data of length 0 -- aborting\n")
        return()
    }

    plot.cdf(data[[1]][[metrics]], log=log, ccdf=ccdf, xlim=xlim, color=colors[1], pch=pointtypes[1], xlabel=plotlabel.human.readable(xlabel, additionaldata=paste("(", data[[1]]$shaped_rtt, "ms, ", data[[1]]$shaped_bw, "Mbit)", sep="")), mainlabel=mainlabel, xaxt=xaxt, subsetdata=subsetdata, cex=cex)
    if (length(data) > 1) {
        for (i in seq(2, length(data))) {
            add.cdf(data[[i]][[metrics]], color=colors[i], pch=pointtypes[i], ccdf=ccdf, subsetdata=subsetdata)
        }
    }
    #cat("xlabel:", xlabel, "\n")
	plotlabels=sapply(names(data), function(x) plotlabel.human.readable(x))
    #cat("plot labels:", plotlabels, "\n")

	legend(legendposition, legend=plotlabels, col=colors, pch=pointtypes, bty="n", cex=cex)
}

# Takes a list (e.g. of data frames), returns a matrix of its quantiles
compute.quantiles <- function(data, quantiles=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
{
	q = sapply(data, function(x) quantile(x, probs=quantiles))
	q.matr = matrix(q, nrow=length(quantiles))
	rownames(q.matr)=quantiles
	colnames(q.matr)=names(data)
	return(q.matr)
}

plot_initial <- function(data, x_metrics="page", y_metrics="actual_time", log="", width=20, height=14, ymax=0, xmax=0, ymin=0, xmin=0, x_label="", y_label="", mainlabel="", plottype="l", plot="pdf", filename="timings.pdf", cex=1, spacebelow=3, pch=4, color="blue") {
    prepare_file(plot=plot, filename=filename, cex=cex, width=width, height=height, spacebelow=spacebelow)
    cat("Started plotting to", filename, "-- needs to be closed again\n")

    data = subset(data, (!is.na(data[[y_metrics]])))

    if (x_label == "") {
        x_label = gsub("_", " ", x_metrics)
    }
    if (y_label == "") {
        y_label = gsub("_", " ", y_metrics)
    }
    if (ymax == 0) {
        ymax = max(data[[y_metrics]])
    }
    if (xmax == 0) {
        xmax = max(data[[x_metrics]])
    }

    if (ymin == 0) {
        if (grepl("y", log)) {
            ymin = min(data[[y_metrics]])
        } else {
            ymin = 0
        }
    }
    if (xmin == 0) {
        if (grepl("x", log)) {
            xmin = min(data[[x_metrics]])
        } else {
            xmin = 0
        }
    }


    cat("Length of x data:", length(data[[x_metrics]]), "length of y:", length(data[[y_metrics]]), "\n")
    cat("ymax:", ymax, "log:", log, "\n")
    plot(as.numeric(data[[x_metrics]]), data[[y_metrics]], type=plottype, pch=pch, log=log, ylim=c(ymin, ymax), xlim=c(xmin, xmax), col=color, main=mainlabel, ylab=y_label, xlab=x_label, cex.axis=cex, cex.lab=cex)
}

plot_add <- function(data, x_metrics="page", y_metrics="actual_time", plottype="p", color="red", pch=1) {
    if (plottype == "p") {
        points(data[[x_metrics]], data[[y_metrics]], col=color, pch=pch)
    }
    if (plottype == "l") {
        lines(data[[x_metrics]], data[[y_metrics]], col=color, pch=pch)
    }
}

plot_density <- function(data, metrics="timediff", subsets=c(), plottype="l", colors=c("black", "red", "blue", "green"), pch=c(1, 2, 3, 4), mainlabel="Probability density", ylabel="Probability density", plot="pdf", filename="density", cex=1, spacebelow=6) {
    prepare_file(plot=plot, filename=filename, cex=cex, spacebelow=spacebelow)
    xmin = get_ymin(data, metrics)
    xmax = get_ymax(data, metrics)
    cat("Metrics:", paste(metrics), ", xlim=", xmin, xmax, "\n")
    if (length(subsets) > 0) {
        cat("Subset for ", subsets[1], "\n")
        firstdata = subset(data, comparison==subsets[1])
    } else {
        firstdata = data
    }

    str(firstdata)
    plot(density(firstdata[[metrics]]), col=colors[1], lty=pch[1], ylim=c(0, 0.0003), xlim=c(xmin, xmax), main=mainlabel, ylab=ylabel)

    if (length(subsets) > 1) {
        for (i in seq(2, length(subsets))) {
            s = subsets[i]
            datatoplot = subset(data, comparison==s)

            str(datatoplot)
            lines(density(datatoplot[[metrics]]), col=colors[i], lty=pch[i])
        }
    }
    legend("topright", legend=subsets, col=colors, pch=pch, cex=cex)

	if (plot == "pdf" | plot == "eps") {
		dev.off()
	}
}

# In a data set, compare a set of metrics with all other metrics in the set (by calculating their numeric difference)
# and put the result in the data set.
# Also return a list of (clumsy) labels for these differences
compute_compare_metrics <- function(data, orig_metrics, compare_metrics=orig_metrics[1:length(orig_metrics)], print=F, abs=F, exclude_lower_zero=F) {
    all_comparisons = c()
    if (print) {
		cat("\nOrig_metrics:", paste(orig_metrics), ", length:", length(orig_metrics), "\n")
    }
	baseline_metrics = orig_metrics
	if (abs) {
		baseline_metrics = baseline_metrics[1:(length(baseline_metrics)-1)]
	}
	for (i in seq(1, length(baseline_metrics))) {
		baseline = baseline_metrics[i]
		if (print) {
			cat("\ti =", i, "Baseline at index i: ", baseline, "\n")
		}
		if (abs) {
			compare_metrics = orig_metrics[(i+1):(length(orig_metrics))]
		} else {
			compare_metrics = orig_metrics
		}
		if (print) {
			cat("Compare_metrics from orig[i+1] to orig[len]:", paste(compare_metrics), "\n")
		}
		for (metric in compare_metrics) {
			if (print) {
				cat("Metric: ", metric, "\n")
			}
			if (is.na(metric)) {
				if (print) {
					cat("NA -- next\n")
				}
				next
			}
			if (metric == baseline) {
				if (print) {
					cat("Equal -- next!\n")
				}
				next
			}
			this_minus_that = paste(metric, "-", baseline, sep="_")
			all_comparisons = c(all_comparisons, this_minus_that)
			data[[this_minus_that]] = data[[metric]] - data[[baseline]]
			if (abs) {
				data[[this_minus_that]] = abs(data[[this_minus_that]])
			}
			if (exclude_lower_zero) {
				data[[this_minus_that]] = ifelse(data[[baseline]] < 0 | data[[metric]] < 0, NA, data[[this_minus_that]])
			}
		}
		if (print) {
			cat("\n")
		}
	}
	if (print) {
		cat("All comparisons done: ", paste(all_comparisons), "\n")
	}
	return(list(data = data, all_comparisons = all_comparisons))
}

# From dataset, compute a new data frame of differences that is easier to use for plotting
# The new data frame has the following columns:
# comparison: 	which two metrics are compared (may be the clumsy label computed by compute_compare_metrics)
# datadiff:   	the numeric difference of the two metrics in this particular case
# page:			which page was loaded in this case
# run:			which run this is
compute_comparison_dataframe <- function(data, all_comparisons, splitby=NA) {
	comparisondata = list()
	if (!is.na(splitby)) {
		splitdata = split(data, data[[splitby]])
	}
	for (comparison in all_comparisons) {
		if (!is.na(splitby)) {
			for (i in seq(1, length(splitdata))) {
				dataitem = splitdata[[i]]
				splitname = gsub(" ", "_", names(splitdata)[i])
				comparisonlabel = paste(comparison, splitname, sep="_")

				newdata = data.frame(comparison=comparisonlabel, datadiff=dataitem[[comparison]], page=dataitem$page, run=dataitem$run)
				comparisondata = c(comparisondata, list(newdata))
			}
		} else {
			newdata = data.frame(comparison=as.character(comparison), datadiff=data[[comparison]], page=data$page, run=data$run)
			comparisondata = c(comparisondata, list(newdata))
		}
	}
	return(do.call(rbind, comparisondata))
}

# Within a dataframe, find rows where "havetomatch" columns all match,
# and compute the differences of all columns where values differ,
# while keeping all columns that are the same
compare_loads_dataframe <- function(data, havetomatch = c("run", "page", "try"), compare="policy", baseline=c("eth0", "wlan0", "eth0_or_wlan0"), baselinemetrics="PLT_without_redirects", verbose=F) {
	comparisondatalist = list()

    # Split data by all the factor values that "have to match"
    # (in order to make the rest comparable)
    splitfactor = data[[havetomatch[1]]]
    if (verbose) {
        cat("Splitfactor:", havetomatch[1], "-", length(levels(data[[havetomatch[1]]])), "levels\n")
    }
    if (length(havetomatch) > 1) {
        for (factor in havetomatch[2:length(havetomatch)]) {
            if (verbose) {
                cat("Appending", factor, "-", length(levels(data[[factor]])), "levels\n")
                if (length(levels(data[[factor]])) > 1) {
                    cat("\t", data[[factor]], "\n")
                }
            }
            splitfactor = splitfactor:data[[factor]]
        }
    }
    splitdata = split(data, splitfactor)
    if (verbose) {
        cat("Split data of length", nrow(data), "by", havetomatch, "-", length(levels(splitfactor)), "levels!\n")
        cat("To compare:", compare, "-", as.character(levels(data[[compare]])), "\n")
        #str(fix.factors(splitdata))
    }
    for (dataitem in splitdata) {
        # If we do not have a data point for every level of the factor
        # that we want to compare, move on
        if (nrow(dataitem) < length(levels(data[[compare]]))) {
            if (verbose) {
                cat("Length of dataitem", nrow(dataitem), "<", length(levels(data[[compare]])), "-- skipping\n")
            }
            next
        }
        if (nrow(dataitem) > length(levels(data[[compare]]))) {
            if (verbose) {
                str(fix.factors(dataitem))
                cat("Length of dataitem", nrow(dataitem), ">", length(levels(data[[compare]])), "-- something went very wrong!\n")
                str(dataitem)
            }
            next
        }
        for (base in baseline) {
            if (verbose) {
                cat("Baseline", base, "\n")
            }
            if (base %in% levels(dataitem[[compare]])) {
                bdata = subset(dataitem, dataitem[[compare]] == base)
                comparelines = levels(dataitem[[compare]])[levels(dataitem[[compare]]) != base]
                if (nrow(bdata) != 1) {
                    cat("Something went wrong with getting the baseline", base, "from", as.character(dataitem[[compare]]), "\n")
                    next
                }
            } else {
                # have to generate baseline dynamically - lowest of the baselines
                # candidates, which we get as string and split by "or"
                baselines_to_compare = strsplit(base, "_or_")[[1]]
                baseline_candidates = subset(dataitem, dataitem[[compare]] %in% baselines_to_compare)
                if (nrow(baseline_candidates) != length(baselines_to_compare)) {
                    cat("Mismatch: Found", nrow(baseline_candidates), "baseline candidates when looking for", length(baselines_to_compare), "\n")
                    next
                }
                if (verbose) {
                    cat("Baseline candidates", baselinemetrics, baseline_candidates[[baselinemetrics]], "\n")
                    cat("min", baselinemetrics, min(baseline_candidates[[baselinemetrics]]), "at index", which(baseline_candidates[[baselinemetrics]] == min(baseline_candidates[[baselinemetrics]])), "\n")
                }
                # Take the candidate with the lowest "baselinemetrics" value
                bdata = baseline_candidates[which(baseline_candidates[[baselinemetrics]] == min(baseline_candidates[[baselinemetrics]])),]
                if (nrow(bdata) > 1) {
                    # More than one candidate with the lowest "baselinemetrics" -- just take first one
                    bdata = bdata[1,]
                }

                # All data we still want to compare is everything except
                # the baseline candidates
                comparelines = levels(dataitem[[compare]])[!levels(dataitem[[compare]]) %in% baselines_to_compare]
            }
            #if (verbose) {
            #    str(bdata)
            #}
            for (comp in comparelines) {
                newdataitem = data.frame(baseline = base)
                for (matchfactor in havetomatch) {
                    newdataitem[[matchfactor]] = dataitem[[matchfactor]][1]
                }
                newdataitem$comparefactor = as.factor(comp)
                compitem = subset(dataitem, dataitem[[compare]] == comp)
                for (var in names(dataitem)) {
                    #if (verbose) {
                    #    cat(var, "- base:", bdata[[var]], "comp:", compitem[[var]], "\n")
                    #}
                    if (is.factor(bdata[[var]])) {
                        # New factor: Storing baseline value and comparison value
                        newdataitem[[var]] = bdata[[var]]
                        newdataitem[[paste(var, "comp", sep="_")]] = compitem[[var]]
                        next
                    }
                    # New data: Baseline value, _absdiff, _reldiff of compared
                    newdataitem[[var]] = bdata[[var]]
                    if (is.na(bdata[[var]]) | is.na(compitem[[var]])) {
                        newdataitem[[var]] = NA
                    }
                    newdataitem[[paste(var, "absdiff", sep="_")]] = compitem[[var]] - bdata[[var]]
                    if (is.na(bdata[[var]]) | is.na(compitem[[var]]) | bdata[[var]] != 0) {
                        newdataitem[[paste(var, "reldiff", sep="_")]] = compitem[[var]] / bdata[[var]]
                    }
                    else {
                        newdataitem[[paste(var, "reldiff", sep="_")]] = ifelse (compitem[[var]] == 0, 1, Inf)
                    }
                }
                if (verbose){
                    cat("\tAdded new data comparing", compare, comp, "to", base, "of length", length(newdataitem), "nrow", nrow(newdataitem), "\n")
                }
                comparisondatalist = c(comparisondatalist, list(newdataitem))
            }
        }

    }
	comparisondata = do.call(rbind, comparisondatalist)
    return(comparisondata)
}

get_percentage <- function(data, metrics, min=NA, max = NA, eq=NA, print=F) {
	data = data[!is.na(data[[metrics]]),]
	datalength = nrow(data)
	if (!is.na(min)) {
		data = subset(data, data[[metrics]] > min)
	}
	if (!is.na(max)) {
		data = subset(data, data[[metrics]] < max)
	}
	if (!is.na(eq)) {
		data = subset(data, data[[metrics]] == eq)
	}
	if (print) {
		cat("Got", nrow(data), "/", datalength, "items =", nrow(data) / datalength * 100, "%\n")
	}
	return(nrow(data) / datalength * 100)
}

diff_to_string <- function(number, metrics) {
    speedupstring = ""
    if (grepl("absdiff", metrics)) {
        if (number < 0) {
            comparestring = "faster"
            bystring = paste("by", round(0 - number, digits=2))
        } else if (number == 1) {
            comparestring = "equal"
            bystring = ""
        } else {
            comparestring = "slower"
            bystring = paste("by", round(number, digits=2))
        }
        return(paste(round(number, digits=2), "\t--", comparestring, bystring))
    } else if (grepl("reldiff", metrics)) {
        speedup = 1 / number
        speedupstring = paste("(speedup: ", round(speedup, digits=2), ")", sep="")
        if (number < 1) {
            comparestring = "faster"
            bystring = paste("by", round((1 - number) * 100, digits=2), "%")
        } else if (number == 1) {
            comparestring = "equal"
            bystring = ""
        } else {
            comparestring = "slower"
            bystring = paste("by", round((number - 1) * 100, digits=2), "%")
        }
        return(paste(round(number, digits=2), "\t--", comparestring, bystring, speedupstring))
    } else {
        return(as.character(round(number, digits=2)))
    }
}

print_comparison_summary <- function(data, metrics, middle=0) {
	if (all(is.na(data[[metrics]]))) {
		cat("All NA!\n")
		return()
	}
	lowerthan = get_percentage(data, metrics, max = middle)
	equalto = get_percentage(data, metrics, eq = middle)
	above = get_percentage(data, metrics, min = middle)
	cat(metrics, "\n\n")
	#cat("is not NA:\t", round(nrow(data[!is.na(data[[metrics]]),]) / nrow(data) * 100, digits=2), "% of samples\n")
	#cat("\tmatches:", round(equalto, digits=2), "% of cases (in which it is not NA)\n")
	if (any(data[[metrics]] != 0)) {
		cat("\tmin:\t", diff_to_string(min(data[[metrics]], na.rm=T), metrics), "\n")
        cat("\tmedian:\t", diff_to_string(median(data[[metrics]], na.rm=T), metrics), "\n")
		cat("\tmax:\t", diff_to_string(max(data[[metrics]], na.rm=T), metrics), "\n")

		#cat("\tquantiles:\n")
		#if (lowerthan > 10) {
		#	cat("\t\t10th:\t", diff_to_string(round(quantile(data[[metrics]], 0.1, na.rm=T), digits=2), metrics), "\n")
		#}
		#if (above > 10) {
		#	cat("\t\t90th:\t", diff_to_string(round(quantile(data[[metrics]], 0.9, na.rm=T), digits=2), metrics), "\n")
		#}
		#if (above > 1) {
		#	cat("\t\t99th:\t", diff_to_string(round(quantile(data[[metrics]], 0.99, na.rm=T), digits=2), metrics), "\n")
		#}
		#if (above > 0.1) {
		#	cat("\t\t99.9th:\t", diff_to_string(round(quantile(data[[metrics]], 0.999, na.rm=T), digits=2), metrics), "\n")
		#}
	}

    cat("\n")
	cat("\tfaster:\t", round(lowerthan, digits=2), "\t% of samples\n")
	cat("\tequal:\t", round(equalto, digits=2), "\t% of samples\n")
	cat("\tslower:\t", round(100 - lowerthan - equalto, digits=2), "\t% of samples\n")
	cat("\n")
}

put_metrics_in_dataframe <- function(data, metrics) {
	data_list = list()
	for (m in metrics) {
		newdata = data.frame(metrics = as.character(m), value=data[[m]], page=data$page)
		if (!is.null(data$run)) {
			newdata$run = data$run
		}
		data_list = c(data_list, list(newdata))
	}
	data_df = do.call(rbind, data_list)
	return(data_df)
}

# function to actually plot the heatmap

plot_heatmap <- function(data, metrics, baseline = "", compare = "", x_axis, y_axis, aggregate="median", filename="test", plot="eps", mainlabel=NULL, add_values_as_text=T, totalmin=NA, totalmax=NA, colorscale="gray", use_same_colorscale=F, MIDPOINT_OF_COLORSCALE=0, verbose=F, printdata=F, print_ci=F, debug_ci=F, print_colorscale=T, plotwidth=10, x_axis_label="", y_axis_label="", plot_x_axis_labels=T, plot_y_axis_labels=T, round_values_digits=1) {

	require(lattice)

	if (nrow(data) < 1) {
		cat("No pagedata for baseline", baseline, "and comparefactor", compare, "\n")
		return()
	}

	if (verbose) {
		cat("Plotting heatmap comparing", aggregate, metrics, "for", compare, "against", baseline, "-", nrow(data), "samples\n")
		cat("x_axis:", x_axis, levels(data[[x_axis]]), "(", length(levels(data[[x_axis]])), ") -- y_axis:", y_axis, levels(data[[y_axis]]), "(", length(levels(data[[y_axis]])), ")\n")
	}
	if (is.null(mainlabel)) {
		mainlabel = plotlabel.human.readable(metrics)
	}

	datamatrix = NULL
	lower_ci = NULL
	upper_ci = NULL

	for (yval in levels(data[[y_axis]])) {
		ydata = subset(data, data[[y_axis]] == yval)

		newcol = c()
		new_lower_ci = c()
		new_upper_ci = c()

		for (xval in levels(data[[x_axis]])) {

			dataitem = subset(ydata, ydata[[x_axis]] == xval)
			if (aggregate == "median") {
				aggregated_dataitem = median(dataitem[[metrics]], na.rm=T)
				if (print_ci) {
					# CI is between values at certain indices depending on the total number of values -- adjusting by +0.5 because there is no data at index 0 but there is data at index n
					# so if one value gets taken out at the bottom, one value gets taken out at the top, too
					upper_index = min(length(dataitem[[metrics]]), round((length(dataitem[[metrics]])/2) + (1.96*sqrt(length(dataitem[[metrics]]))/2)+0.5))
					lower_index = max(1, round((length(dataitem[[metrics]])/2) - (1.96*sqrt(length(dataitem[[metrics]]))/2)+0.5))
					new_lower_ci = c(new_lower_ci, sort(dataitem[[metrics]])[lower_index])
					new_upper_ci = c(new_upper_ci, sort(dataitem[[metrics]])[upper_index])
					#new_lower_ci = c(new_lower_ci, sort(dataitem[[metrics]])[max(1, qbinom(c(.025), length(dataitem[[metrics]]), 0.5))])
					#new_upper_ci = c(new_upper_ci, sort(dataitem[[metrics]])[min(length(dataitem[[metrics]]), qbinom(c(.975), length(dataitem[[metrics]]), 0.5))])
				}
				if (debug_ci) {
					cat(round(sort(dataitem[[metrics]]), round_values_digits), "\n")
					cat("For", length(dataitem[[metrics]]), "values, lower index: ", lower_index, "-->", round(new_lower_ci[length(new_lower_ci)], round_values_digits), ", upper index:", upper_index, "-->", round(new_upper_ci[length(new_upper_ci)], round_values_digits), "\n")
				}
			} else if (aggregate == "mean") {
				aggregated_dataitem = mean(dataitem[[metrics]], na.rm=T)
				if (print_ci) {
					new_lower_ci = c(new_lower_ci, mean(dataitem[[metrics]]) - qnorm(0.975) * sd(dataitem[[metrics]]) / sqrt(length(dataitem[[metrics]])))
					new_upper_ci = c(new_upper_ci, mean(dataitem[[metrics]]) + qnorm(0.975) * sd(dataitem[[metrics]]) / sqrt(length(dataitem[[metrics]])))
				}
				if (debug_ci) {
					cat(round(sort(dataitem[[metrics]]), round_values_digits), "\n")
					cat("For", length(dataitem[[metrics]]), "values, mean: ", mean(dataitem[[metrics]]), ", sd:", sd(dataitem[[metrics]]), "-->", round(new_lower_ci[length(new_lower_ci)], round_values_digits), "and", round(new_upper_ci[length(new_upper_ci)], round_values_digits), "\n")
				}
			} else if (aggregate == "speedup_percentage") {
				aggregated_dataitem = nrow(subset(dataitem, dataitem[[metrics]] < 0)) / nrow(dataitem) * 100
			} else if (grepl("^[[:digit:]]+q", aggregate)) {
				quantilenumber = as.integer(gsub("q", "", aggregate)) / 100
				aggregated_dataitem = quantile(dataitem[[metrics]], quantilenumber, na.rm=T)
			}
			newcol = c(newcol, aggregated_dataitem)
			if (printdata) {
				cat("\t", aggregate, metrics, "for", yval, "and", xval, "is", round(aggregated_dataitem, round_values_digits), "\n")
				if (length(new_lower_ci) > 0) {
					cat("\tCI is", round(new_lower_ci[length(new_lower_ci)], round_values_digits), "..", round(new_upper_ci[length(new_upper_ci)], round_values_digits), "\n")
				}
			}
		}
		if (is.null(datamatrix)) {
			datamatrix = matrix(newcol)
			if (length(new_lower_ci) > 0) {
				lower_ci = matrix(new_lower_ci)
				upper_ci = matrix(new_upper_ci)
			}
		} else {
			datamatrix = cbind(datamatrix, newcol)
			if (length(new_lower_ci) > 0) {
				lower_ci = cbind(lower_ci, new_lower_ci)
				upper_ci = cbind(upper_ci, new_upper_ci)
			}
		}
		if (printdata) {
			cat("\n")
		}
	}

	if (add_values_as_text) {
		panelfunction = function(x, y, z, ...) {
			panel.levelplot(x, y, z, ...)
			if (printdata) {
				cat("x:", x, "y:", y, "z:", round(z, round_values_data), ", CI:", paste("[", paste(round(lower_ci, round_values_data), round(upper_ci, round_values_data), sep=" .. "), "]", sep=""), "\n")
			}
			panel.text(x, y, round(z, round_values_digits))
			if (!is.null(lower_ci)) {
				panel.text(x, y, paste("\n\n[", paste(round(lower_ci, round_values_digits), round(upper_ci, round_values_digits), sep=".."), "]", sep=""), cex=0.5)
			}
		}
	} else {
		panelfunction = function(x, y, z, ...) { panel.levelplot(x, y, z, ...) }
	}

	if (!is.na(totalmin) & !is.na(totalmax)) {
		NUM_COLORS = 1000
		if (use_same_colorscale) {
			ourmin = totalmin
			ourmax = totalmax
			totalmin = ifelse(totalmin < 0, totalmin * 1.1, totalmin)
			totalmax = ifelse(totalmax > 0, totalmax * 1.1, totalmax * 0.9)
			if (verbose) {
				cat("Now totalmin:", totalmin, "totalmax:", totalmax, "ourmin:", ourmin, "ourmax:", ourmax, "\n")
			}
		} else {
			ourmin = min(datamatrix, na.rm=T)
			ourmax = max(datamatrix, na.rm=T)
			if (ourmin < totalmin) {
				totalmin = ourmin
			}
			if (ourmax > totalmax) {
				totalmax = ourmax
			}

			cat("Not using same colorscale. totalmin:", totalmin, "totalmax:", totalmax, "ourmin:", ourmin, "ourmax:", ourmax, "\n")
		}
		if (colorscale == "gray") {
			colorstep = (totalmax - totalmin) / NUM_COLORS
			lower_index = round((ourmin - totalmin) / colorstep)
			upper_index = round((ourmax - totalmin) / colorstep)
			levelplotcolors = gray.colors(NUM_COLORS, start=0.3, 1)[lower_index:upper_index]
			if (verbose) {
				cat("Using grayscale from", lower_index, "to", upper_index, "( min:", ourmin, "of", totalmin, ", max:", ourmax, "of", totalmax, "\n")
			}
		} else if (colorscale == "viridis") {
			library("viridis")
			totalrange = totalmax - totalmin
			colorstep = totalrange / NUM_COLORS

			if (totalmin < MIDPOINT_OF_COLORSCALE & totalmax > MIDPOINT_OF_COLORSCALE) {
				negative_cols = round(abs(totalmin) / totalrange * NUM_COLORS)
				negative_colorspace = rev(magma(negative_cols * 5/3)[round(negative_cols * 2/3):round(negative_cols * 5/3)]) # low index = bright, high = red and then violet
				positive_cols = round(abs(totalmax) / totalrange * NUM_COLORS)
				positive_colorspace = rev(viridis(positive_cols * 5/3)[round(positive_cols * 2/3):round(positive_cols * 5/3)]) # low index = bright yellow, high = green and then blue
				if (verbose) {
					cat("Total color range:", negative_cols, "negative colors,", positive_cols, "positive_colors\n")
				}
			} else if (totalmin > MIDPOINT_OF_COLORSCALE & totalmax > MIDPOINT_OF_COLORSCALE) {
				negative_colorspace = c()
				positive_colorspace = rev(viridis(NUM_COLORS * 5/3)[round(NUM_COLORS * 2/3):round(NUM_COLORS * 5/3)]) # low index = bright yellow, high = green and then blue
			} else if (totalmin < MIDPOINT_OF_COLORSCALE & totalmax < MIDPOINT_OF_COLORSCALE) {
				negative_colorspace = rev(magma(NUM_COLORS * 5/3)[round(NUM_COLORS * 2/3):round(NUM_COLORS * 5/3)]) # low index = bright, high = red and then violet
				positive_colorspace = c()
			}

			our_colors = c()

			if (ourmin < MIDPOINT_OF_COLORSCALE & ourmax > MIDPOINT_OF_COLORSCALE) {
				lower_index = max(round(abs(ourmin) / colorstep), 1)
				negative_colors = negative_colorspace[1:lower_index]
				upper_index = max(round(ourmax / colorstep), 1)
				positive_colors = positive_colorspace[1:upper_index]
				if (verbose) {
					cat("midpoint of colorscale", MIDPOINT_OF_COLORSCALE, "in plot - using", length(negative_colors), "negative and", length(positive_colors), "positive colors\n")
				}
			} else if (ourmin >= MIDPOINT_OF_COLORSCALE & ourmax > MIDPOINT_OF_COLORSCALE) {
				negative_colors = c()
				lower_index = max(round(ourmin / colorstep), 1)
				upper_index = max(round(ourmax / colorstep), 1)
				positive_colors = positive_colorspace[lower_index:upper_index]
				if (verbose) {
					cat("all values above midpoint of colorscale", MIDPOINT_OF_COLORSCALE, "- using", length(negative_colors), "negative and", length(positive_colors), "positive colors\n")
				}
			} else if (ourmin < MIDPOINT_OF_COLORSCALE & ourmax <= MIDPOINT_OF_COLORSCALE) {
				positive_colors = c()
				lower_index = max(round(abs(ourmin) / colorstep), 1)
				upper_index = max(round(abs(ourmax) / colorstep), 1)
				negative_colors = rev(negative_colorspace[lower_index:upper_index])
				if (verbose) {
					cat("all values below", MIDPOINT_OF_COLORSCALE, "- using", length(negative_colors), "negative and", length(positive_colors), "positive colors\n")
				}
			} else {
				cat("WTF happened? ourmin =", ourmin, "and ourmax =", ourmax, "\n")
			}
			levelplotcolors = c(rev(negative_colors), positive_colors)

			if (verbose) {
				cat("Using viridis from viridis:", length(positive_colors), "positive colors,", length(negative_colors), "negative colors, ( min:", ourmin, "of", totalmin, ", max:", ourmax, "of", totalmax, "-- midpoint of colorscale:", MIDPOINT_OF_COLORSCALE, "\n")
			}
		}
	} else {
		levelplotcolors = gray.colors(100)
	}

	colnames(datamatrix) = sapply(levels(data[[y_axis]]), plotlabel.human.readable)
	rownames(datamatrix) = sapply(levels(data[[x_axis]]), plotlabel.human.readable)
	if (x_axis_label == "") {
		xlab=plotlabel.human.readable(x_axis)
	} else {
		xlab=x_axis_label
	}
	if (y_axis_label == "") {
		ylab=plotlabel.human.readable(y_axis)
	} else {
		ylab=y_axis_label
	}
	if (!plot_y_axis_labels) {
		colnames(datamatrix) = rep("", length(levels(data[[y_axis]])))
		ylab=""
	}
	if (!plot_x_axis_labels) {
		rownames(datamatrix) = rep("", length(levels(data[[x_axis]])))
		xlab=""
	}

	filename=paste(filename, "heatmap", aggregate, metrics, baseline, compare, sep="_")
	intervals = max((totalmax - totalmin) / 100, 0.001)
	#intervals = max((totalmax - totalmin) / 100, 0.1)
	if (verbose) {
		cat("Totalmin:", totalmin, "totalmax:", totalmax, "intervals:", intervals, "\n")
	}

	prepare_file(plot=plot, filename=filename, spacebelow=5, width=plotwidth)
    #str(datamatrix)
	#print(levelplot(datamatrix, scales=list(x=list(rot=45)), panel = panelfunction, xlab=xlab, ylab=ylab, main=mainlabel, col.regions=levelplotcolors, colorkey=print_colorscale, at=seq(totalmin, totalmax, intervals)))
	print(levelplot(datamatrix, panel = panelfunction, xlab=xlab, ylab=ylab, main=mainlabel, col.regions=levelplotcolors, colorkey=print_colorscale, at=seq(totalmin, totalmax, intervals)))
	# use different color intervals for some??
	#print(levelplot(datamatrix, panel = panelfunction, xlab=plotlabel.human.readable(x_axis), ylab=plotlabel.human.readable(y_axis), main=mainlabel, col.regions=levelplotcolors, colorkey=print_colorscale, at=seq(totalmin, totalmax, 80)))

	if (plot %in% c("eps", "png", "pdf")) {
		invisible(dev.off())
	}
	cat("Plotted heatmap to", filename, plot, "\n\n")
}

plot_scatterplot <- function(data, xmetrics, ymetrics, filename="scatterplot", plot="eps", cex=1) {
	prepare_file(plot=plot, filename=filename, cex=cex, spacebelow=4, spaceleft=4)
	plot(data[[xmetrics]], data[[ymetrics]], xlab=plotlabel.human.readable(xmetrics), ylab=plotlabel.human.readable(ymetrics))
	dev.off()
}
