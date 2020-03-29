#!/usr/bin/Rscript
# Plots metrics logged by the Multi Access Manager
# by Theresa Enghardt

if (file.exists("../R_functions.R")) {
    source("../R_functions.R")
}

suppressMessages(library(zoo))

compute_policydata_predictions <- function(policydata, interfaces=c("eth0", "wlan0"), print=FALSE) {
    for (iface in interfaces) {
        if (is.null(policydata$count)) {
            next
        }
        policydata[[paste("prediction", iface, sep="_")]] = ifelse(policydata[[paste("num_sockets", iface, sep="_")]] > 0, policydata[[paste("median_srtt", iface, sep="_")]] + policydata$filesize / (policydata[[paste("download_max_rate", iface, sep="_")]] / (policydata$count + policydata$count_small)), policydata[[paste("median_srtt", iface, sep="_")]] * 2 + policydata$filesize / (policydata[[paste("download_max_rate", iface, sep="_")]] / (policydata$count + policydata$count_small)))
    }
    return(policydata)
}

compute_policydata_penalties <- function(policydata, penalizers=c("mean_of_rtt_vars", "median_of_rtt_vars", "srtt_var"), normalizers= c("min_srtt", "median_srtt", "mean_srtt"), interfaces=c("eth0", "wlan0"), print=FALSE) {
    for (iface in interfaces) {
        for (metrics in penalizers) {
            for (divisor in normalizers) {
                if (is.null(policydata[[paste(metrics, iface, sep="_")]]) | is.null(policydata[[paste(divisor, iface, sep="_")]])) {
                    next
                }
                if (print) {
                    cat("Computing", paste(metrics, iface, sep="_"), "by", paste(divisor, iface, sep="_"), "for", iface, "\n")
                }
                policydata[[paste(metrics, "by", divisor, iface, sep="_")]] = policydata[[paste(metrics, iface, sep="_")]] / policydata[[paste(divisor, iface, sep="_")]]
            }
        }
    }
    return(policydata)
}

merge_policydata_mamdata <- function(mamdata, policydata=NULL, pagedata=NULL, xdata_policy="timestamp", xdata_if="timestamp", metrics_to_copy_if=c("download_rate", "download_max_rate", "download_rate_max_600", "tx_rate", "rx_rate", "signal_strength", "channel_utilization"), metrics_to_copy_pf=c("min_srtt", "median_srtt", "mean_srtt", "srtt_var", "num_conns", "mean_of_rtt_vars", "median_of_rtt_vars"), print=FALSE, debugprint=FALSE, show_progress=F) {
    interfacedata = mamdata$ifdata
    prefixdata = mamdata$pfdata
    if (is.null(policydata)) {
        policydata = do.call(rbind, mamdata$eadata)
    }
    # If policydata is still NULL, there is nothing to merge
    if (is.null(policydata) || nrow(policydata) == 0) {
        return(NULL)
    }
    interfaces = unique(unlist(lapply(interfacedata, function(x) unique(x$interface))))

    # Initialize metrics
    for (metric in c(metrics_to_copy_if, metrics_to_copy_pf)) {
        for (interface in interfaces) {
            policydata[[paste(metric, interface, sep="_")]] = NA
        }
    }
    if (!is.null(pagedata)) {
        policydata$page = NA
    }

    progress_diff = 10
    progress_diff = ceiling(50 / nrow(policydata) * 100)
    if (show_progress) {
    cat("Got", nrow(policydata), "policydata points, reporting progress every", progress_diff, "%\n")
	cat("Policydata:\n")
	str(policydata)
	cat("Interfacedata:\n")
	str(interfacedata)
	cat("Prefixdata:\n")
	str(prefixdata)
    }
    counter=1
    for (index in 1:nrow(policydata)) {
		#cat("index", index, "out of", nrow(policydata), "\n")
        percent = (index / nrow(policydata)) * 100
        if ((round(percent) > counter * progress_diff) && show_progress) {
            counter = counter + 1
            cat(format(percent, digits=3), "% .. ")
        }
        thisdecision_if = interfacedata[grepl(policydata[index,]$run, names(interfacedata))]
        thisdecision_pf = prefixdata[grepl(policydata[index,]$run, names(prefixdata))]

        if (print) {
            cat("[", index, "]", as.character(policydata[index,]$page), "decision", policydata[index,]$index, "- getting mam data before:", format(policydata[index,][[xdata_policy]], nsmall=6), "\n")
        }

        for (ifd in thisdecision_if) {
            newifdata = fix.factors(lastreadingbefore(ifd, x=policydata[index,][xdata_policy], xdata=xdata_if))
            if(is.null(newifdata)) {
                cat("Warning: Got NULL data for", index, ". policydata item\n")
                next
            }
            for (metric in metrics_to_copy_if) {
                if (debugprint) {
                    cat("    Inserting", metric, "on", as.character(newifdata$interface)[1], ":", paste(newifdata[[metric]]), "\n")
                }
                policydata[index,][[paste(metric, as.character(newifdata$interface)[1], sep="_")]] = as.numeric(newifdata[[metric]])
            }
        }

        for (pfd in thisdecision_pf) {
            newpfdata = fix.factors(lastreadingbefore(pfd, x=policydata[index,][xdata_policy], xdata=xdata_if))
            if(is.null(newpfdata)) {
                cat("Warning: Got NULL data for", index, ". policydata item\n")
                next
            }
            for (metric in metrics_to_copy_pf) {
                if (debugprint) {
                    cat("    Inserting", metric, "on", as.character(newpfdata$interface)[1], ":", paste(newpfdata[[metric]]), "\n")
                }
                policydata[index,][[paste(metric, as.character(newpfdata$interface)[1], sep="_")]] = newpfdata[[metric]]
            }

        }
        #if (is.na(policydata[index,]$page) && !is.null(pagedata)) {
        #    policydata[index,]$page = as.character(lastreadingbefore(pagedata, x=policydata[index,][xdata_policy], xdata="starttimestamp")$page)
        #    if (debugprint) {
        #        cat("Getting last reading before", as.numeric(policydata[index,][xdata_policy]), "\n")
        #    }
        #    if (debugprint) {
        #        cat("Found page:", policydata[index,]$page, "\n")
        #    }
        #}
    }
    if (show_progress) {
        cat("\n")
    }
    policydata$page = as.factor(policydata$page)
    #policydata = compute_policydata_penalties(policydata, print=debugprint)
    #policydata = compute_policydata_predictions(policydata, print=debugprint)
    return(policydata)
}

one_page_load_over_time <- function(data, eadata, metrics=c("mean_srtt", "mean_of_rtt_vars"), filterscenarios=c(), xmin, xmax, xdata_plot="timestamp", plot_decision_lines=F, plottype="l", ylabel="", plotlabel="", plot="terminal", filename="plotovertime", rev_metrics_labels=F) {
	if (plot=="pdf") {
		prepare_pdf(filename, width=10, height=7, cex=1)
	}
	if (length(filterscenarios) > 0) {
		data = filter_scenarios(data, filterscenarios)
	}

	plotovertime(gettimewindow(data, xmin=xmin, xmax=xmax), metrics, xdata = xdata_plot, force_abs_x=TRUE, plottype=plottype, ylabel=ylabel, plotlabel=plotlabel, plot="terminal", filename=filename)
    if (!is.null(eadata) & plot_decision_lines) {
        decisions = gettimewindow(eadata, xmin=xmin, xmax=xmax)
        abline(v=decisions$timestamp, col="red")
    }
	if (plot=="pdf") {
		dev.off()
	}
}

mamdata_during_page_load <- function(pagedata, mamdata, page_start="starttimestamp", page_end="endtimestamp", beforestart=0.1, afterend=0, print=FALSE, show_progress=F) {
    ifdatalist = list()
    pfdatalist = list()
    interfacedata = mamdata$ifdata
    prefixdata = mamdata$pfdata

    ifcounter=1
    pfcounter=1
    if (!is.null(pagedata$run)) {
        splitpagedata = split(pagedata, pagedata$run)
    } else {
        splitpagedata = list(pagedata)
    }
    for (runpagedata in splitpagedata) {
        if (!is.null(runpagedata$run)) {
            run = unique(levels(runpagedata$run))
            if(show_progress) {
                cat("Doing run", run, "out of", length(splitpagedata), "\n")
            }
            thisrun_if = interfacedata[grepl(run, names(interfacedata))]
            thisrun_pf = prefixdata[grepl(run, names(prefixdata))]
        } else {
            thisrun_if = interfacedata
            thisrun_pf = prefixdata
        }
        progress_diff = round(50 / nrow(runpagedata) * 100)
        if (show_progress) {
            cat("Got", nrow(runpagedata), "pagedata points, reporting progress every", progress_diff, "%\n")
        }
        counter=1

        for (index in 1:nrow(runpagedata)) {
            percent = (index / nrow(runpagedata)) * 100
            if ((round(percent) > counter * progress_diff) && show_progress) {
                counter = counter + 1
                cat(format(percent, digits=3), "% .. ")
            }

            thispage = runpagedata[index,]
            if (print) {
                cat("Page:", thispage$page, "with", thispage$iface, "- getting time window:", format(thispage[[page_start]]-beforestart, nsmall=3), "..", format(thispage[[page_end]]+afterend, nsmall=3), "\n")
            }

            for (ifdata in thisrun_if) {
                newifdata = do.call(rbind, gettimewindow(thisrun_if, xmin=thispage[[page_start]]-beforestart, xmax=thispage[[page_end]]+afterend))
                if (is.null(newifdata)) {
                    cat("Got NULL ifdata for", as.character(thispage$page), "on", as.character(thispage$iface), "\n")
                    next
                }
                newifdata$page = thispage$page
                newifdata$policy = thispage$iface
                newifdata$workload = thispage$workload
                newifdata$time = thispage$wallclock_time
                newifdata$starttimestamp = thispage$starttimestamp
                newifdata$endtimestamp = thispage$endtimestamp

                if (print) {
                    str(newifdata)
                }

                if (!is.null(newifdata)) {
                    ifdatalist = c(ifdatalist, list(fix.factors(newifdata)))
                    names(ifdatalist)[ifcounter] = paste(thispage$page, thispage$iface, sep="__")
                    ifcounter=ifcounter + 1
                }
            }
            if (print) {
                cat("\n\n")
            }

            newpfdatalist = list()
            for (pfdata in thisrun_pf) {
                newpfdata = do.call(rbind, gettimewindow(thisrun_pf, xmin=thispage[[page_start]]-beforestart, xmax=thispage[[page_end]]+afterend))
                if (is.null(newpfdata)) {
                    cat("Got NULL pfdata for", as.character(thispage$page), "on", as.character(thispage$iface), "\n")
                    next
                }
                newpfdata$page = thispage$page
                newpfdata$policy = thispage$iface
                newpfdata$workload = thispage$workload
                newpfdata$time = thispage$wallclock_time
                newpfdata$starttimestamp = thispage$starttimestamp
                newpfdata$endtimestamp = thispage$endtimestamp
                if (!is.null(pfdatalist)) {
                    pfdatalist = c(pfdatalist, list(fix.factors(newpfdata)))
                    names(pfdatalist)[pfcounter] = paste(thispage$page, thispage$iface, sep="__")
                    pfcounter = pfcounter + 1
                }
            }
            if (print) {
                cat("\n\n\n")
            }
        }
        if (show_progress) {
            cat("\n\n")
        }
    }
    if (show_progress) {
        cat("Done with pages, now merging lists into data frames...\n")
    }
    ifdataframe = do.call(rbind, ifdatalist)
    rownames(ifdataframe) = c()
    if (show_progress) {
        cat("Done with merging interfaces\n")
    }
    pfdataframe = do.call(rbind, pfdatalist)
    rownames(pfdataframe) = c()
    if (show_progress) {
        cat("Done with merging prefixes\n")
    }
    return(list(ifdata=ifdataframe, pfdata=pfdataframe))
}

merge_pagedata_ifdata <- function(pagedata, interfacedata, xdata_page="starttimestamp", xdata_if="timestamp", print=FALSE) {
    ifdatalist = list()
    for (ifdata in interfacedata) {
        ifdata = ifdata[with(ifdata, order(ifdata[[xdata_if]])),]
        thisrun=levels(ifdata$run)[1]
        thisiface=levels(ifdata$interface)[1]
        pagesubset = fix.factors(subset(subset(pagedata, iface==thisiface), run==thisrun))
        if (print) {
            cat("Doing this for", thisrun, thisiface, "\n")
        }
        pagesubset = pagesubset[with(pagesubset, order(pagesubset[[xdata_page]])),]
        pagesubsetbypage = fix.factors(split(pagesubset, pagesubset$page))
        for (dataforonepage in pagesubsetbypage) {
            pagesubsetlist = split(dataforonepage, dataforonepage$try)
            newifdata = lapply(pagesubsetlist, function(p) { lastreadingbefore(ifdata, x=p[[xdata_page]], xdata=xdata_if) } )
            ifdataframe = do.call(rbind, newifdata)
            ifdataframe = ifdataframe[with(ifdataframe, order(ifdataframe[[xdata_if]])),]
            ifdataframe$try = as.factor(seq(1,10))
            ifdataframe = merge(ifdataframe, dataforonepage, by=c("run", "scenario", "try"))
            ifdataframe = ifdataframe[with(ifdataframe, order(ifdataframe[[xdata_if]])),]
            if (print) {
                str(ifdataframe)
            }
            ifdatalist = c(ifdatalist, list(ifdataframe))
        }
    }
    return(do.call(rbind, ifdatalist))
}

findprefix <- function(prefixlevels, interface) {
	cat("Looking for prefix for", interface, "\n")
	interface_components = strsplit(interface, "_")[[1]]

	str(prefixlevels)

	for (component in interface_components) {
		cat("Current component:",component,"\n")
		if (any(sapply(prefixlevels, function(x) grepl(component, x)))) {
			prefixlevels = prefixlevels[sapply(prefixlevels, function(x) grepl(component, x))]
			str(prefixlevels)
			if (length(prefixlevels) == 1) {
				return(prefixlevels[1])
			}
		}
	}
	return(prefixlevels[1])
}

plottimeseries <- function(data, ylabel="", ymax=0, ymin=0, xmax=0, xmin=0, metrics=c("download_rate"), plottype="l", col="black", plotlabel="", cex=1, xdata="timestamp", xlabel=xdata, yaxt="s", axisincludezero=TRUE, log="", wheretoplotlegend="", yaxis2="", verbose=F, rev_metrics_labels=F) {
	# If no y axis label was given, set it
	if (ylabel == "") {
		if (length(metrics) == 1) {
			ylabel = metrics[1]
		} else {
			ylabel = "various metrics"
		}
	}

    if (xmax == 0) {
        xmax = get_xmax(data, xdata)
    }
    if (xmin == 0) {
        xmin = get_xmin(data, xdata)
    }
	if (ymax == 0) {
		if (yaxis2 == "") {
			ymax = get_ymax(data, metrics)
		} else {
			cat("yaxis2", yaxis2, "excluded\n")
			ymax = get_ymax(data, metrics[metrics != yaxis2])
		}
	}
	if (ymax > 1000) {
		yaxt="n"
        ylabel = adjust_y_label(ymax, ylabel)
	} else if (ymax < 0 && axisincludezero) {
		# For metrics with negative values, include 0 on the axis
		ymax = 0
	}
	# For metrics with negative values, use ymin
	if (ymin == 0) {
		ymin = get_ymin(data, metrics)
	}
    # For logarithmic metrics, y axis cannot include zero
    if (log == "y") {
        axisincludezero = FALSE
        if (ymin == 0) {
            ymin=0.01
        }
    }

	if (ymin > 0 && axisincludezero) {
		ymin = 0
	}
    if (verbose) {
        cat("ylim:", ymin, ymax, "\n")
    }

	# Check if the same x value occurs multiple times in xdata, and fix this so we can plot properly
	xrle = rle(data[[xdata]])
	if(max(xrle$lengths) > 1) {
		# We have the same x value multiple times - e.g. if our timestamp granularity was too low
		newxdata = c()
		for (i in (1:length(xrle$lengths))) {
			# Compute new sequence of xdata values that has distinct values
			newxdata = c(newxdata, xrle$values[i] + seq(0, (1-1/xrle$lengths[i]), 1/xrle$lengths[i]))
		}
		data[[xdata]] = newxdata
	}

	# Plot first metrics for interface
	plot(data[[xdata]], data[[metrics[1]]], type=plottype, col=col, xlim = c(xmin, xmax), ylim=c(ymin, ymax), ylab=ylabel, xlab=xlabel, main=plotlabel, log=log, cex.lab=cex, cex.main=cex, cex.axis=cex, yaxt=yaxt)

	if (yaxt == "n") {
        draw_y_axis(ymax)
	}

	# If we have more metrics to plot
	if (length(metrics) > 1) {
		# Go through list of metrics
        for (i in 2:length(metrics)) {
            if (metrics[i] != yaxis2) {
                # Plot metrics
                if (plottype == "p") {
                    points(data[[xdata]], data[[metrics[i]]], col=col, pch=i)
                }
                if (plottype == "l") {
                    lines(data[[xdata]], data[[metrics[i]]], col=col, lty=i)
                }
			} else {
				# reset plot with new y axis
				par(new=T)
				if (plottype == "p") {
					plot(data[[xdata]], data[[metrics[i]]], xaxt="n", yaxt="n", ylab="", xlab="", col=col, pch=i)
				} else if (plottype == "l") {
					plot(data[[xdata]], data[[metrics[i]]], type="l", xaxt="n", yaxt="n", ylab="", xlab="", col=col, lty=i)
				}
				axis(side = 4)
				mtext(plotlabel.human.readable(yaxis2), side=4, line=3)


			}
		}
		# Plot legend explaining the different metrics (not needed if there is only one)
		if (wheretoplotlegend == "" ) {
			wheretoplotlegend = find_empty_plotarea(as.matrix(data[,metrics], byrow=T), c(ymin,ymax), F)
			if (wheretoplotlegend == "") {
				wheretoplotlegend = "topright"
			}
		}
		#cat("Plotting metrics legend at", wheretoplotlegend, "with labels", sapply(metrics, plotlabel.human.readable), "\n")
		legendlabels = sapply(metrics, plotlabel.human.readable)
		linetypes = (1:length(metrics))
		if (rev_metrics_labels) {
			legendlabels = rev(legendlabels)
			linetypes = rev(linetypes)
		}
        if (plottype == "p") {
            legend(wheretoplotlegend, legend=legendlabels, pch=linetypes, cex=cex)
        }
        if (plottype == "l") {
            legend(wheretoplotlegend, legend=legendlabels, lty=linetypes, cex=cex)
        }
	}
}

addtimeseries <- function(data, metrics=c("download_rate"), col="darkgoldenrod", xdata="timestamp", plottype="l") {
	# Check if the same x value occurs multiple times in xdata, and fix this so we can plot properly
    if (is.na(data[[xdata]]) || is.null(data[[xdata]])) {
        cat("Warning: Data", xdata, "and", metrics, "cannot be plotted! (is NULL or NA)\n")
        return()
    }
	xrle = rle(data[[xdata]])
	if(max(xrle$lengths) > 1) {
		# We have the same x value multiple times - e.g. if our timestamp granularity was too low
		newxdata = c()
		for (i in (1:length(xrle$lengths))) {
			newxdata = c(newxdata, xrle$values[i] + seq(0, (1-1/xrle$lengths[i]), 1/xrle$lengths[i]))
		}
		data[[xdata]] = newxdata
	}

	# Go through list of metrics
	for (i in 1:length(metrics)) {
		# Plot metrics for both interfaces
        if (plottype == "p") {
            points(data[[xdata]], data[[metrics[i]]], col=col, pch=i)
        }
        if (plottype == "l") {
            lines(data[[xdata]], data[[metrics[i]]], col=col, lty=i)
        }
	}
}

comparetimeseries <- function(data, data_labels=names(data), ylabel="", metrics=c("download_rate"), plottype="l", cols=c("black", "darkgoldenrod"), cex=1, plotlabel="", plot_mainlabel=T, xdata="timestamp", xlabel=xdata, log="", force_abs_x=FALSE, yaxis2="", verbose=F, rev_metrics_labels=F, ymax=NULL, force_plot_data_labels=F) {

    if (verbose) {
        cat("Comparing",metrics,"\n")
   }
	if (! class(data) == "list")
	{
		# This function works on lists - if we get a data frame, make a list out of it
		data = list(data)
	}

	# Get lengths of list items = interface-scenario-combinations to plot
	xlengths = c()
	xstarts = c()

	for (i in seq(1, length(data))) {
        if (is.null(data[[i]][[xdata]])) {
            cat("WARNING: xdata", xdata, "not found -- falling back to value index\n")
            data[[i]]$value_index = seq(1, length(rownames(data[[i]])))
            xdata = "value_index"
        }
    }


	for (item in data) {
		hasdata = FALSE
		for (metric in metrics) {
			if (all(is.na(item[[metric]]))) {
				cat("All",metric,"is NA!\n")
			} else {
				hasdata = TRUE
			}
		}
        if (xdata == "index_value" && is.null(item[[xdata]])) {

            cat("WARNING: xdata", xdata, "still not found -- falling back to value index")
            item$value_index = as.numeric(rownames(item))
            xdata = "value_index"
            str(item[[xdata]])
        }
		if (hasdata) {
			xlengths = c(xlengths, length(item[[xdata]]))
			xstarts = c(xstarts, item[[xdata]][1])
		}
	}

	if (length(xlengths) == 0) {
		cat("No data to plot!\n")
		return(FALSE)
	}
	if (length(unique(xstarts)) > 1 && !force_abs_x) {
		# Data frames do not start at the same xdata value - need to use relative xdata values
		if(is.null(data[[xdata]])) {
			data = compute_relative(data, xdata)
		}
		xdata = paste(xdata, "rel", sep="_")
		cat("Start values for x axis do not match - Using xdata", xdata, "instead\n")
	}

	if (max(xlengths) != min(xlengths)) {
#		# Data frames do not have the same length - need to stretch the shorter ones
		maxlength = max(xlengths)
		maxind = which.max(xlengths)
		for (i in 1:length(data)) {
			itemlength = xlengths[i]
			if (!is.na(itemlength) && itemlength < maxlength) {
				#cat("[", i,"/", length(data), "] Setting length from", itemlength, "to", maxlength, "based on", maxind, "\n")
				appenddata = data[[maxind]][(itemlength+1):maxlength,]
				appenddata[,metrics] = NA
				#str(appenddata)
				#cat("\n")
				#cat("data length:", length(data[[i]]), "appenddatalength:", length(appenddata), "\n")
				if (length(data[[i]]) == length(appenddata)) {
					data[[i]] = rbind(data[[i]], appenddata)
				} else {
					cat("Warning: Could not append data item", i, "\n")
				}
				#str(data[[i]])
				#cat("\n")
			} #else {
				#cat("This is the longest item\n")
			#}
		}
	}
    if (verbose) {
        cat("\n\n")
        str(summarizedata(data, metrics))
    }

	if (is.null(ymax)) {
		# Compute maximum of all values for y axis scaling
		if (yaxis2 == "") {
			ymax = get_ymax(data, metrics)
		} else {
			cat("yaxis2", yaxis2, "excluded\n")
			ymax = get_ymax(data, metrics[metrics != yaxis2])
		}
	}

	ymin = get_ymin(data, metrics)
    if (verbose) {
        cat("ymax", ymax, "ymin", ymin, "\n")
    }

	yaxt="s"
	# Make custom y axis for large y
	if (ymax > 10000) {
		yaxt="n"
	}

	# If no y axis label was given, set it
	if (ylabel == "") {
		if (length(metrics) == 1) {
			ylabel = metrics[1]
		} else {
			splitmetrics = strsplit(metrics[1], split="_")
			if (length(splitmetrics[[1]]) >= 2) {
				ylabel = splitmetrics[[1]][[2]]
			} else {
				ylabel = paste("metrics, e.g.", metrics[1])
			}
		}
	}

	if (length(cols) < length(data)) {
		cols = c(cols, rainbow(length(data)-2))
	}

	# Set main label
	if (plotlabel == "" && plot_mainlabel) {
		plotlabel = paste("Comparison of", ylabel, "for", paste(lapply(data_labels, plotlabel.human.readable), collapse=", "))
	}

    #str(data)
    if (verbose) {
        cat("\n\nStarting to find legend space for", metrics, "\n")
    }
	if (length(data) > 0) {
		datamatrix = as.matrix(data[[1]][,metrics], byrow=T, nrow=length(metrics))
		str(datamatrix)
        if (length(data) > 1) {
            for (i in 2:length(data)) {
                datamatrix = as.matrix(cbind(datamatrix, data[[i]][,metrics]))
            }
        }
		if (!is.null(data_labels)) {
			cat("Data labels:", data_labels, "\n")
			wheretoplotlegend = find_empty_plotarea(datamatrix, numlegends=2)
			cat("Plotting legend for data labels", wheretoplotlegend, "\n")
		}
        wheretoplotlegend1 = find_empty_plotarea(datamatrix, numlegends=1)
		cat("Plotting legend1 for metrics", wheretoplotlegend1, "\n")
	}

    xmax = get_xmax(data, xdata)
    if (verbose) {
        cat("Plotting from", min(data[[1]][xdata]), "to", xmax, "\n")
    }
	plottimeseries(data[[1]], yaxt=yaxt, ymax=ymax, ymin=ymin, xmax=xmax, ylabel=plotlabel.human.readable(ylabel), xlabel=plotlabel.human.readable(xlabel), plotlabel=plotlabel, metrics=metrics, col=cols[[1]], plottype=plottype, cex=cex, xdata=xdata, log=log, wheretoplotlegend=wheretoplotlegend1, yaxis2=yaxis2, verbose=verbose, rev_metrics_labels=rev_metrics_labels)

	#If we have more data to plot
	if (length(data) > 1) {
		# Go through list of data
		for (i in 2:length(data)) {
			# Plot metrics
			addtimeseries(data[[i]], metrics=metrics, col=cols[[i]], xdata=xdata, plottype=plottype)
		}
	}
	# Go through list of data
	if (!is.null(data_labels) && (length(data) > 1 || force_plot_data_labels)) {
		# Plot legend explaining the different sources of data
		if (wheretoplotlegend == "") {
			wheretoplotlegend = "topleft"
			cat("No suitable area for legend found - defaulting to", wheretoplotlegend, "\n")
		}
		legendlabels = sapply(data_labels, plotlabel.human.readable)
		cat("Plotting data legend at", wheretoplotlegend, "with labels:", legendlabels, "\n")
		legend(wheretoplotlegend, legend=legendlabels, col=cols, fill=cols, cex=cex)
	}
}

ploterrors <- function(data, if1="eth0", if2="wlan0", cex=1, plotlabel="", metrics=c("rx_errors", "tx_errors"), diff=F)
{
	if (diff) {
		metrics=c("rx_errors_diff", "tx_errors_diff")
	}
	comparetimeseries(list(data[[if1]], data[[if2]]), c(if1, if2), metrics=metrics, ylabel=paste(ifelse(diff, "Relative ",""),"Interface Error counter values", sep=""), cex=cex, plotlabel=plotlabel)
}

output802.11 <- function(data, if1="wlan0", if2="", cex=1, suffix="", plot="terminal")
{
	if (plot == "pdf") {
		prepare_pdf(filename="wirelessmetrics.pdf", cex=cex, width=10*cex, height=16*cex)
	}

	par(mfrow=c(3,1))
	# Set the font size in the plots again, since it was overwritten by setting mfrow
	par(cex=cex, cex.axis=cex, cex.lab=cex, cex.main=cex, cex.sub=cex)

	plotssi(data, if1=if1, if2=if2, cex=cex, plotlabelsuffix=suffix)
	plotrates(data, if1=if1, if2=if2, cex=cex, plotlabelsuffix=suffix)
	returnvalue = plotutilization(data, if1=if1, if2=if2, cex=cex, plotlabelsuffix=suffix)
	if (returnvalue == FALSE) {
		cat("Could not plot utilization!\n")
	}

	if (plot == "pdf") {
		dev.off()
	}
}

plotssi <- function(data, if1="wlan0", if2="", cex=1, plotlabelsuffix="")
{
	metrics=c("signal_strength", "signal_strength_avg", "signal_strength_bss")
	if (if2 == "") {
		plottimeseries(data[[if1]], metrics=metrics, ylabel="Signal Strength [dBm]", plotlabel=paste("Signal Strength Indicators on", if1, plotlabelsuffix))
	} else {
		comparetimeseries(list(data[[if1]], data[[if2]]), c(if1, if2), metrics=metrics, ylabel="Signal Strength [dBm]", plotlabel=paste("Signal Strength Indicators on", if1, "and", if2, plotlabelsuffix))
	}
}

plotrates <- function(data, if1="wlan0", if2="", cex=1, plotlabelsuffix="")
{
	metrics=c("tx_rate", "rx_rate")
	if (if2 == "") {
		plottimeseries(data[[if1]], metrics=metrics, ylabel="Send and receive modulation rate [Mbit/s]", plotlabel=paste("802.11 Modulation rates on", if1, plotlabelsuffix))
	} else {
		comparetimeseries(list(data[[if1]], data[[if2]]), c(if1, if2), metrics=metrics, ylabel="Send and receive modulation rate [Mbit/s]", plotlabel=paste("802.11 Modulation rates on", if1, "and", if2, plotlabelsuffix))
	}
}

plotutilization <- function(data, if1="wlan0", if2="", cex=1, plotlabelsuffix="")
{
	metrics=c("channel_utilization")
	if (if2 == "") {
		plottimeseries(data[[if1]], metrics=metrics, ylabel="Channel utilization [%]", plotlabel=paste("802.11 channel utilization as reported on", if1, plotlabelsuffix))
	} else {
		returnvalue = comparetimeseries(list(data[[if1]], data[[if2]]), c(if1, if2), metrics=metrics, ylabel="Channel utilization [%]", plotlabel=paste("802.11 channel utilization as reported on", if1, "and", if2, plotlabelsuffix))
		return(returnvalue)
	}
}

# Plot interface bandwidths for two interfaces against each other
comparebws <- function(data, if1="eth0", if2="wlan0", direction="download", xdata="timestamp", metrics=c("download_rate", "download_srate", "download_max_rate", "download_max_srate", "upload_rate", "upload_srate", "upload_max_rate", "upload_max_srate"), cex=1, plotlabel="", n=50) {

	#metrics = c("download_rate", "download_srate", "download_max_rate", "download_max_srate", "upload_rate", "upload_srate", "upload_max_rate", "upload_max_srate")
	library(pracma)
	library(zoo)

	#cat("Computing metrics", metrics, "\n")
	for (iface in c(if1, if2)) {
		#cat("Computing stuff for", iface, ", n=",n,"\n")
		#str(data[[iface]]$download_rate)
		#data[[iface]]$bw_sma = movavg(data[[iface]]$download_rate, n, "s")
		#data[[iface]]$bw_wma = movavg(data[[iface]]$download_rate, n, "w")
		#data[[iface]]$bw_ewma = movavg(data[[iface]]$download_rate, n, "e")
		#data[[iface]]$ubw_sma = movavg(data[[iface]]$upload_rate, n, "s")
		#data[[iface]]$ubw_wma = movavg(data[[iface]]$upload_rate, n, "w")
		#data[[iface]]$ubw_ewma = movavg(data[[iface]]$upload_rate, n, "e")
		data[[iface]]$down_rollmax = rollmaxr(data[[iface]]$download_rate, n, fill=0)
		data[[iface]]$up_rollmax = rollmaxr(data[[iface]]$upload_rate, n, fill=0)
	}
	metrics = c("download_rate", "down_rollmax", "download_max_rate", "download_max_srate", "upload_rate", "up_rollmax", "upload_max_rate", "upload_max_srate")
	#cat("Computed metrics\n")


	# Shorten list of bandwidth metrics to plot based on direction (if not "both" or "up- and download" or similar)
	if (direction == "download") {
		metrics = metrics[1:4]
	} else if (direction == "upload") {
		metrics = metrics[5:8]
	}
	
	#cat("Plotting", metrics, "...\n")
	# Plot the bandwidths
	comparetimeseries(list(data[[if1]], data[[if2]]), c(if1, if2), metrics=metrics, ylabel=paste(direction, "bitrate [Bit/s], n=", n), cex=cex, plotlabel=plotlabel, xdata=xdata)
}

boxplotmetrics_ea <- function(data, metrics=c("median_of_rtt_vars_by_min_srtt", "median_of_rtt_vars_by_median_srtt", "median_of_rtt_vars_by_mean_srtt"), interfaces=c("eth0", "wlan0"), mainlabel="", ylab="") {
	par(mar=c(20,8,2,1))
    ealist = list()
    i=1
    numruns = length(levels(data$run))
    runs = as.character(levels(data$run))
    for (metric in metrics) {
        for (iface in interfaces) {
            metriclabel = paste(metric, iface, sep="_")
            if (is.null(data[[metriclabel]])) {
                cat("Warning: No data for", metriclabel, "-- omiting\n")
                next
            }
            ealist = c(ealist, split(data[[metriclabel]][!is.infinite(data[[metriclabel]])], data$run[!is.infinite(data[[metriclabel]])]))
            for (j in 0:(numruns-1)) {
                names(ealist)[i+j] = paste(metric, iface, runs[j+1], sep="_")
            }
            i = i+numruns
        }
	}
	if (length(ealist) == 0 | all(is.null(ealist))) {
		cat("No data -- cannot plot.\n")
		return()
	}
	if (ylab == "") {
		if (all(grepl("srtt", names(ealist)))) {
			ylab="[ms]"
		}
	}
	boxplot(ealist, names = lapply(names(ealist), function(x) {y= strsplit(gsub("./", "", x), "_run")[[1]]; paste(y[1], make.plotlabel(strsplit(y[2], "[_:]")[[1]], print=F, exclude="policy"), sep=" ")}), ylab=ylab, las=2, main=mainlabel)

}

boxplotmetrics_pf <- function(data, pfs=NULL, metrics=c("min_srtt", "median_srtt", "mean_srtt")) {
    if (is.null(pfs)) {
        pfs=names(data$pfdata)
    }
	par(mar=c(10,4,2,1))
	pfdataframe = do.call(rbind, data$pfdata[pfs])
	pfdataframe$interface = factor(pfdataframe$interface, unique(pfdataframe$interface))
	str(pfdataframe)
    pflist = list()
	for (metric in metrics) {
        pflist[[metric]] = pfdataframe[[metric]]
	}
	boxplot(pflist, names = metrics, ylab="[ms]", las=2)
}


boxplotmetrics_if <- function(data, ifs=NULL, pfs=NULL, metrics=c("download_rate")) {
	if (is.null(pfs)) {
		if (!is.null(ifs)) {
			par(mar=c(10,4,2,1))
			ifdataframe = do.call(rbind, data$ifdata[ifs])
			ifdataframe$interface = factor(ifdataframe$interface, unique(ifdataframe$interface))
			str(ifdataframe)
			for (metric in metrics) {
				boxplot(ifdataframe[[metric]] ~ run:interface, ifdataframe, ylab=metric, las=2)
			}
		} else {
			cat("No interfaces or prefixes supplied - cannot plot!\n")
		}
	}
}

plotprefixdata <- function(data, pfs, metrics, ylabel="", cex=1, plotlabel="") {

	datalist=list()
	labellist=list()

	for (i in 1:length(pfs)) {
		if (is.null(data[[pfs[[i]]]])) {
			warning("Prefix ", pfs[[i]], " not found!\n")
		}
		datalist[[i]] = data[[pfs[[i]]]]
		labellist[[i]] = paste(pfs[[i]], " (", data[[pfs[[i]]]]$interface[1], ")", sep="")
	}

	# Plot the metrics
	comparetimeseries(datalist, labellist, metrics=metrics, ylabel=ylabel, cex=cex, plotlabel=plotlabel)
}

# Plot prefix Smoothed RTTs for one or more prefixes against each other
comparertts <- function(data, pfs=c("130.149.220.45", "141.23.76.89"), cex=1, plotlabel="", n=20) {

	#str(pfs)
	#metrics = c("mean_srtt", "median_srtt", "min_srtt")
	library(pracma)
	library(zoo)

	for (prefix in pfs) {
		#str(data[[prefix]]$median_srtt)
		data[[prefix]]$srtt_sma = movavg(data[[prefix]]$median_srtt, n, "s")
		data[[prefix]]$srtt_wma = movavg(data[[prefix]]$median_srtt, n, "w")
		data[[prefix]]$srtt_ewma = movavg(data[[prefix]]$median_srtt, n, "e")
		data[[prefix]]$srtt_movmed = rollmedianr(data[[prefix]]$median_srtt, fill=0, n+1)
	}
	metrics = c("median_srtt", "srtt_sma", "srtt_wma", "srtt_ewma", "srtt_movmed")

	# Plot the metrics
	plotprefixdata(data, pfs, metrics=metrics, ylabel=paste("Smoothed RTT [ms], n=", n), cex=cex, plotlabel=plotlabel)
}

# Plot data for early arrival policy
compare_ea_rate <- function(data, pfs=c("130.149.220.45", "141.23.76.89"), cex=1, plotlabel="") {
	metrics = c("maxrate", "rate", "capacity")

	# Plot the metrics
	plotprefixdata(data, pfs, metrics=metrics, ylabel="Bitrate [Bit/s]", cex=cex, plotlabel=plotlabel)
}

# Plot timings data for early arrival policy
compare_ea_prediction <- function(data, pfs=c("130.149.220.45", "141.23.76.89"), cex=1, plotlabel="") {
	metrics=c("prediction1000", "prediction10000", "prediction100000")

	plotprefixdata(data, pfs, metrics=metrics, ylabel="Predicted completion time [ms]", cex=cex, plotlabel=plotlabel)
}

# Plot rates as the early arrival policy actually took them into account
compare_ea_decision_rate <- function(data, pfs=names(data), cex=1, plotlabel="Comparison of rates in Early Arrival Policy", xdata="sequence", xlabel=xdata) {
	metrics = c("maxrate", "rate", "capacity")
	comparetimeseries(data, pfs, metrics=metrics, xdata=xdata, ylabel="Bitrate [Bit/s]", cex=cex, plotlabel=plotlabel, xlabel=xlabel)
}

# Plot times as the early arrival policy actually took them into account
compare_ea_decision_times <- function(data, pfs=names(data), cex=1, plotlabel="Comparison of times in Early Arrival Policy", xdata="sequence", xlabel=xdata) {
	metrics = c("srtt", "predicted_time")
	comparetimeseries(data, pfs, metrics=metrics, xdata=xdata, ylabel="Time [ms]", cex=cex, plotlabel=plotlabel, xlabel=xlabel)
}

compare_ea_filesize_decision <- function(data, pfs=names(data), cex=1, plotlabel="Comparison of file sizes and decisions in Early Arrival Policy", cols=c("black", "darkgoldenrod"), xdata="sequence", xlabel=xdata) {
	metrics = c("filesize")
	decisions=(levels(data[[1]]$decision))

	comparetimeseries(data, NULL, metrics=metrics, xdata=xdata, ylabel="File size [Bytes]", cex=cex, plotlabel=plotlabel, xlabel=xlabel)

	if (length(cols) < length(decisions)) {
		cols = c(cols, rainbow(length(decisions)-2))
	}

	pchs = 1:length(decisions)

	ymax=max(data[[1]]$filesize)

	i = 1
	for (decision in decisions) {
		points(data[[1]][[xdata]], ifelse(data[[1]]$decision==decision, ymax/2, ymax*2), pch=pchs[i], col=cols[i])
		i = i+1
	}
	
	legend("topleft", legend=decisions, col=cols, pch=pchs, cex=cex)
}

# Read in the prefix log data from a given file
readdata_pf <- function(prefixlogfile = "/tmp/metrics-prefix.log", print=T, run="", onlyiface=NULL) {
	if (print) {
		cat("	Reading", prefixlogfile, "...\n")
	}

	prefixdata <- read.csv(prefixlogfile, header=F)

	# Set column names
	colnames(prefixdata) = c("timestamp", "address", "interface", "mean_srtt", "srtt_var", "median_srtt", "min_srtt", "median_of_rtt_vars", "mean_of_rtt_vars", "loss_up", "num_conns")
    prefixdata = check_fieldcounts(prefixdata, prefixlogfile, print=print)

    if (!is.null(onlyiface)) {
        cat("    Only reading in prefixdata for", onlyiface, "\n")
        prefixdata$resolved_interface=substitute_addresses_by_interfaces(prefixdata$address)
        prefixdata = fix.factors(subset(prefixdata, resolved_interface==onlyiface))
    }
	timestamp0 = prefixdata[1,]$timestamp
	prefixdata$timestamp_rel = prefixdata$timestamp - timestamp0
	prefixdata$median_srtt[prefixdata$median_srtt == 0] = NA
	prefixdata$mean_srtt[prefixdata$mean_srtt == 0] = NA

	if (run != "") {
		prefixdata$run = as.factor(run)
		datematch = regexpr("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}-",run)
        dateofrun = substring(run, datematch, datematch+attr(datematch, "match.length")-2)
		prefixdata$dateofrun = as.factor(dateofrun)
        gsub(dateofrun, "", run)
        scenario = as.factor(substring(run, datematch+attr(datematch, "match.length")))
		prefixdata$scenario = as.factor(scenario)
		if (grepl("shaper", scenario)) {
			splitstring = strsplit(prefixlogfile, split="/")[[1]]
			filepath = paste(splitstring[1:(length(splitstring) - 1)], collapse="/")
			shaperscenarios = matchshaperscenarios(filepath, run, levels(prefixdata$interface))
			#cat("Shaperscenarios:",shaperscenarios,"\n")
			#str(prefixdata$interface)
			prefixdata$shaping = prefixdata$interface
			levels(prefixdata$shaping) = shaperscenarios
			prefixdata$qsize = as.factor(get_qsize_from_run_string(run))
			prefixdata$cross = as.factor(get_crosstraffic_from_run_string(run))
			prefixdata$shaped_rtt = as.numeric(gsub("ms", "", get_rtt_from_run_string(run, print=F)))
			prefixdata$wifi_or_cell = as.factor(ifelse(grepl("wifi", run), "wifi", "cell"))
			shaped_bw_up = get_bw_from_run_string(run, 1)
			prefixdata$shaped_bw_up = ifelse(grepl("kbit", shaped_bw_up), as.numeric(gsub("kbit", "", shaped_bw_up)), 1000 * as.numeric(gsub("Mbit", "", shaped_bw_up)))
			shaped_bw_down = get_bw_from_run_string(run, 2)
			prefixdata$shaped_bw_down = ifelse(grepl("kbit", shaped_bw_down), as.numeric(gsub("kbit", "", shaped_bw_down)), 1000 * as.numeric(gsub("Mbit", "", shaped_bw_down)))
			prefixdata$shaped_loss = as.numeric(gsub("loss", "", get_loss_from_run_string(run, print=F)))

		}
	}

	# Split data by prefix for further processing
	prefixdata = split(prefixdata, prefixdata$address)

	for (i in (1:length(prefixdata))) {
		names(prefixdata)[i] = paste(names(prefixdata)[i], unique(prefixdata[[i]]$shaping),sep="_")
	}
    names(prefixdata) = paste(names(prefixdata), run, sep="_")

	#cat("Names:",names(prefixdata), "\n")

	# Print data
	if (print) {
		str(prefixdata)
	}

	return(prefixdata)
}

# Read in the download and upload rate from a simplified interface bandwidth log file
readbws <- function(logfile){
	data <- read.csv(logfile, header=F)
	colnames(data) = c("timestamp", "interface", "download_rate", "upload_rate")
	str(data)
	return(data)
}

# Read in the interface log data from a given file
readdata_if <- function(interfacelogfile="/tmp/metrics-interface.log", run="", print=T, onlyiface=NULL) {
	if (print) {
		cat("	Reading", interfacelogfile, "...\n")
	}

	interfacedata <- read.csv(interfacelogfile, header=F)

	# Set column names
	colnames(interfacedata) = c("timestamp", "interface", "download_rate", "download_max_rate", "download_sma_max_long", "download_sma_max_mid", "download_sma_max_short", "download_sma_min_long", "download_sma_min_mid", "download_sma_min_short", "download_sma_10q_long", "download_sma_10q_mid", "download_sma_10q_short", "upload_rate", "upload_max_rate", "upload_counter", "download_counter", "rx_errors", "tx_errors", "signal_strength", "signal_strength_bss", "signal_strength_avg", "tx_rate", "rx_rate", "number_of_stations", "channel_utilization", "available_admission_capacity")
    interfacedata = check_fieldcounts(interfacedata, interfacelogfile, print=print)

    if (!is.null(onlyiface)) {
        cat("    Only reading in interface", onlyiface, "\n")
        interfacedata = fix.factors(subset(interfacedata, interface==onlyiface))
    }
	timestamp0 = interfacedata[1,]$timestamp
	interfacedata$timestamp_rel = interfacedata$timestamp - timestamp0

	# Multiply bitrates by 8 to get Bit/s instead of Bytes/s
	interfacedata$download_rate = interfacedata$download_rate * 8
    interfacedata$download_mbitrate = interfacedata$download_rate / (1024 * 1024)
	interfacedata$download_max_rate = interfacedata$download_max_rate * 8
	interfacedata$download_sma_max_long = interfacedata$download_sma_max_long * 8
	interfacedata$download_sma_max_mid = interfacedata$download_sma_max_mid * 8
	interfacedata$download_sma_max_short = interfacedata$download_sma_max_short * 8
	interfacedata$download_sma_min_long = interfacedata$download_sma_min_long * 8
	interfacedata$download_sma_min_mid = interfacedata$download_sma_min_mid * 8
	interfacedata$download_sma_min_short = interfacedata$download_sma_min_short * 8

	interfacedata$upload_rate = interfacedata$upload_rate * 8
    interfacedata$upload_mbitrate = interfacedata$upload_rate / (1024 * 1024)
	interfacedata$upload_max_rate = interfacedata$upload_max_rate * 8

	if (run != "") {
		interfacedata$run = as.factor(run)
		dateofrun = getdateofrun(run)
   		interfacedata$dateofrun = as.factor(dateofrun)
        gsub(dateofrun, "", run)
		interfacedata$scenario = as.factor(getrunscenario(run))
		if (grepl("shaper", run)) {
			splitstring = strsplit(interfacelogfile, split="/")[[1]]
			filepath = paste(splitstring[1:(length(splitstring) - 1)], collapse="/")

			#cat("File path:", filepath, "\n")
			shaperscenarios = matchshaperscenarios(filepath, run, levels(interfacedata$interface))
			#cat("Shaper scenarios:", shaperscenarios, "\n")
			#str(interfacedata$interface)
			if (any(shaperscenarios != "None")) {
				interfacedata$shaping = factor(shaperscenarios)
			}
			interfacedata$qsize = as.factor(get_qsize_from_run_string(run))
			interfacedata$cross = as.factor(get_crosstraffic_from_run_string(run))
			interfacedata$shaped_rtt = as.numeric(gsub("ms", "", get_rtt_from_run_string(run, print=F)))
			shaped_bw_up = get_bw_from_run_string(run, 1)
			interfacedata$shaped_bw_up = ifelse(grepl("kbit", shaped_bw_up), as.numeric(gsub("kbit", "", shaped_bw_up)), 1000 * as.numeric(gsub("Mbit", "", shaped_bw_up)))
			shaped_bw_down = get_bw_from_run_string(run, 2)
			interfacedata$shaped_bw_down = ifelse(grepl("kbit", shaped_bw_down), as.numeric(gsub("kbit", "", shaped_bw_down)), 1000 * as.numeric(gsub("Mbit", "", shaped_bw_down)))
			#str(interfacedata$shaped_bw_down)
			interfacedata$shaped_loss = as.numeric(gsub("loss", "", get_loss_from_run_string(run, print=F)))
			interfacedata$wifi_or_cell = as.factor(ifelse(grepl("wifi", run), "wifi", "cell"))
		}
	}

	# Split data by interface for further processing
	interfacedata = split(interfacedata, interfacedata$interface)

	interfacedata = compute_rel_diff(interfacedata, "rx_errors")
	interfacedata = compute_rel_diff(interfacedata, "tx_errors")

    suppressMessages(library(zoo))
    suppressMessages(library(pracma))
    suppressMessages(library(caTools))

    #rollmaxduration=c(10, 50, 100, 300, 600, 1200, 3000, 6000)
    rollmaxduration=c(10, 6000)
    for (i in seq(1, length(interfacedata))) {
        for (r in rollmaxduration) {
                if (nrow(interfacedata[[i]]) < r) {
                    # if data is shorter than rolling max duration, skip
                    next
                }
            #for (m in c("download_rate", "upload_rate")) {
            for (m in c("download_rate")) {
                interfacedata[[i]] = compute_rollmax(interfacedata[[i]], m, r)
                #interfacedata[[i]] = compute_quantile_of_last_n(interfacedata[[i]], m, r, q=0.9)
                #interfacedata[[i]] = compute_quantile_of_last_n(interfacedata[[i]], m, r, q=0.5)
                #str(interfacedata[[i]])
                #cat("r = ", r, "\n")
                interfacedata[[i]][[paste(m, "bw_sma", r, sep="_")]] = movavg(interfacedata[[i]][[m]], r, "s")
                #cat("sma done\n")
                #interfacedata[[i]][[paste(m, "bw_wma", r, sep="_")]] = movavg(interfacedata[[i]][[m]], r, "w")
                #cat("wma done\n")
                #interfacedata[[i]][[paste(m, "bw_ewma", r, sep="_")]] = movavg(interfacedata[[i]][[m]], r, "e")
                #cat("ewma done\n")

            }
        }
    }

	names(interfacedata) = paste(names(interfacedata), run, sep="_")

	# Print data
	if (print) {
		str(interfacedata)
	}

	return(interfacedata)
}

readdata_runs <- function(logprefix="data/", runs=list.files(path=logprefix, pattern="^run*"), onlyiface="wlan0", filterby=c(), print=FALSE) {
	interfacelist = list()
	prefixlist = list()
	eadatalist = list()
	runlengths=c()

	for (i in 1:length(runs)) {
		run = runs[i]
		cat("[", i, "/", length(runs), "] Reading in", run, "\n")
		ifdata = readdata_if(paste(logprefix, run, "/interface-combined.log", sep=""), print=print, run=run, onlyiface=onlyiface)
		pfdata = readdata_pf(paste(logprefix, run, "/prefix-combined.log", sep=""), print=print, run=run, onlyiface=onlyiface)

		eafiles = list.files(path=paste(logprefix, run, "/", sep=""), pattern = "eaf_*")
		eafiles = eafiles[grepl(".log", eafiles)]
		if (length(eafiles) > 0) {
			eadata = list()
			for (i in seq(1, length(eafiles))) {
				if (file.info(eafiles[i])$size < 1) {
					# File is empty!
					next
				}
				neweadata = readdata_ea(paste(logprefix, run, "/", eafiles[i], sep=""), print=print, run=run)
				neweadata$policy = as.factor(gsub(".log", "", eafiles[i]))
				neweadata$run = as.factor(run)
				eadata = c(eadata, list(neweadata))
			}
		} else {
			cat("Did not find eadata files in", run, "\n")
			eadata = NULL
		}
		gsub("run-", "", run)
		if (grepl("shaper", run)) {
			names(ifdata) = paste(names(ifdata), paste(getdateofrun(run), "shaper", sep="-"), sep="_")
		} else {
			names(ifdata) = paste(names(ifdata), run, sep="_")
			names(pfdata) = paste(names(pfdata), run, sep="_")
		}
		interfacelist = c(interfacelist, ifdata)
		prefixlist = c(prefixlist, pfdata)
		eadatalist = c(eadatalist, list(eadata))
		runlengths=c(runlengths, length(ifdata[[1]]$timestamp))

	}
    if (length(filterby) > 0) {
        interfacelist = filter_scenarios(interfacelist, filterby)
        prefixlist = filter_scenarios(prefixlist, filterby)
    }

	return(list(ifdata = interfacelist, pfdata = prefixlist, eadata=eadatalist, runs=data.frame(name=runs, length=runlengths)))
}

aggregate_quantiles <- function(dataframe, origdata, metrics, quantiles, withoutzero=FALSE, print=FALSE) {
    for (m in metrics) {
        if (withoutzero) {
            datatouse = origdata[[m]][origdata[[m]] > 0]
        } else {
            datatouse = origdata[[m]]
        }
        quantilevalues = do.call("quantile", list(x=origdata[[m]], probs=quantiles, na.rm=TRUE))
        if (print) {
            cat("   quantiles of", m, ":", quantilevalues, "\n")
        }
        for (q in quantiles) {
            dataframe[[paste(q, "q_", m, sep="")]] = do.call("quantile", list(x=origdata[[m]], probs=q, na.rm=TRUE))
        }
    }
    return(dataframe)
}

aggregate_data_into_frame <- function(dataframe, origdata, metrics, functions, withoutzero=FALSE, print=FALSE) {
    for (m in metrics) {
        for (func in functions) {
            if (withoutzero) {
                datatouse = origdata[[m]][origdata[[m]] > 0]
            } else {
                datatouse = origdata[[m]]
            }
            value = do.call(func, list(datatouse, na.rm=TRUE))
            if (print) {
                cat("   ", func, m, ":", value, "(", length(datatouse), "values)\n")
            }
            dataframe[[paste(func, m, sep="_")]] = value
        }
    }
    return(dataframe)
}

compute_plt_metrics_df <- function(pagedata, metricsdata, basemetrics=c("run", "interface", "scenario", "shaping", "dateofrun"), aggregationfunctions=c("min", "median", "mean", "max", "sd"), quantiles=c(0.9, 0.95, 0.99, 0.999), aggregationmetrics_pagedata=c("actual_time"), aggregationmetrics_if=c("download_rate", "download_max_rate", "signal_strength", "rx_rate"), aggregationmetrics_pf=c("median_srtt", "min_srtt"), print=FALSE) {
    i = 1
    outputlist = list()
    for (i in 1:length(metricsdata$ifdata)) {
        newdataframe = data.frame(id = names(metricsdata$ifdata)[i])
        for (metrics in basemetrics) {
            newdataframe[[metrics]] = metricsdata$ifdata[[i]][[metrics]][[1]]
        }
        if (print) {
            cat("Looking at", names(metricsdata$ifdata)[i], "\n")
            #str(metricsdata$ifdata[[i]])
        }
        newdataframe = aggregate_data_into_frame(newdataframe, metricsdata$ifdata[[i]], aggregationmetrics_if, aggregationfunctions, print=print)
        newdataframe = aggregate_quantiles(newdataframe, metricsdata$ifdata[[i]], aggregationmetrics_if, quantiles, print=print)
        newdataframe = aggregate_data_into_frame(newdataframe, metricsdata$pfdata[[i]], aggregationmetrics_pf, aggregationfunctions, withoutzero=TRUE, print=print)
        newdataframe = aggregate_quantiles(newdataframe, metricsdata$pfdata[[i]], aggregationmetrics_pf, quantiles, withoutzero=TRUE, print=print)

        thispagedata=subset(subset(pagedata, run==levels(metricsdata$ifdata[[i]]$run)[1]), iface==levels(metricsdata$ifdata[[i]]$interface))
        #str(fix.factors(thispagedata))
        newdataframe = aggregate_data_into_frame(newdataframe, thispagedata, aggregationmetrics_pagedata, aggregationfunctions, print=print)
        newdataframe = aggregate_quantiles(newdataframe, thispagedata, aggregationmetrics_pagedata, quantiles, print=print)
        outputlist = c(outputlist, list(newdataframe))
    }
    if (print) {
        str(do.call(rbind, outputlist))
    }
    return(do.call(rbind, outputlist))
}

compute_correlation <- function(data, metrics="actual_time", print=FALSE) {
    factors=names(data)[sapply(names(data), function(x) is.factor(data[[x]]))]
    nonfactors=names(data)[!sapply(names(data), function(x) is.factor(data[[x]]))]
    xmetrics=nonfactors[grepl(metrics, nonfactors)]
    ymetrics=nonfactors[!grepl(metrics, nonfactors)]
    corrlist = list()
    for (x in xmetrics) {
        if (sd(data[[x]], na.rm=TRUE) == 0) {
            cat("Sd was 0 for", x, "\n")
            next
        }
        if (print) {
            cat("x metrics:", x, "\n")
        }
        for (y in ymetrics) {
            ydata = data[[y]][is.finite(data[[y]])]
            xdata = data[[x]][is.finite(data[[y]])]
            if ((sd(ydata, na.rm=TRUE)) == 0) {
                if (print) {
                    cat("Sd was 0 for", y, "\n")
                }
                next
            }
            if (print) {
                cat("\ty metrics:", y, "\n")
            }
            for (method in c("kendall", "spearman")) {
                res = cor.test(xdata, ydata, method=method, exact=FALSE)
                if (!is.na(res$p.value) && (res$p.value < 0.05)) {
                    if (print) {
                        cat("\t\t", method, ": correlation found with p=", res$p.value, "coefficient =", res$estimate, "\n")
                    }
                    newcorr= data.frame(xmetric = x, ymetric = y, pvalue = res$p.value, corr = unname(res[["estimate"]]), method=method)
                    corrlist = c(corrlist, list(newcorr))
                }
            }
        }
    }
    corrdataframe = do.call(rbind, corrlist)
    return(corrdataframe[with(corrdataframe, order(pvalue)),])
}

readdata_ea <- function(ealogfile = "data/run-2017-11-14T14:48-shaper_test-10ms-2Mbit-0loss_test-100ms-20Mbit-0loss/policy_log_eaf_threshold.csv", run="", print=F) {
	eadata = read.csv(ealogfile, header=F)

	# Set column names
	cf = count.fields(ealogfile, sep=",")
	if (unique(cf)[1] == 13) {
		# Logfile directly by policy
		colnames(eadata) = c("timestamp", "filesize", "sockets_eth0", "num_sockets_eth0", "sockets_wlan0", "num_sockets_wlan0", "prob_eth0", "prob_wlan0", "random_number", "predicted_time", "count", "count_small", "decision")
	} else {
		if (unique(cf)[1] == 10) {
			colnames(eadata) = c("timestamp", "filesize", "sockets_eth0", "num_sockets_eth0", "sockets_wlan0", "num_sockets_wlan0", "predicted_time", "count", "count_small", "decision")
		} else {
			# Logfile by visualize_policies script
			colnames(eadata) = c("index", "page", "download_max_rate", "download_rate", "timestamp", "filesize", "count", "count_small", "decision", "predicted_time", "actual_time")
		}
	}

	eadata$count_factor = factor(eadata$count)
	eadata$count_small_factor = factor(eadata$count_small)
	if (!is.null(eadata$actual_time)) {
		eadata$gap = eadata$actual_time - eadata$predicted_time
		eadata$gap_abs = abs(eadata$gap)
	}
    if (print) {
        str(eadata)
    }
	return(eadata)
}

# Compute early arrival policy metrics
compute_early_arrival <- function(prefixdata, interfacedata) {
	EPSILON=0.0001
	policydata = list()
	prefixlevels = levels(prefixdata[[1]]$address)
	interfacelevels = levels(interfacedata[[1]]$interface)

	for (i in 1:length(prefixdata)) {

		pfdata = prefixdata[[i]]
		interface = interfacelevels[pfdata$interface[1]]

		#cat("Prefix", prefixlevels[pfdata$address[[1]]], "on interface", interface, "\n")

		policydata[[i]] = data.frame(address=pfdata$address, interface=pfdata$interface, timestamp=pfdata$timestamp)

		ifdata = interfacedata[[interface]]

		tpmin = min(pfdata$timestamp)
		tpmax = max(pfdata$timestamp)

		timin = min(ifdata$timestamp)
		timax = max(ifdata$timestamp)

		if (tpmin != timin || tpmax != timax) {
			cat("Time stamp mismatch!\n")
			cat("Prefixdata:", tpmin, "..", tpmax, "(", length(pfdata$timestamp), "), Interfacedata: ", timin, "..", timax, "(", length(ifdata$timestamp), ")\n")
			return(NULL)
		}

		ifdata$download_max_srate[is.na(ifdata$download_max_srate)] = 0
		ifdata$download_max_rate[is.na(ifdata$download_max_rate)] = 0
		ifdata$download_srate[is.na(ifdata$download_srate)] = 0
		ifdata$download_rate[is.na(ifdata$download_rate)] = 0
		pfdata$min_srtt[is.na(pfdata$min_srtt)] = 0
		pfdata$mean_srtt[is.na(pfdata$mean_srtt)] = 0
		pfdata$median_srtt[is.na(pfdata$median_srtt)] = 0

		policydata[[i]]$maxrate = ifelse(ifdata$download_max_srate < EPSILON, ifdata$download_max_rate, ifdata$download_max_srate)
		policydata[[i]]$rate = ifelse(ifdata$download_srate < EPSILON, ifdata$download_rate, ifdata$download_srate)
		policydata[[i]]$capacity = policydata[[i]]$maxrate - policydata[[i]]$rate

		policydata[[i]]$rtt = ifelse(pfdata$median_srtt < EPSILON, 0, pfdata$median_srtt)

		policydata[[i]]$prediction1000 = ifelse(policydata[[i]]$rtt < EPSILON, 0, 2 * policydata[[i]]$rtt + 1000 / policydata[[i]]$capacity)
		policydata[[i]]$prediction10000 = ifelse(policydata[[i]]$rtt < EPSILON, 0, 2 * policydata[[i]]$rtt + 10000 / policydata[[i]]$capacity)
		policydata[[i]]$prediction100000 = ifelse(policydata[[i]]$rtt < EPSILON, 0, 2 * policydata[[i]]$rtt + 100000 / policydata[[i]]$capacity)
		
		policydata[[i]]$prediction1000[is.infinite(policydata[[i]]$prediction1000)] = 0
		policydata[[i]]$prediction10000[is.infinite(policydata[[i]]$prediction10000)] = 0
		policydata[[i]]$prediction100000[is.infinite(policydata[[i]]$prediction100000)] = 0
	}
	names(policydata) = prefixlevels
	
	#str(policydata)
	return(policydata)
}

# Print a summary of the data for a given interface
datasummary <- function(prefixdata, interfacedata, filterscenarios=c("eth0", "wlan0", "10.1.1.3", "10.1.2.4"), interfacemetrics=c("download_rate", "download_max_rate", "download_srate", "download_max_srate", "upload_rate", "upload_max_rate", "upload_srate", "upload_max_srate"), prefixmetrics = c("mean_srtt", "median_srtt", "min_srtt"), index="run") {

    if (length(filterscenarios) > 0) {
        interfacedata = filter_scenarios(interfacedata, filterscenarios)
        prefixdata = filter_scenarios(prefixdata, filterscenarios)
    }

	# Print summary of all given metrics for the given interface
    summarizedata(interfacedata, metrics=interfacemetrics, index=index)

	cat("\n\n")

    summarizedata(prefixdata, metrics=prefixmetrics, index=index)
}

# Plot a comparison of interface up- und download bandwidths and prefix srtts
plotcomparisons <- function(logfileprefix="/tmp/", dataset="", data=NULL, plot="terminal", cex=1, if1="eth0", if2="wlan0", family="IPv4", what="stats", xmin=0, xmax=0) {

	interfacedata = NULL
	prefixdata = NULL
	policystats = NULL
	ealist = NULL

	if (!is.null(data) && what == "stats") {
		interfacedata = data$ifdata
		prefixdata = data$pfdata
	} else if (what == "early_arrival_decisions") {
		eafile = paste(logfileprefix, "ea.log", sep="")
		cat("Reading data from", eafile, "\n")
		ealist = readdata_ea(eafile)
	} else {
		# If no particular dataset was given, take the log files locally logged by MAM
		if (dataset == "") {
			interfacelogfile=paste(logfileprefix, "interface.log", sep="")
			prefixlogfile=paste(logfileprefix, "prefix.log", sep="")
		} else {
		# Otherwise, get the logfile names based on the given dataset
			interfacelogfile=paste(dataset, "_if.log", sep="")
			prefixlogfile=paste(dataset, "_pf.log", sep="")
			cat("Reading dataset", dataset, "from", interfacelogfile, "and", prefixlogfile, "\n")
		}
		# Read data from the log files
		interfacedata=readdata_if(interfacelogfile, print=F)
		prefixdata=readdata_pf(prefixlogfile, print=F)
	}

	if (what == "early_arrival_stats") {
		policystats = compute_early_arrival(prefixdata, interfacedata)
	}

	xlabelsuffix = ""

	# If timestamps are given, subset the data
	if (xmin > 0 && xmax > 0) {
		if (!is.null(interfacedata)) {
			interfacedata = gettimewindow(interfacedata, xmin=xmin, xmax=xmax)
			if (length(interfacedata[[1]]$timestamp) == 0) {
				cat("No data beween", xmin, "and", xmax, "\n");
				return()
			}
		}
		if (!is.null(prefixdata)) {
			prefixdata = gettimewindow(prefixdata, xmin=xmin, xmax=xmax)
			if (length(prefixdata[[1]]$timestamp) == 0) {
				cat("No data beween", xmin, "and", xmax, "\n");
				return()
			}
		}
		if (!is.null(policystats)) {
			policystats = gettimewindow(policystats, xmin=xmin, xmax=xmax)
			if (length(policystats[[1]]$timestamp) == 0) {
				cat("No data beween", xmin, "and", xmax, "\n");
				return()
			}
		}
		if (!is.null(ealist)) {
			ealist = gettimewindow(ealist, xmin=xmin, xmax=xmax)
			if (length(ealist[[1]]$timestamp) == 0) {
				cat("No data beween", xmin, "and", xmax, "\n");
				return()
			}
		}

		xlabelsuffix = paste("(", xmin, " =< timestamp < ", xmax, ")", sep="")
		cat("length of interface list:", length(interfacedata[[1]]$timestamp), ", is.null?", is.null(interfacedata), "\n")
	} else {
		if (!is.null(interfacedata)) {
			xmin = min(interfacedata[[1]]$timestamp)
			xmax = max(interfacedata[[1]]$timestamp)
		} else if (!is.null(ealist)) {
			xmin = min(ealist[[1]]$timestamp)
			xmax = max(ealist[[1]]$timestamp)
		}	
	}

	cat("\n\tPlotting", what, ifelse(dataset == "", "", paste("of", dataset)), "between", xmin, "and", xmax, "to", plot, "\n")

	if (plot=="png") {
		# If font size "1" is given, set to 2, otherwise too small in PNG
		if (cex == 1) {
			cex=2
		}
		# Set PNG size based on given font size
		prepare_png(filename=paste(dataset, "_", what, "_cmp_bw_srtt.png", sep=""), cex=cex, width=cex*600, height=cex*1000)
	} else if (plot=="pdf") {
		# Set PDF size based on given font size
		prepare_pdf(filename=paste(dataset, "_", what, "_cmp_bw_srtt.pdf", sep=""), cex=cex, width=cex*10, height=cex*16)
	}

	# Configure the plot to have 3 rows
	par(mfrow=c(3,1))
	# Set the font size in the plots again, since it was overwritten by setting mfrow
	par(cex=cex, cex.axis=cex, cex.lab=cex, cex.main=cex, cex.sub=cex)

	if (what == "early_arrival_decisions") {
		# Plot timings
		compare_ea_decision_rate(ealist, xlabel=paste("sequence", xlabelsuffix))
		compare_ea_decision_times(ealist, xlabel=paste("sequence", xlabelsuffix))
		compare_ea_filesize_decision(ealist, xlabel=paste("sequence", xlabelsuffix))
	} else {
		# Get relevant prefixes

		# Initialize empty vector
		plotprefixes=c(NA, NA)

		# Get prefix labels
		prefixlevels = names(prefixdata)

		# String that matches both IPv4 and IPv6 addresses
		grepstring="[.:]"

		if (family == "IPv4" || family == "v4" || family == "AF_INET") {
			# Only match IPv4 prefixes
			grepstring = "[.]"
		}
		
		if (family == "IPv6" || family == "v6" || family == "AF_INET6") {
			# Only match IPv6 prefixes
			grepstring = "[:]"
		}

		splitif1 = strsplit(if1, "_")[[1]]
		splitif2 = strsplit(if2, "_")[[1]]
		if1name = splitif1[[1]]
		if2name = splitif2[[1]]
		if (length(splitif1) > 1) {
			run1name = strsplit(if1, "_")[[1]][[2]]
		} else {
			run1name = NA
		}
		if (length(splitif2) > 1) {
			run2name = strsplit(if2, "_")[[1]][[2]]
		} else {
			run2name = NA
		}

		#cat("run names:", run1name, "and", run2name, "\n")
		# Go through list of prefixes
		for (i in 1:length(prefixdata)) {
			prefix = prefixdata[[i]]
			#cat("Check if prefix is in", if1name, ifelse(if1name == if2name, "", paste("or", if2name)), ifelse((is.na(run1name) || is.na(run2name) || run1name == run2name), "", paste("and run matches", run1name, "or", run2name)), "\n")
			#cat("This run is", prefix$run[1], "\n")
			if (!is.na(run1name) && !is.na(run2name) && prefix$run[1] != run1name && prefix$run[1] != run2name) {
				#cat("Wrong run\n")
			} else {
				#cat("Right run\n")
				prefixaddrs = levels(prefixdata[[i]]$address)
				# If this prefix matches the address family...
				if (grepl(grepstring, prefixaddrs[prefix$address[1]])) {
					# If this prefix belongs to the given interface
					if (prefix$interface[1] == if1name) {
						#cat("Found match!\n\n")
						# Add prefix to list of prefixes to be plotted
						if (is.na(plotprefixes[1])) {
							plotprefixes[1] = prefixlevels[i]
						} else {
							if (if1name == if2name && is.na(plotprefixes[2])) {
								plotprefixes[2] = prefixlevels[i]
							} else {
							plotprefixes = c(plotprefixes, prefixlevels[i])
							}
						}
					} else if (prefix$interface[1] == if2name) {
						#cat("Found match!\n\n")
						# Add prefix to list of prefixes to be plotted
						if (is.na(plotprefixes[2])) {
							plotprefixes[2] = prefixlevels[i]
						} else {
							plotprefixes = c(plotprefixes, prefixlevels[i])
						}
					}
				}
			}
		}

		#str(plotprefixes)
		#str(c(if1, if2))
		str(prefixlevels)

		if (all(is.na(plotprefixes))) {
			# Could not find prefixes with runs
			plotprefixes[1] = findprefix(prefixlevels, if1)
			plotprefixes[2] = findprefix(prefixlevels, if2)
			cat("Found plotprefixes:", plotprefixes, "\n")
		}


		if (what == "stats") {
			# Plot bandwidth comparisons for both interfaces
			comparebws(interfacedata, if1=if1, if2=if2, direction="download")
			cat("Plotted bws\n")
			#comparebws(interfacedata, if1=if1, if2=if2, direction="upload")
			ploterrors(interfacedata, if1=if1, if2=if2, diff=F)
			cat("Finished plotting errors\n")
		
			# Plot RTTs for the given prefixes
			comparertts(prefixdata, pfs=plotprefixes, plotlabel=paste("Comparison of Smoothed RTT [ms] for", paste(plotprefixes, collapse=", ")))
		} else if (what == "early_arrival_stats") {
			compare_ea_rate(policystats, pfs=plotprefixes, plotlabel="Bit rates and capacities according to Early Arrival Policy")
			compare_ea_prediction(policystats, pfs=plotprefixes, plotlabel="Predicted completion times according to Early Arrival Policy")

			# Plot RTTs for the given prefixes
			comparertts(prefixdata, pfs=plotprefixes, plotlabel=paste("Comparison of Smoothed RTT [ms] for", family, "prefixes"))
		}
	}

	if (plot=="png" || plot=="pdf") {
		# Close the file
		invisible(dev.off())
	}
}
