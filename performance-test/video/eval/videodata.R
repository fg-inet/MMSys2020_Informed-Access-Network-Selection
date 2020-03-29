#!/usr/bin/Rscript
# Functions to read in video data

source("../../web-timing/eval/plottimings.R")
source("../../mam-metrics/processdata.R")
source("../../R_functions.R")
source("../../mam-metrics/process_by_run.R")


read_abr_runs <- function(logprefix="data/", runs=list.files(path=logprefix, pattern="^run*"), filename="abr.log", print=FALSE) {
	abrdata = data.frame()
	abrlist = list()

	for (i in 1:length(runs)) {
		run = runs[i]
		cat("  [", i, "/", length(runs), "] Reading in data from", run, "\n")
		data = read_abrdata(paste(logprefix, run, "/", sep=""), filename=filename, print=print)
		if (is.null(data)) {
			next
		}
		gsub("run-", "", run)
		datematch = regexpr("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}-",run)
		dateofrun = substring(run, datematch, datematch+attr(datematch, "match.length")-2)
		data$run = as.factor(run)
		#data$methodology = as.factor(get_methodology_label(run))
		#data$workload = as.factor(get_workload(run))
		gsub(dateofrun, "", run)
		scenario = as.factor(substring(run, datematch+attr(datematch, "match.length")))
		if (print) {
			cat("  Dateofrun:", dateofrun, "scenario:", scenario, "\n")
		}
		data$scenario = scenario
		#s = strsplit(run, "[/_:+-]")[[1]]
		#latencyindex = which(grepl("ms", s))
		#bandwidthindex = which(grepl("Mbit", s))
		#if (length(latencyindex) == 2 && length(bandwidthindex) == 2) {
		#	data$if1_rtt = as.factor(s[latencyindex[1]])
		#	data$if2_rtt = as.factor(s[latencyindex[2]])
		#	data$if1_bw = as.factor(s[bandwidthindex[1]])
		#	data$if2_bw = as.factor(s[bandwidthindex[2]])
		#} else {
		#	data$if1_rtt = NA
		#	data$if2_rtt = NA
		#	data$if1_bw = NA
		#	data$if2_bw = NA
		#}
		#if (!grepl("shaper", run)) {
		#	data$policy = as.factor(paste(data$iface,data$scenario,sep="_"))
		#} else {
		#	data$policy = data$iface:data$policy
		#}
		data$policy = data$iface
		downloaddata = read_downloaddata(paste(logprefix, run, "/", sep=""), print=print)

		data=merge(data, downloaddata)
		data = data[with(data, order(starttimestamp_numeric, segmentindex)),]

		abrlist[[i]] = data
	}
    abrlistlength1 = length(abrlist[[1]])
    if (all(lapply(abrlist, function(x) length(x)) == abrlistlength1)) {
        # All list items are similar - we can combine them to a single data frame
        abrdata = do.call(rbind, abrlist)
    } else {
        abrlist = abrlist[vapply(abrlist, Negate(is.null), NA)]
        cat("Warning: Lengths of data items do not match (different clients?)\n")
        cat("Some columns were filled with NAs.\n")
        abrdata = do.call(rbind, filldataframes(abrlist))
    }
    # Only use data with more than half of max loaded segments
    # i.e., throw out broken runs due to Segfault etc
    rows_before = nrow(abrdata)
    maxseg = tapply(abrdata$segmentindex, INDEX=abrdata$policy:abrdata$abr, FUN=max)
    HALF_MAX = max(maxseg, na.rm=T) / 2
    abrdata = fix.factors(subset(abrdata, abrdata$policy:abrdata$abr %in% names(maxseg[maxseg>HALF_MAX])))
    if (print) {
        cat("Only using runs with more than", HALF_MAX, "segments -- down from", rows_before, "to", nrow(abrdata), "\n")
    }

#	if (any(grepl("MAM", runs))) {
#		mam = sapply(abrdata$scenario, function(x) {matchobj=regexpr("[A-Z]+", x); return(ifelse(attr(matchobj, "match.length") > 2 , substring(x, matchobj, matchobj+attr(matchobj, "match.length")-1), "MAM"))})
#		abrdata$mam = factor(mam)
#		abrdata$policy = abrdata$iface:abrdata$policy:abrdata$mam
#	}
	if (print) {
        cat("Full ABR data:\n")
		str(abrdata)
	}
	return(abrdata)
}


read_abrdata <- function(logprefix=list.files(path="data/", pattern="run-*")[1], filename="abr.log", print=TRUE) {
	abrdata = data.frame()
	abrlist = list()

    colnames_to_use=c("workload", "policy", "scenario", "starttimestamp", "abr", "segmentindex", "representation", "bufferlevel", "dl_rate_prev", "max_buffer")
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
		if (print) {
			cat("Checking", filename, "\n")
		}
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
        tdata$client = as.factor("gpac")
        tdata$bufferlevel = tdata$bufferlevel / 1000

        tdata$starttimestamp_numeric = as.numeric(as.POSIXct(gsub("T", " ", tdata$starttimestamp)))
        tdata = tdata[with(tdata, order(tdata[["starttimestamp_numeric"]])),]

		abrlist[[i]] = tdata
	}
	abrdata = do.call(rbind, abrlist)

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
    #    #abrdata=merge(abrdata, pdata, by=c("try", "iface"))
    #}

	if (print) {
		str(abrdata)
	}
	return(abrdata)
}

read_downloaddata <- function(logprefix=list.files(path="data/", pattern="run-*")[1], filename="download.log", print=TRUE) {
	downloaddata = data.frame()
	downlist = list()

	colnames_to_use=c("workload", "policy", "scenario", "starttimestamp", "segmentindex", "timestamp_start", "timestamp_end")
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

		tdata = subset(tdata, segmentindex > 0)
		datapoints_before = nrow(tdata)

		for (startt in levels(tdata$starttimestamp)) {

			# Only keep last download with last segmentindex 1 -- all previous are
			# just the init segments
			tdata1 = subset(tdata, tdata$starttimestamp == startt & tdata$segmentindex == 1)
			tdata_last = max(tdata1$timestamp_end)
			tdata = subset(tdata, starttimestamp != startt | timestamp_end >= tdata_last)
			if (print) {
				cat("Starttime", startt, ":", nrow(tdata1), "with last timestamp", tdata_last, "\n")
				cat("Filtered", startt, "for segment index 1 >=", tdata_last, "- from", datapoints_before, "to", nrow(tdata), "\n")
			}
			datapoints_before = nrow(tdata)
		}

		tdata$downloadtime = tdata$timestamp_end - tdata$timestamp_start

		downlist[[i]] = tdata
	}
	downloaddata = do.call(rbind, downlist)

	if (print) {
		str(downloaddata)
	}
	return(downloaddata)
}

read_init_runs <- function(logprefix="data/", runs=list.files(path=logprefix, pattern="^run*"), filename="initial_playout.log", print=FALSE) {
	initdata = data.frame()
	initlist = list()

	for (i in 1:length(runs)) {
		run = runs[i]
		cat("  [", i, "/", length(runs), "] Reading in data from", run, "\n")
		data = read_initial_playoutdata(paste(logprefix, run, "/", sep=""), filename=filename, print=print)
		if (is.null(data)) {
			next
		}
		gsub("run-", "", run)
		datematch = regexpr("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}-",run)
		dateofrun = substring(run, datematch, datematch+attr(datematch, "match.length")-2)
		data$run = as.factor(run)
		#data$methodology = as.factor(get_methodology_label(run))
		#data$workload = as.factor(get_workload(run))
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
		data$policy = data$iface
		data$load_terminal = data$timestamp_terminal_loaded - data$timestamp_first_loc
		data$load_mpd = data$timestamp_start_initsegs - data$timestamp_terminal_loaded
		data$load_initsegs = data$timestamp_end_initsegs - data$timestamp_start_initsegs
		data$initial_playout_delay = data$timestamp_start_playout - data$timestamp_start_mpd_load

		initlist[[i]] = data
	}
	initlistlength1 = length(initlist[[1]])
	if (all(lapply(initlist, function(x) length(x)) == initlistlength1)) {
		# All list items are similar - we can combine them to a single data frame
		initdata = do.call(rbind, initlist)
	} else {
		initlist = initlist[vapply(initlist, Negate(is.null), NA)]
		cat("Warning: Lengths of data items do not match (different clients?)\n")
		cat("Some columns were filled with NAs.\n")
		initdata = do.call(rbind, filldataframes(initlist))
	}

#	if (any(grepl("MAM", runs))) {
#		mam = sapply(initdata$scenario, function(x) {matchobj=regexpr("[A-Z]+", x); return(ifelse(attr(matchobj, "match.length") > 2 , substring(x, matchobj, matchobj+attr(matchobj, "match.length")-1), "MAM"))})
#		initdata$mam = factor(mam)
#		initdata$policy = initdata$iface:initdata$policy:initdata$mam
#	}
	if (print) {
		str(initdata)
	}
	return(initdata)
}

read_initial_playoutdata <- function(logprefix=list.files(path="data/", pattern="run-*")[1], filename="initial_playout.log", print=TRUE) {
	initdata = data.frame()
	initlist = list()

	#colnames_to_use=c("workload", "policy", "scenario", "starttimestamp", "timestamp_first_loc", "url", "timestamp_terminal_loaded", "timestamp_start_mpd_load", "timestamp_start_initsegs", "timestamp_end_initsegs", "timestamp_player_initialized", "timestamp_start_playout")
	colnames_to_use=c("workload", "policy", "scenario", "starttimestamp", "timestamp_first_loc", "url", "timestamp_terminal_loaded", "timestamp_start_mpd_load", "timestamp_start_initsegs", "timestamp_end_initsegs", "timestamp_player_initialized", "timestamp_start_playout", "foo")
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
		tdata$timestamp_start_playout[is.na(tdata$timestamp_start_playout)] = tdata$timestamp_player_initialized[is.na(tdata$timestamp_start_playout)]

		initlist[[i]] = tdata
	}
	initdata = do.call(rbind, initlist)

	if (print) {
		str(initdata)
	}
	return(initdata)
}

read_frame_runs <- function(logprefix="data/", runs=list.files(path=logprefix, pattern="^run*"), filename="frame_drawn.log", print=FALSE) {
	framedata = data.frame()
	framelist = list()

	for (i in 1:length(runs)) {
		run = runs[i]
		cat("  [", i, "/", length(runs), "] Reading in data from", run, "\n")
		data = read_frame_drawn(paste(logprefix, run, "/", sep=""), filename=filename, print=print)
		if (is.null(data)) {
			next
		}
		gsub("run-", "", run)
		datematch = regexpr("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}-",run)
		dateofrun = substring(run, datematch, datematch+attr(datematch, "match.length")-2)
		data$run = as.factor(run)
		#data$methodology = as.factor(get_methodology_label(run))
		#data$workload = as.factor(get_workload(run))
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
		data$policy = data$iface

		framelist[[i]] = data
	}
	framelistlength1 = length(framelist[[1]])
	if (all(lapply(framelist, function(x) length(x)) == framelistlength1)) {
		# All list items are similar - we can combine them to a single data frame
		framedata = do.call(rbind, framelist)
	} else {
		framelist = framelist[vapply(framelist, Negate(is.null), NA)]
		cat("Warning: Lengths of data items do not match (different clients?)\n")
		cat("Some columns were filled with NAs.\n")
		framedata = do.call(rbind, filldataframes(framelist))
	}

#	if (any(grepl("MAM", runs))) {
#		mam = sapply(framedata$scenario, function(x) {matchobj=regexpr("[A-Z]+", x); return(ifelse(attr(matchobj, "match.length") > 2 , substring(x, matchobj, matchobj+attr(matchobj, "match.length")-1), "MAM"))})
#		framedata$mam = factor(mam)
#		framedata$policy = framedata$iface:framedata$policy:framedata$mam
#	}
	if (print) {
		str(framedata)
	}
	return(framedata)
}


read_frame_drawn <- function(logprefix=list.files(path="data/", pattern="run-*")[1], filename="frame_drawn.log", print=TRUE) {
	framedata = data.frame()
	framelist = list()

	colnames_to_use=c("workload", "policy", "scenario", "starttimestamp", "frame", "timestamp_drawn")
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
		tdata$drawn_diff = c(0, diff(tdata$timestamp_drawn))
		tdata$drawn_diff = ifelse(as.integer(tdata$frame) > 0, tdata$drawn_diff, 0)

		framelist[[i]] = tdata
	}
	framedata = do.call(rbind, framelist)

	if (print) {
		str(framedata)
	}
	return(framedata)
}

read_policydata <- function(logprefix=list.files(path="data/", pattern="run-*")[1], filename="policy.log", print=TRUE) {
	policydata = data.frame()
	policylist = list()

	colnames_to_use=c("workload", "policy", "scenario", "starttimestamp", "timestamp", "sockets_1", "num_sockets_1", "sockets_2", "num_sockets_2", "foo", "bar", "baz", "filesize", "category", "bitrate", "duration", "decision", "reason")
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
        cf = count.fields(paste(logprefix, files[i], sep=""), sep=",")
		tdata = read.csv(paste(logprefix,files[i], sep=""), header=FALSE)
		colnames(tdata) = colnames_to_use
        tdata$foo = NULL
        tdata$bar = NULL
        tdata$baz = NULL
        tdata$num_sockets_1[is.na(tdata$num_sockets_1)] = 0
        tdata$num_sockets_2[is.na(tdata$num_sockets_2)] = 0

		policylist[[i]] = tdata
	}
	policydata = do.call(rbind, policylist)

	if (print) {
		str(policydata)
	}
	return(policydata)
}

read_policy_runs <- function(logprefix="data/", runs=list.files(path=logprefix, pattern="^run*"), filename="policy.log", print=FALSE) {
	policydata = data.frame()
	policylist = list()

	for (i in 1:length(runs)) {
		run = runs[i]
		cat("  [", i, "/", length(runs), "] Reading in data from", run, "\n")
		data = read_policydata(paste(logprefix, run, "/", sep=""), filename=filename, print=print)
		if (is.null(data)) {
			next
		}
		gsub("run-", "", run)
		datematch = regexpr("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}-",run)
		dateofrun = substring(run, datematch, datematch+attr(datematch, "match.length")-2)
		data$run = as.factor(run)
		#data$methodology = as.factor(get_methodology_label(run))
		#data$workload = as.factor(get_workload(run))
		gsub(dateofrun, "", run)
		scenario = as.factor(substring(run, datematch+attr(datematch, "match.length")))
		if (print) {
			cat("  Dateofrun:", dateofrun, "scenario:", scenario, "\n")
		}
		data$scenario = scenario
		#if (!grepl("shaper", run)) {
		#	data$frame = as.factor(paste(data$iface,data$scenario,sep="_"))
		#} else {
		#	data$frame = data$iface:data$policy
		#}
		data$policy = data$iface

		policylist[[i]] = data
	}
	policylistlength1 = length(policylist[[1]])
	if (all(lapply(policylist, function(x) length(x)) == policylistlength1)) {
		# All list items are similar - we can combine them to a single data frame
		policydata = do.call(rbind, policylist)
	} else {
		policylist = policylist[vapply(policylist, Negate(is.null), NA)]
		cat("Warning: Lengths of data items do not match (different clients?)\n")
		cat("Some columns were filled with NAs.\n")
		policydata = do.call(rbind, filldatapolicys(policylist))
	}

#	if (any(grepl("MAM", runs))) {
#		mam = sapply(policydata$scenario, function(x) {matchobj=regexpr("[A-Z]+", x); return(ifelse(attr(matchobj, "match.length") > 2 , substring(x, matchobj, matchobj+attr(matchobj, "match.length")-1), "MAM"))})
#		policydata$mam = factor(mam)
#		policydata$policy = policydata$iface:policydata$policy:policydata$mam
#	}
	if (print) {
		str(policydata)
	}
	return(policydata)
}

read_qoe_runs <- function(logprefix="data/", runs=list.files(path=logprefix, pattern="^run*"), filename="qoe.log", print=FALSE) {
	qoedata = data.frame()
	qoelist = list()

	for (i in 1:length(runs)) {
		run = runs[i]
		cat("  [", i, "/", length(runs), "] Reading in data from", run, "\n")
		data = read_qoedata(paste(logprefix, run, "/", sep=""), filename=filename, print=print)
		if (is.null(data)) {
			next
		}
		gsub("run-", "", run)
		datematch = regexpr("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}-",run)
		dateofrun = substring(run, datematch, datematch+attr(datematch, "match.length")-2)
		data$run = as.factor(run)
		#data$methodology = as.factor(get_methodology_label(run))
		#data$workload = as.factor(get_workload(run))
		gsub(dateofrun, "", run)
		scenario = as.factor(substring(run, datematch+attr(datematch, "match.length")))
		if (print) {
			cat("  Dateofrun:", dateofrun, "scenario:", scenario, "\n")
		}
		data$scenario = scenario

		qoelist[[i]] = data
	}
	qoelistlength1 = length(qoelist[[1]])
	if (all(lapply(qoelist, function(x) length(x)) == qoelistlength1)) {
		# All list items are similar - we can combine them to a single data frame
		qoedata = do.call(rbind, qoelist)
	} else {
		qoelist = qoelist[vapply(qoelist, Negate(is.null), NA)]
		cat("Warning: Lengths of data items do not match (different clients?)\n")
		cat("Some columns were filled with NAs.\n")
		qoedata = do.call(rbind, filldataframes(qoelist))
	}

#	if (any(grepl("MAM", runs))) {
#		mam = sapply(qoedata$scenario, function(x) {matchobj=regexpr("[A-Z]+", x); return(ifelse(attr(matchobj, "match.length") > 2 , substring(x, matchobj, matchobj+attr(matchobj, "match.length")-1), "MAM"))})
#		qoedata$mam = factor(mam)
#		qoedata$qoe = qoedata$iface:qoedata$qoe:qoedata$mam
#	}
	if (print) {
		str(qoedata)
	}
	return(qoedata)
}

read_qoedata <- function(logprefix=list.files(path="data/", pattern="run-*")[1], filename="qoe.log", print=TRUE) {
	qoedata = data.frame()
	qoelist = list()

	colnames_to_use=c("workload", "scenario", "starttimestamp", "abr", "policy", "qoe")
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
        cf = count.fields(paste(logprefix, files[i], sep=""), sep=",")
		tdata = read.csv(paste(logprefix,files[i], sep=""), header=FALSE)
		colnames(tdata) = colnames_to_use

		qoelist[[i]] = tdata
	}
	qoedata = do.call(rbind, qoelist)

	if (print) {
		str(qoedata)
	}
	return(qoedata)
}

read_cqm_runs <- function(logprefix="data/", runs=list.files(path=logprefix, pattern="^run*"), filename="cqm_vals.csv", print=FALSE) {
	qoedata = data.frame()
	qoelist = list()

	for (i in 1:length(runs)) {
		run = runs[i]
		cat("  [", i, "/", length(runs), "] Reading in data from", run, "\n")
		data = read_cqmdata(paste(logprefix, run, "/", sep=""), filename=filename, print=print)
		if (is.null(data)) {
			next
		}
		gsub("run-", "", run)
		datematch = regexpr("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}-",run)
		dateofrun = substring(run, datematch, datematch+attr(datematch, "match.length")-2)
		data$run = as.factor(run)
		#data$methodology = as.factor(get_methodology_label(run))
		#data$workload = as.factor(get_workload(run))
		gsub(dateofrun, "", run)
		scenario = as.factor(substring(run, datematch+attr(datematch, "match.length")))
		rr = rle(as.integer(data$starttimestamp))
		data$content = unlist(sapply(rr$lengths, function(x) seq(1, x)))
		if (print) {
			cat("  Dateofrun:", dateofrun, "scenario:", scenario, "\n")
		}
		data$scenario = scenario

		qoelist[[i]] = data
	}
	qoelistlength1 = length(qoelist[[1]])
	if (all(lapply(qoelist, function(x) length(x)) == qoelistlength1)) {
		# All list items are similar - we can combine them to a single data frame
		qoedata = do.call(rbind, qoelist)
	} else {
		qoelist = qoelist[vapply(qoelist, Negate(is.null), NA)]
		cat("Warning: Lengths of data items do not match (different clients?)\n")
		cat("Some columns were filled with NAs.\n")
		qoedata = do.call(rbind, filldataframes(qoelist))
	}

#	if (any(grepl("MAM", runs))) {
#		mam = sapply(qoedata$scenario, function(x) {matchobj=regexpr("[A-Z]+", x); return(ifelse(attr(matchobj, "match.length") > 2 , substring(x, matchobj, matchobj+attr(matchobj, "match.length")-1), "MAM"))})
#		qoedata$mam = factor(mam)
#		qoedata$qoe = qoedata$iface:qoedata$qoe:qoedata$mam
#	}
	if (print) {
		str(qoedata)
	}
	return(qoedata)
}

read_cqmdata <- function(logprefix=list.files(path="data/", pattern="run-*")[1], filename="cqm_vals.log", print=TRUE) {
	qoedata = data.frame()
	qoelist = list()

	colnames_to_use=c("workload", "scenario", "starttimestamp", "cqm_qoe")
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
        cf = count.fields(paste(logprefix, files[i], sep=""), sep=",")
		tdata = read.csv(paste(logprefix,files[i], sep=""), header=FALSE)
		colnames(tdata) = colnames_to_use

		qoelist[[i]] = tdata
	}
	qoedata = do.call(rbind, qoelist)

	if (print) {
		str(qoedata)
	}
	return(qoedata)
}



compute_stallings <- function(data, init_data, segment_duration=1000, verbose=T, summaryprint=T) {
	splitdata = fix.factors(split(data, data$starttimestamp))
	if (verbose) {
		#cat(levels(data$starttimestamp), "\n")
		#str(levels(init_data$starttimestamp))
		#str(data$starttimestamp)
		cat("Split data based on", levels(data$starttimestamp), "into list of length", length(splitdata), "\n")
		#str(splitdata)
	}
	for (p in seq(1, length(splitdata))) {
        downloadtimesum = 0
        stallingsum = 0
		playout = splitdata[[p]]
		playout$early_or_late = rep(0, length(playout$segmentindex))
		playout$supposed_to_be_there = rep(0, length(playout$segmentindex))
		playout$stalling = rep(0, length(playout$segmentindex))
		if (verbose) {
			#str(playout)
			cat("Looking for initdata starting at", as.character(levels(playout$starttimestamp)), "\n")
			#str(subset(init_data, starttimestamp == levels(playout$starttimestamp)))
		}
		i1 = subset(init_data, starttimestamp == levels(playout$starttimestamp))
		# Start timestamp of the actual playout
		start_playout_time = i1$timestamp_start_playout[1]
		if (verbose) {
			cat("Playout started at", start_playout_time, "for", length(playout$segmentindex), "segments\n")
		}
		playout$stalling_duration=0

		for (i in seq(2, length(playout$segmentindex))) {
			if (playout[i,]$timestamp_end <= start_playout_time) {
				# Playout had not yet started when this segment was loaded -- no stalling
				next
			}
			downloadtimesum = downloadtimesum + playout[i,]$downloadtime
			if (verbose) {
				cat("Segment", i, "load started after", playout[i,]$timestamp_start - start_playout_time, "and ended after", playout[i,]$timestamp_end - start_playout_time, "ms - download duration:", playout[i,]$downloadtime, "-- sum:", downloadtimesum, "-- stalling sum:", stallingsum, "\n")

			}

			# When was this segment load supposed to be finished?
			# e.g., segment 2 at start timestamp + 1 * duration of one segment
			#	   segment 3 at start timestamp + 2 * duration of one segment
			#	   and so on...
			playout[i,]$supposed_to_be_there = start_playout_time + (playout[i,]$segmentindex- 1) * segment_duration

			# Comparing to whet the segment load was actually finished:
			# Did the segment arrive in time ("early" --> negative value)
			# or was it late ("late" --> positive value)
			playout[i,]$early_or_late = playout[i,]$timestamp_end - playout[i,]$supposed_to_be_there

			# If the segment was late, this segment load stalled the playout
			playout[i,]$stalling = ifelse(playout[i,]$early_or_late > 0, 1, 0)

			if (verbose) {
				cat("\tsupposed to be there at", playout[i,]$supposed_to_be_there, "(after", playout[i,]$supposed_to_be_there - start_playout_time, ") but actually there at", playout[i,]$timestamp_end, "--> stalling", ifelse(playout[i,]$stalling > 0, "HAPPENED!!", "did not happen"), "\n")
			}

			if (playout[i,]$stalling > 0) {
				# Stalling happened -- store stalling duration
				playout[i,]$stalling_duration = playout[i,]$early_or_late / 1000

				stallingsum = stallingsum + playout[i,]$stalling_duration
				
				# adjust (fictional) playout start depending on playback resume timestamp
				# so we get the correct "supposed to be there" timestamp for later segments
				start_playout_time = playout[i,]$timestamp_end - (playout[i,]$segmentindex - 1) * segment_duration

				if (verbose) {
					cat("Stalling duration:", playout[i,]$early_or_late, "ms -- adjusted playout start to", start_playout_time, "\n")
				}

			}
		}
		if (summaryprint) {
			cat("\nComputed stallings for playout starting at", as.character(levels(playout$starttimestamp)), "(ABR:", as.character(levels(playout$abr)), ", policy:", as.character(levels(playout$policy)), "):\n\t", sum(playout$stalling), "stallings", ifelse(sum(playout$stalling) > 0, paste("of durations", paste(playout$stalling_duration[playout$stalling_duration > 0], collapse=" s, "), "s \n\tfor segments", paste(playout$segmentindex[playout$early_or_late > 0], collapse=", ")), ""), "\n\n")
		}
		splitdata[[p]] = playout
	}
	return(do.call(rbind, splitdata))
}

actual_stallings <- function(data, framedata, init_data, expected_max_frame_interval=50, verbose=T, summaryprint=T) {
	data$actual_stalling = 0
	data$actual_stalling_duration = 0
	data$actual_stalling_frame = 0
	splitdata = fix.factors(split(framedata, framedata$starttimestamp))
	#splitdata = split(data, data$starttimestamp)
	if (verbose) {
		#cat(levels(data$starttimestamp), "\n")
		#str(levels(init_data$starttimestamp))
		#str(data$starttimestamp)
		cat("Split data based on", levels(framedata$starttimestamp), "into list of length", length(splitdata), "\n")
		#str(splitdata)
	}
	for (p in seq(1, length(splitdata))) {
		playout = splitdata[[p]]
		if (verbose) {
			#str(playout)
			cat("Looking for initdata starting at", as.character(levels(playout$starttimestamp)), "\n")
			#str(subset(init_data, starttimestamp == levels(playout$starttimestamp)))
		}
		i1 = fix.factors(subset(init_data, starttimestamp == levels(playout$starttimestamp)))
		# Start timestamp of the actual playout
		start_playout_time = i1$timestamp_start_playout[1]
		if (verbose) {
			cat("Playout started at", start_playout_time, "for", length(playout$frame), "frames\n")
		}
		to_subtract = median(playout$drawn_diff[playout$drawn_diff < expected_max_frame_interval])
		playout$drawn_diff = playout$drawn_diff - to_subtract
		playout = subset(playout, playout$timestamp_drawn > start_playout_time + expected_max_frame_interval)
		stallings = subset(playout, playout$drawn_diff > expected_max_frame_interval)
        if (nrow(stallings) == 0) {
            if (summaryprint) {
                dataitem = fix.factors(subset(data, starttimestamp == levels(playout$starttimestamp)))
                cat("Actual stallings for playout starting at", as.character(levels(playout$starttimestamp)), "(ABR:", as.character(levels(dataitem$abr)), ", policy:", as.character(levels(dataitem$policy)), ")\n\tNone\n\n")
            }
            next
        }
		stallings$drawn_diff = stallings$drawn_diff / 1000
		stalled_segments = c()
		#str(stallings)
		for (i in seq(1, nrow(stallings))) {
			s = stallings[i,]
			#str(s)
			segments_loaded_before = fix.factors(subset(data, s$timestamp_drawn > data$timestamp_end))
			segment_before_which = fix.factors(subset(segments_loaded_before, segments_loaded_before$timestamp_end == max(segments_loaded_before$timestamp_end)))
			if (verbose) {
				cat("Looking for last segment finished downloading before render time", s$timestamp_drawn, "\n")
				#str(segments_loaded_before)
				#str(segment_before_which)
				cat("Segment", segment_before_which$segmentindex, "stalled frame", s$frame, "(loaded at", segment_before_which$timestamp_end, "which is", s$timestamp_drawn - segment_before_which$timestamp_end, "ms before frame was drawn)\n\tstalling duration:", s$drawn_diff, "ms\n")
				#cat("Stalling", s$frame, "happened for segment", segment_before_which$segmentindex, "(loaded at", segment_before_which$timestamp_end, "which is", s$timestamp_drawn - segment_before_which$timestamp_end, "ms before frame was drawn)\n")
			}

			dataindex = which (data$starttimestamp == levels(playout$starttimestamp)[1] & data$segmentindex == segment_before_which$segmentindex)
            if (verbose) {
				cat("dataindex for starttimestamp", as.character(playout$starttimestamp[1]), "before segment", segment_before_which$segmentindex, ":", dataindex, "\n")
                str(fix.factors(subset(data, data$starttimestamp == levels(playout$starttimestamp)[1])))
            }
            if (nrow(data[dataindex,]) == 0) {
                if (verbose) {
                    cat("\tDid not find a valid dataindex -- skipping\n")
                }
                next
            }
			data[dataindex,]$actual_stalling = 1
			data[dataindex,]$actual_stalling_duration = s$drawn_diff
			data[dataindex,]$actual_stalling_frame = s$frame
			stalled_segments = c(stalled_segments, segment_before_which$segmentindex)

		}
		if (summaryprint) {
			cat("\nActual stallings for playout starting at", as.character(levels(segment_before_which$starttimestamp)), "(ABR:", as.character(levels(segment_before_which$abr)), ", policy:", as.character(levels(segment_before_which$policy)), "):\n\t", nrow(stallings), "stallings", ifelse(nrow(stallings) > 0, paste("of durations", paste(stallings$drawn_diff, collapse=" s, "), "s\n\tfor segments ", paste(stalled_segments, collapse=", "), "\n\tbefore frames", paste(stallings$frame, collapse=", "), "\n\tdecoded at times", paste(stallings$timestamp_drawn, collapse=", ")), ""), "\n\n")
		}
	}
	return(data)
}

segment_duration <- function(framedata, verbose=T) {
	splitframedata = fix.factors(split(framedata, framedata$starttimestamp))
	for (framedata in splitframedata) {
		cat(as.character(levels(framedata$starttimestamp)), "\n")
		for (startframe in seq(2, max(as.integer(framedata$frame)), 96)) {
			frame1 = subset(framedata, frame == startframe)
			#str(frame1)
			frame2 = subset(framedata, frame == (startframe + 95))
			#str(frame2)
			cat("Frame", startframe, "-", startframe + 95, ":  \t", frame2$timestamp_drawn - frame1$timestamp_drawn, "ms \t(", frame1$timestamp_drawn, "..", frame2$timestamp_drawn, ")\n")
			if (verbose) {
				frames = subset(framedata, frame >= startframe & frame <= (startframe + 95))
				#str(frames)
				cat(frames$drawn_diff[2:length(frames$drawn_diff)], "\n")
				cat(cumsum(frames$drawn_diff[2:length(frames$drawn_diff)]), "\n")
			}
		}
		cat("\n")
	}
}

add_mam_readings_to_policydata <-function(policydata, mamdata, downloaddata, interfaces=c("eth0", "wlan0"), metrics=c("segmentindex", "min_srtt", "median_srtt", "download_max_rate", "download_rate")) {

    str(policydata)
    for (m in metrics) {
        policydata[[m]] = NA
        if (!is.null(mamdata$ifdata[[1]][[m]])) {
            cat("Getting mamdata (interface)\n")
            thismamdata = mamdata$ifdata
            xmetrics="timestamp"
            str(thismamdata[[1]][[xmetrics]])
        } else if (!is.null(mamdata$pfdata[[1]][[m]])) {
            cat("Getting mamdata (prefix)\n")
            thismamdata = mamdata$pfdata
            xmetrics="timestamp"
            str(thismamdata[[1]][[xmetrics]])
        } else if (!is.null(downloaddata[[m]])) {
            cat("Getting downloaddata\n")
            thismamdata = downloaddata
            xmetrics="timestamp_start"
            str(thismamdata[[xmetrics]])
        } else {
            cat("Did not find metrics", m, "--skip\n")
            next
        }

        #for (i in seq(1, nrow(policydata))) {
        for (i in seq(19, 20)) {
            if(print) {
                if (m == "segmentindex") {
                    policytimestamp = policydata[i,]$timestamp * 1000
                } else {
                    policytimestamp = policydata[i,]$timestamp * 1000
                }
                cat("Policy decision", i, "at", policytimestamp, "\n")
                #str(thismamdata[[xmetrics]])

                databeforethis = gettimewindow(thismamdata, xmin=0, xmax=policytimestamp)
                cat("Data before this\n")
                str(databeforethis)
            }
            str(names(databeforethis))
            names(databeforethis) = sapply(names(databeforethis), substitute_addresses_by_interfaces)
            str(names(databeforethis))
            for (interf in interfaces) {
                dataforthisinterface = databeforethis[[interf %in% names(databeforethis)]]
                cat("Adding policydata for", interf, "-", tail(dataforthisinterface)[1][[m]], "\n")
                policydata[i,][[paste(m, interf, sep="_")]] = tail(dataforthisinterface)[1][[m]]
            }
            #policydata[i,][[m]] = tail(subset(thismamdata, thismamdata[[xmetrics]] < policydata[i,]$timestamp), n=1)[1][[m]]
        }
    }
    str(policydata)
}

read_cqm_runs <- function(logprefix="cqm-data/bbb_bus/data/", runs=list.files(path=logprefix, pattern="^run*"), filename="cqm_vals.csv", print=FALSE) {
	qoedata = data.frame()
	qoelist = list()

	for (i in 1:length(runs)) {
		run = runs[i]
		cat("  [", i, "/", length(runs), "] Reading in data from", run, "\n")
		data = read_cqmdata(paste(logprefix, run, "/json/", sep=""), filename=filename, print=print)
		if (is.null(data)) {
			next
		}
		gsub("run-", "", run)
		datematch = regexpr("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}-",run)
		dateofrun = substring(run, datematch, datematch+attr(datematch, "match.length")-2)
		data$run = as.factor(run)
		#data$methodology = as.factor(get_methodology_label(run))
		#data$workload = as.factor(get_workload(run))
		gsub(dateofrun, "", run)
		#scenario = as.factor(substring(run, datematch+attr(datematch, "match.length")))
		if (print) {
			cat("  Dateofrun:", dateofrun, "scenario:", scenario, "\n")
		}
		#data$scenario = scenario

		qoelist[[i]] = data
	}
	qoelistlength1 = length(qoelist[[1]])
	if (all(lapply(qoelist, function(x) length(x)) == qoelistlength1)) {
		# All list items are similar - we can combine them to a single data frame
		qoedata = do.call(rbind, qoelist)
	} else {
		qoelist = qoelist[vapply(qoelist, Negate(is.null), NA)]
		cat("Warning: Lengths of data items do not match (different clients?)\n")
		cat("Some columns were filled with NAs.\n")
		qoedata = do.call(rbind, filldataframes(qoelist))
	}

#	if (any(grepl("MAM", runs))) {
#		mam = sapply(qoedata$scenario, function(x) {matchobj=regexpr("[A-Z]+", x); return(ifelse(attr(matchobj, "match.length") > 2 , substring(x, matchobj, matchobj+attr(matchobj, "match.length")-1), "MAM"))})
#		qoedata$mam = factor(mam)
#		qoedata$qoe = qoedata$iface:qoedata$qoe:qoedata$mam
#	}
	qoedata$scenario = as.factor(qoedata$scenario)
	if (print) {
		str(qoedata)
	}
	return(qoedata)
}

read_cqmdata <- function(logprefix=list.files(path="data/", pattern="run-*")[1], filename="qoe.log", print=TRUE) {
	qoedata = data.frame()
	qoelist = list()

	colnames_to_use=c("workload", "scenario", "starttimestamp", "qoe")
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
        cf = count.fields(paste(logprefix, files[i], sep=""), sep=",")
		tdata = read.csv(paste(logprefix,files[i], sep=""), header=FALSE)
		colnames(tdata) = colnames_to_use
		playlengths = rle(as.integer(tdata$starttimestamp))$lengths
		#cat("Compute second for timestamps", levels(tdata$starttimestamp), ":", playlengths, "\n")
		#cat("Is all", max(playlengths), "?", all(playlengths) == max(playlengths), "\n")
		if (all(playlengths == max(playlengths))) {
			#cat("Yes!\n")
			tdata$second = rep(seq(1, max(playlengths)), length(levels(tdata$starttimestamp)))
		} else {
			#cat("No!\n")
			tdata$second = unlist(sapply(playlengths, function(x) { seq(1, x) }))
		}
		#cat("done\n")
		#str(tdata$second)
		#cat("scenario before:", as.character(tdata$scenario[1]), "\n")
		tdata$scenario = strsplit(as.character(tdata$scenario[1]), "videotest_")[[1]][2]
		#cat("scenario after:", as.character(tdata$scenario[2]), "\n")
		#str(tdata)

		qoelist[[i]] = tdata
	}
	qoedata = do.call(rbind, qoelist)

	if (print) {
		str(qoedata)
	}
	return(qoedata)
}

last_cqm_qoe <- function(cqmdata, abrdata = NULL, initdata = NULL) {
	newdata = data.frame(starttimestamp = levels(cqmdata$starttimestamp))
	maxsecond = tapply(cqmdata$second, INDEX=cqmdata$starttimestamp, FUN=max)
	newdata$workload = cqmdata$workload[1]
	newdata$run = unlist(sapply(levels(cqmdata$starttimestamp), function(x) { subset(cqmdata, starttimestamp == x)[1,]$run }))
	newdata$qoe = sapply(levels(cqmdata$starttimestamp), function(x) { subset(cqmdata, starttimestamp == x & second == maxsecond[x])$qoe })
	if (! is.null(abrdata)) {
		newdata$abr = sapply(levels(cqmdata$starttimestamp), function(x) { subset(abrdata, starttimestamp == x)[1,]$abr })
		newdata$policy = sapply(levels(cqmdata$starttimestamp), function(x) { subset(abrdata, starttimestamp == x)[1,]$policy })
		newdata$scenario = sapply(levels(cqmdata$starttimestamp), function(x) { subset(abrdata, starttimestamp == x)[1,]$scenario })
	}
	return(newdata)
}
