#!/usr/bin/Rscript


VIDEO_WORKLOAD_DATA_PATH = "data/video-workload"

load(paste(VIDEO_WORKLOAD_DATA_PATH, "workload_segmentsizes.Rdata", sep="/"))

source("videodata.R")
source("../../web-timing/eval/plottimings.R")


data = list(df_bbb2, df_rb2, df_v2)
names(data) = c("BBB", "RB", "V")
names(data[["BBB"]]) = c("Rep. 1 (218 kBit/s)", "Rep. 2 (378 kBit/s)", "Rep. 3 (509 kBit/s)", "Rep. 4 (783 kBit/s)", "Rep. 5 (1474 kBit/s)", "Rep. 6 (2087 kBit/s)", "Rep. 7 (3936 kBit/s)")
names(data[["RB"]]) = c("Rep. 1 (201 kBit/s)", "Rep. 2 (395 kBit/s)", "Rep. 3 (500 kBit/s)", "Rep. 4 (892 kBit/s)", "Rep. 5 (1498 kBit/s)", "Rep. 6 (1992 kBit/s)", "Rep. 7 (2996 kBit/s)")
names(data[["V"]]) = c("Rep. 1 (210 kBit/s)", "Rep. 2 (433 kBit/s)", "Rep. 3 (574 kBit/s)", "Rep. 4 (811 kBit/s)", "Rep. 5 (1422 kBit/s)", "Rep. 6 (1861 kBit/s)", "Rep. 7 (3523 kBit/s)")

max_value = max(unlist(lapply(data, function(x) get_xmax(x, "size")))) / 1000000


# overtime
for (workload in c("BBB", "RB", "V")) {
	plotovertime(data[[workload]], "size", filename=paste(VIDEO_WORKLOAD_DATA_PATH, paste(workload, "segmentsizes", sep="_"), sep="/"), plot="eps", plotlabel=workload, ymax=max_value)
}

# cdf

for (workload in c("BBB", "RB", "V")) {
	for (i in seq(1, length(data[[workload]]))) {
		data[[workload]][[i]][["size"]] = data[[workload]][[i]][["size"]] / 1000000
	}
	str(data[[workload]])
	prepare_file(plot="eps", filename=paste(VIDEO_WORKLOAD_DATA_PATH, paste(workload, "cdf", sep="_"), sep="/"), spacebelow=3.5, spaceleft=3.5)
	plot.cdf.list(data[[workload]], metrics="size", xlabel="Segment size [MB]", legendposition="bottomright", xlim=c(0, max_value), cex=1.3)
	dev.off()
}
