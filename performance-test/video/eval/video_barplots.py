#!/usr/bin/env python3
#
# Author: Theresa Enghardt (theresa@inet.tu-berlin.de)
# 2018
#
# Tools to compute and plot page load timings from Navigation Timings, Resource Timings, and HAR files as exported by webtimings.py
#
# Usage:
#           ./computetimings.py RUNFILTER WORKLOAD POLICY LOG_LEVEL
#                   RUNFILTER:  every run which contains this string will be considered (default: consider all runs)
#                   WORKLOAD:   supply multiple separated by comma, every page which contains one of these strings will be consider (default: "all")
#                   POLICY:     supply multiple separated by comma (default: "all")
#                   LOG_LEVEL:  set to "debug" or "info" to get more debug output

import os
import io
import errno
import datetime
import time
import sys 
import glob
import csv
import re
import logging
import json
import matplotlib

matplotlib.use('Agg')

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages
#import hartimings
import subprocess
import copy
#import validate_object_size


RUNDIR = "data/"

DATA_FILENAME="abrdata_with_stallings.log"
POLICYDATA_FILENAME="policy.log"

X_AXIS_LABEL_THRESHOLD = 40

PLOT_WIDTH=30
PLOT_HEIGHT=15

MAX_BARPLOTS_IN_ONE_PAGE = 20

EPSILON=0.00000001

SEGMENT_LENGTH = 4

# Fields of the CSV files
data_fields = [ "workload", "scenario", "starttimestamp", "segmentindex", "abr", "representation", "bufferlevel", "dl_rate_prev", "max_buffer", "client", "starttimestamp_numeric", "run", "policy", "timestamp_start", "timestamp_end", "downloadtime", "early_or_late", "supposed_to_be_there", "stalling", "stalling_duration", "actual_stalling", "actual_stalling_duration", "actual_stalling_frame", "page" ]
policydata_fields = [ "workload", "policy", "scenario", "starttimestamp", "policy_timestamp", "sockets_reuse_if1", "num_sockets_reuse_if1", "sockets_reuse_if2", "num_sockets_reuse_if2", "foo", "bar", "baz", "filesize", "category", "bitrate", "buffer_status", "choice", "reason"]

INTERFACE1 = "eth0"
INTERFACE2 = "wlan0"
interface_colors = { INTERFACE1: "yellow", INTERFACE2: "blue" , "mptcp": "lightgreen", "unknown": "grey" }
interface_colors_stalled = { INTERFACE1: "orange", INTERFACE2: "darkblue" , "mptcp": "darkgreen", "unknown": "darkgrey" }

POLICY_ORDER = { 'eth0': 0, 'wlan0': 1, 'wlan1': 2, 'video': 3, 'video_pessimist': 4, 'bypass_testbed': 1000}

hidelabels = ["start_delta"]
SET_PLOT_LABELS = True
BIGGER_X_LABEL = False
LABEL_FONTSIZE=40

def createDirectory(path):
	logging.debug("Trying to create directory " + path)
	try:
		os.makedirs(path)
	except OSError as exception:
		if exception.errno != errno.EEXIST:
			raise

def filter_timings(timings, values, key="page"):
	if timings is None:
		return None
	if isinstance(values, str):
		values = [ values ]
	logging.debug("Filtering " + str(timings) + " by " + str(key) + ": " + str(values))

	filtered_timings = []
	for t in timings:
		if key:
			compare_item = t[key]
		else:
			compare_item = t
		if any(v in compare_item for v in values):
			filtered_timings.append(t)
	logging.debug("\n\t\tResult: " + str(filtered_timings))
	logging.debug("Filtered for " + str(key) + ": " + str(len(filtered_timings)) + "/" + str(len(timings)) + "\n")
	return filtered_timings

def sort_list(origlist, by="scenario"):
	if origlist is None:
		return None

	# Filter for everything that is not None
	returnlist = [item for item in origlist if (item is not None and item[by] is not None)]

	if by == "policy":
		return sorted(returnlist, key=lambda k: POLICY_ORDER[k[by]])
	if any(v in by for v in ["time", "Time", "Start", "End", "dom", "duration"]):
		return sorted(returnlist, key=lambda k: float(k[by]))
	else:
		return sorted(returnlist, key=lambda k: k[by])

# Read a CSV file, return list of dicts of contents and the open file
def read_csvfile(csvfilename, fields):
	try:
		csvfile = open(csvfilename, 'r')
		csvreader = csv.DictReader(csvfile, fieldnames = fields)
		csvlist = list(csvreader)
		csvfile.close()
		return csvlist
	except Exception as e:
		print("Error with " + str(csvfilename) + ": " + str(e))
		return None


# Prepare a plot with a given width and height
def prepare_plot(plotlabel = "", xlabel = "Time", width=PLOT_WIDTH, height=PLOT_HEIGHT, xlim = [0, 2000], ylim = [-0.5, 1]):
	logging.debug("Preparing plot of width " + str(width) + " and height " + str(height) + ", ylim: " + str(ylim))
	fig, ax=plt.subplots(figsize=(width, height))
	if SET_PLOT_LABELS:
		ax.set_title(plotlabel)
	axes = plt.gca()
	axes.set_ylim(ylim)
	axes.set_xlim(xlim)
	if BIGGER_X_LABEL:
		ax.set_xlabel(xlabel, fontsize=LABEL_FONTSIZE)
		ax.set_ylabel("Resource", fontsize=LABEL_FONTSIZE)
		ax.tick_params(labelsize=LABEL_FONTSIZE)
	else:
		ax.set_xlabel(xlabel)
	ax.invert_yaxis()
	return (plt, ax)


# Show or store a plot and close it
def finish_plot(plt, plotlabel, plot="pdf", outputdir=RUNDIR + "plots/"):
	if plot == "terminal":
		plt.show()
		plt.close()
	else:
		plt.savefig(outputdir + "barplot_" + plotlabel + "." + plot)
		plt.clf()
		plt.cla()
		plt.close()
		print("Saved plot to " + outputdir + "barplot_" + plotlabel + "." + plot + "\n\n")

def draw_label(ax, patch, xytext, label, color):
	# Draw label for the first row
	xpos = patch.get_x()+ patch.get_width()
	ypos = patch.get_y()
	xypos = (xpos, ypos)
	if label != "":
		logging.debug("x=" + str(patch.get_x()) + ", y=" + str(ypos) + ", width=" + str(patch.get_width()) + " - Drawing label " + str(label) + " at " + str(xypos) + ", textpos=" + str(xytext) + "\n")
		ax.annotate(str(label), xy = xypos, xycoords="data", xytext = xytext, textcoords="data", color=color, arrowprops=dict(edgecolor=color, facecolor=color, width=0.5, shrink=0.005))


# Add one barplot to a plot that has been prepared
def add_barplot(ax, timings, barplot_base_x, timinglabels, colors = "yellow", xmax = None):
	number_of_timings = len(timings[0])
	barplots_at = list(map(lambda x, y: x + y, [barplot_base_x] * number_of_timings, list(range(number_of_timings))))
	logging.debug("Barplots at: " + str(barplots_at))

	xmin = 0
	if xmax is None:
		xmax = np.max(timings)

	labelstodraw = list(filter(None, timinglabels))
	labelstodraw = [l for l in labelstodraw if l not in hidelabels]
	logging.debug("Timinglabels to plot: " + str(labelstodraw))

	logging.debug("xmin = " + str(xmin) + ", xmax = " + str(xmax))
	logging.debug("Barplots at " + str(barplots_at))

	# Each list iteration corresponds to one additional patch stack on top of each of the barplots next to each other
	for (ind, t) in enumerate(timings):
		logging.debug("\t" + str(ind) + ": " + str(t) )
		prevtimings = ( timings[ind-1] if ind > 0 else [0] * number_of_timings)
		logging.debug("\tPrev: " + str(prevtimings) )
		timediff = tuple(map(lambda x, y: x - y, t, prevtimings))
		logging.debug("\t" + str(ind) + ": " + str(t))
		logging.debug("duration until event: " + str(timediff))
		edgecolor="black"
		try:
			colortodraw = colors[ind]
			logging.debug(" (" + str(colors[ind]) + ")")
		except IndexError:
			colortodraw = "white"
		if colortodraw[0] == "white":
			edgecolor="white"

		# Draw a stacked barplot patch from the previous timing value up to the current timing value
		ps = ax.barh(barplots_at, timediff, left= prevtimings, align='edge', height=(0.5 if SET_PLOT_LABELS else 0.9), color=colortodraw, edgecolor=edgecolor)

		#if timinglabels[ind] != "":
		#	if timinglabels[ind] in labelstodraw:
		#		logging.debug("Drawing labels for " + str(timinglabels[ind]))
		#		# Draw label for the first row
		#		# Calculate position of label text along the x axis
		#		# Labels need to be spaced out across the width of the plot. E.g., if there are 7 labels, label 1 is at 1/7 of the x axis
		#		labeltext_x = labelstodraw.index(timinglabels[ind])/len(labelstodraw) * (xmax - xmin)

		#		logging.debug("Calculating position of label " + timinglabels[ind] + ": y (line) = " + str(barplots_at[0] - 0.25) + ", x (left_right) = " + str(labeltext_x) + " (based on index " + str(labelstodraw.index(timinglabels[ind])) + " in list of length " + str(len(labelstodraw)) + ", (xmax - xmin) = " + str(xmax) + " - " + str(xmin))
		#		draw_label(ax, ps.patches[0], (labeltext_x, barplots_at[0] - 0.25), timinglabels[ind], "black")
		#		for (patch_ind, patch) in enumerate(ps.patches[1:]):
		#			logging.debug("patch index " + str(patch_ind) + ", index " + str(ind) )
		#			draw_label(ax, patch, (labeltext_x, barplots_at[patch_ind+1] - 0.25), timinglabels[ind], "#111111")
		#	else:
		#		logging.debug("Not drawing label " + timinglabels[ind])

def plot_ylabels(ax, ylabelslist, offset=0):
	yticks = []
	ylabels = []
	# If we are plotting to offset > 0, we printed ylabels before - preserve them
	if (offset > 0):
		yticks.extend(ax.get_yticks())
		ylabels.extend([ t.get_text() for t in ax.get_yticklabels() ])
	if ylabelslist is not None:
		new_yticklabels = [ item if item is not None else "" for item in ylabelslist ]
		barplot_positions = range(len(new_yticklabels))
		ylabels.extend(new_yticklabels)
		yticks.extend(list(map(lambda x: offset + x + 0.25, barplot_positions)))
		if ylabels is not None and len(ylabels) > 0:
			logging.debug("Trying to set ylabels " + str(ylabels) + " at " + str(yticks))
			ax.set_yticks(yticks)
			ax.set_yticklabels(ylabels)

# Produce a plot containing multiple barplots below each other, each with its timingslist, timinglabels, and colors
# timingslist: contains lists where each list corresponds to one barplot
# The timinglabelslist, colorslist, and ylabelslist item at the same index corresponds to the same barplot
def plot_barplots(starttimestamp, timingslist, timinglabelslist, colorslist, plotlabel = "", xlabel="Time [s]", ylabelslist = None, plot="pdf", outputdir=RUNDIR + "plots/"):

	if not BIGGER_X_LABEL and (starttimestamp > 0):
		xlabel = xlabel + " relative to " + str(starttimestamp)

	number_of_barplots = [ len(t[0]) for t in timingslist ] 
	logging.debug("Number_of_barplots: " + str(number_of_barplots))
	totallength = sum(number_of_barplots)

	xmax = np.max([np.max(sublist) for sublist in timingslist])
	if isinstance(xmax, list):
		xmax = np.max(xmax)
	logging.debug("xmax of " + str(timingslist) + ": \n\t" + str(xmax))

	multipage_pdf = None

	if totallength > MAX_BARPLOTS_IN_ONE_PAGE:
		print("Got " + str(totallength) + " barplots, plotting " + str(MAX_BARPLOTS_IN_ONE_PAGE) + " (+-1) per page")

		multipage_pdf = PdfPages(outputdir + "barplot_" + plotlabel + "." + plot)
		(plt, ax) = prepare_plot(plotlabel, xlabel, xlim = [0, xmax], ylim = [-0.5, MAX_BARPLOTS_IN_ONE_PAGE+1])
	else:
		(plt, ax) = prepare_plot(plotlabel, xlabel, xlim = [0, xmax], ylim = [-0.5, totallength])

	barplots_in_this_page = 0
	second_ax = ax.twinx()
	second_ax.set_ylim(ax.get_ylim())

	for (ind, timings) in enumerate(timingslist):
		logging.debug("ind " + str(ind))
		logging.debug(", timinglabellist[ind]" + str(timinglabelslist[ind]))
		logging.debug(", colorslist[ind]" + str(colorslist[ind]))
		add_barplot(ax, timings=timings, barplot_base_x=barplots_in_this_page, timinglabels=timinglabelslist[ind], colors=colorslist[ind], xmax=xmax)
		if not SET_PLOT_LABELS:
			ylabelslist[ind] = ['' * len(ylabelslist[ind])]
		plot_ylabels(ax, ylabelslist[ind], offset=barplots_in_this_page)
		resourcestarttimes = {}
		for ti in timings:
			for objectindex in range(0, len(ti)):
				if ti[objectindex] == 0:
					resourcestarttimes[objectindex] = 0
				try:
					if ti[objectindex] > 0 and resourcestarttimes[objectindex] == 0:
						resourcestarttimes[objectindex] = ti[objectindex]
				except KeyError:
					resourcestarttimes[objectindex] = ti[objectindex]
		if SET_PLOT_LABELS:
			plotylabelssecondax = [str(round(entry[0], 3)) + " - " + str(entry[1]) + "\n =" + str(round(entry[1] - entry[0], 3)) for entry in zip(resourcestarttimes.values(), timings[-1])]
		else:
			plotylabelssecondax = ['' for entry in zip(resourcestarttimes.values(), timings[-1])]
		logging.debug(str(plotylabelssecondax) + "\n")
		plot_ylabels(second_ax, plotylabelssecondax, offset=barplots_in_this_page)
		barplots_in_this_page += len(timings[0])
		logging.debug("Barplots in this page now: " + str(barplots_in_this_page))

		# Check if we want to save the figure so far to a page
		if multipage_pdf and barplots_in_this_page >= MAX_BARPLOTS_IN_ONE_PAGE:
			logging.debug("Closing PDF page after " + str(barplots_in_this_page) + " plots")
			multipage_pdf.savefig()
			plt.clf()
			plt.cla()
			plt.close()
			barplots_in_this_page = 0
			if ind < len(timingslist) - 1:
				# There is more to print - Prepare a new PDF page
				(plt, ax) = prepare_plot(plotlabel, xlabel, xlim = [0, xmax], ylim = [-0.5, MAX_BARPLOTS_IN_ONE_PAGE])
				second_ax = ax.twinx()
				second_ax.set_ylim(ax.get_ylim())

	if not multipage_pdf:
		finish_plot(plt, plotlabel, plot, outputdir)
	else:
		if barplots_in_this_page > 0:
			# Save this figure
			multipage_pdf.savefig()
			plt.clf()
			plt.cla()
			plt.close()
		multipage_pdf.close()
		print("Saved plot to " + outputdir + "barplot_" + plotlabel + "." + plot + "\n\n")


def policytimestamp_in_download(p, res):
	in_it = False
	try:
		if float(p["policy_timestamp"]) > float(res["timestamp_start"]) / 1000 and float(p["policy_timestamp"]) < float(res["timestamp_end"]) / 1000:
			return True
	except ValueError:
		return False

# Read a list of timings and produce one page worth of barplotdata
def timings_to_barplotdata_page(timings, policydata):
	do_interface_color_hack=False
	colors = []
	alltimediffs = []
	names = []

	just_append = False
	firsttimestamp = float(timings[0]["timestamp_start"])

	starttimes = []
	durations = []
	for res in timings:
		resource_name = "segment " + res["segmentindex"] + "\nstart at " + str(float(res["timestamp_start"]) / 1000)
		#logging.debug("Processing " + resource_name + "...")

		res_starttime = (float(res["timestamp_start"]) - firsttimestamp) / 1000
		res_duration = float(res["downloadtime"])
		#logging.debug("Started after " + str(res_starttime) + " and took " + str(res_duration))
		#alltimediffs.append([res_starttime, res_starttime + res_duration])
		#colors.append("white")
		starttimes.append(res_starttime)
		durations.append(res_starttime + res_duration)
		colormap_to_use = interface_colors
		if (res["stalling"] == "1"):
			# this segment stalled!
			colormap_to_use = interface_colors_stalled
		if type(policydata) == str:
			try:
				colors.append(colormap_to_use[policydata])
			except KeyError:
				colors.append(colormap_to_use["unknown"])
		else:
			logging.debug("Looking for timestamp " + str(res["timestamp_start"]))
			logging.debug("\t\tin " + str([p["policy_timestamp"] for p in policydata]))
			policytimes = [ p for p in policydata if policytimestamp_in_download(p, res)]
			logging.debug("Policytimes: " + str(policytimes))
			try:
				colors.append(colormap_to_use[policytimes[0]["choice"]])
				resource_name += "\nbitrate = " + policytimes[0]["bitrate"]
			except:
				colors.append(colormap_to_use["unknown"])
		#logging.debug("Starttimes now: " + str(starttimes))
		#logging.debug("Durations now: " + str(durations))
		#logging.debug("Names now: " + str(names))
		alltimediffs = [starttimes, durations]
		names.append(resource_name)
	timelabels=[0,1]
	colors = [ len(names) * ["white"] ] + [colors]
	logging.debug("Colors now: " + str(colors))
	logging.debug("All timediffs: " + str(alltimediffs))

	logging.debug(colors)
	return (alltimediffs, timelabels, names, colors)

def timings_to_barplotdata(timings, policydata):
	total_resources = len(timings)
	logging.debug("Got " + str(total_resources) + " timings")

	if total_resources <= MAX_BARPLOTS_IN_ONE_PAGE:
		try:
			(timediffs, timelabels, names, colors) = timings_to_barplotdata_page(timings, policydata)
			timediffs = [timediffs]
			timelabels = [timelabels]
			names = [names]
			colors = [colors]
		except Exception as e:
			print("Error getting timings_to_barplotdata for this page: " + str(e) + "\n\n")
			return ([0], [""], [""], ["#FFFFFF"])
	else:
		timediffs = []
		timelabels = []
		names = []
		colors = []
		index_a = 0
		index_b = MAX_BARPLOTS_IN_ONE_PAGE
		while index_a < total_resources:
			logging.debug("index_a: " + str(index_a) + ", index_b: " + str(index_b))
			#logging.debug(str(timings))

			# get barplotdata for next page, append it to list
			(page_timediffs, page_timelabels, page_names, page_colors) = timings_to_barplotdata_page(timings[index_a:index_b], policydata)
			timediffs.append(page_timediffs)
			timelabels.append(page_timelabels)
			names.append(page_names)
			colors.append(page_colors)

			index_a += MAX_BARPLOTS_IN_ONE_PAGE
			index_b += MAX_BARPLOTS_IN_ONE_PAGE

	return (timediffs, timelabels, names, colors)

# Parse multiple navigation timings and plot them all into one barplot
def plot_videotimings(timings, policydata, run, plotlabel):

	print("\tPlotting Video Timings for " + plotlabel + "...")

	timingslist = []
	timinglabelslist = []
	ylabelslist = []
	colorslist = []

	data = timings[0]
	segment = data["segmentindex"]
	pagelabel = str(data["starttimestamp"]) + "\n(" + data["abr"] + ")\n" + data["policy"]
	logging.debug("Processing " + pagelabel.replace("\n", " ") + "...")

	(new_timediffs, new_timelabels, new_names, new_colors) = timings_to_barplotdata(timings, policydata)

	logging.debug("Got newtimediffs: " + str(new_timediffs))
	logging.debug("Got colorslist: " + str(new_colors))
	logging.debug("Got ylabelslist: " + str(new_names))
	timingslist = new_timediffs
	#timingslist.append(new_timediffs)
	timinglabelslist.extend(new_timelabels)
	colorslist.extend(new_colors)
	ylabelslist.extend(new_names)

	logging.debug("Timings: " + str(timingslist) + "\nTiminglabels: " + str(timinglabelslist) + "\nYlabels: " + str(ylabelslist) + "\nColors: " + str(colorslist))
	plot_barplots(0, timingslist = timingslist, timinglabelslist = timinglabelslist, ylabelslist = ylabelslist, colorslist = colorslist, plotlabel = plotlabel, outputdir = run + "barplots/")


# Read all data for a run, return as a list of dicts
def read_abr_log_with_stallings(run):
	datalogfilename = run + DATA_FILENAME
	data = read_csvfile(datalogfilename, data_fields)

	logging.debug("Data: " + str(data))
	return data

def read_policydata(run):
	datalogfilename = run + POLICYDATA_FILENAME
	data = read_csvfile(datalogfilename, policydata_fields)

	logging.debug("Policydata: " + str(data))
	return data


# Process runs, call plotting functions on each of them
def main(argv=[]):
	runfilter=None
	logtofile = True
	policyfilter = None
	if (len(argv) > 1):
		runfilter = argv[1]
	if (len(argv) > 2):
		policyfilter = argv[2]
	if (len(argv) > 3):
		print("Trying to set log level to " + argv[3])
		root = logging.getLogger()
		if "debug" in argv[3]:
			root.setLevel(logging.DEBUG)
			logging.debug("Log level: Debug")
		elif "info" in argv[3]:
			root.setLevel(logging.INFO)
			logging.info("Log level: Info")

	print("Getting runs in " + RUNDIR + "run-*")
	runs = glob.glob(RUNDIR + "run-*")
	if runfilter is not None:
		runs = [ r for r in runs if runfilter in r ]
	print("Runs: " + str(runs) + "\n")
	for run in runs:
		if run[-1] != "/":
			run = run + "/"

		print("Processing " + run + '...')
		createDirectory(run + "barplots/")
		runlabel= list(filter(None, run.split('/')))[-1]

		# Get all Video timings as list of dicts
		data = read_abr_log_with_stallings(run)
		if data is None:
			print("No data for " + run + " -- skipping")
			continue

		if policyfilter:
			data = filter_timings(data, policyfilter, "policy")

		policydata = read_policydata(run)

		starttimes = list(set( [ d["starttimestamp"] for d in data ] ))
		starttimes = sorted(starttimes)
		for startt in starttimes:
			thisdata = filter_timings(data, startt, "starttimestamp")
			if thisdata[0]["policy"] == "eth0":
				thispolicydata = "eth0"
			elif thisdata[0]["policy"] == "wlan0":
				thispolicydata = "wlan0"
			elif thisdata[0]["policy"] == "mptcp":
				thispolicydata = "mptcp"
			else:
				thispolicydata = filter_timings(policydata, startt, "starttimestamp")

			plotlabel = thisdata[0]["policy"] + "_" + thisdata[0]["abr"] + startt

			plot_videotimings(thisdata, thispolicydata, run, plotlabel)

if __name__ == "__main__":
	main(sys.argv)
