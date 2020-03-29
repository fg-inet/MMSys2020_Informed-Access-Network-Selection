#!/usr/bin/env python3
#
# Author: Theresa Enghardt (theresa@inet.tu-berlin.de)
# 2019
#
# Read JSON file as generated by p_1203, dump relevant information to be used as input to CQM model
# P.1203: https://github.com/itu-p1203/itu-p1203
# CQM:    https://github.com/TranHuyen1191/CQM
#
# Usage:
#		   ./dump_json.py RUNFILTER
#				   RUNFILTER:  every run which contains this string will be considered (default: consider all runs)
#

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
import subprocess

RUNDIR = "data/"

CQM_INPUT_FILENAME_INID="cqm_inid.csv"
CQM_INPUT_FILENAME_SQARR="cqm_sqarr.csv"
CQM_INPUT_FILENAME_SPARR="cqm_sparr.csv"
CQM_INPUT_FILENAME_SDARR="cqm_sdarr.csv"

CQM_INPUT_FILES = [ CQM_INPUT_FILENAME_INID, CQM_INPUT_FILENAME_SQARR, CQM_INPUT_FILENAME_SPARR, CQM_INPUT_FILENAME_SDARR ]

cqm_input_inid = [ "workload", "scenario", "starttimestamp", "abr", "policy", "inid"]
cqm_input_sqarr = [ "workload", "scenario", "starttimestamp", "abr", "policy", "sqarr"]
cqm_input_sparr = [ "workload", "scenario", "starttimestamp", "abr", "policy", "sparr"]
cqm_input_sdarr = [ "workload", "scenario", "starttimestamp", "abr", "policy", "sdarr"]

qoedata_fields = [ "workload", "scenario", "starttimestamp", "abr", "policy", "qoe" ]


data_fields = [ "workload", "scenario", "starttimestamp", "segmentindex", "abr", "representation", "bufferlevel", "dl_rate_prev", "max_buffer", "client", "starttimestamp_numeric", "run", "policy", "timestamp_start", "timestamp_end", "downloadtime", "early_or_late", "supposed_to_be_there", "stalling", "stalling_duration", "actual_stalling", "actual_stalling_duration", "actual_stalling_frame", "page" ]
initdata_fields = [ "workload", "policy", "scenario", "starttimestamp", "timestamp_first_loc", "url", "timestamp_terminal_loaded", "timestamp_start_mpd_load", "timestamp_start_initsegs", "timestamp_end_initsegs", "timestamp_player_initialized", "timestamp_start_playout" ]

DATA_FILENAME="abrdata_with_stallings.log"
INITDATA_FILENAME="initial_playout.log"

CAP_CONTENT_TO = 240
TOTAL_DURATION = 240



def createDirectory(path):
	logging.debug("Trying to create directory " + path)
	try:
		os.makedirs(path)
	except OSError as exception:
		if exception.errno != errno.EEXIST:
			raise

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

def filter_timings(timings, values, key="page"):
	if timings is None:
		return None
	if isinstance(values, str):
		values = [ values ]
	#logging.debug("Filtering " + str(timings) + " by " + str(key) + ": " + str(values))

	filtered_timings = []
	for t in timings:
		if key:
			compare_item = t[key]
		else:
			compare_item = t
		if any(v in compare_item for v in values):
			filtered_timings.append(t)
	#logging.debug("\n\t\tResult: " + str(filtered_timings))
	logging.debug("Filtered for " + str(key) + ": " + str(len(filtered_timings)) + "/" + str(len(timings)) + "\n")
	return filtered_timings

# Read all data for a run, return as a list of dicts
def read_abr_log_with_stallings(run):
	datalogfilename = run + DATA_FILENAME
	data = read_csvfile(datalogfilename, data_fields)

	logging.debug("Data: " + str(data))
	return data

# Read all data for a run, return as a list of dicts
def read_init_data(run):
	datalogfilename = run + INITDATA_FILENAME
	data = read_csvfile(datalogfilename, initdata_fields)

	logging.debug("Initdata: " + str(data))
	return data

def output_cqm(run, data, initdata, log=False):
	#log_json = log
	log_csv = log
	#QOE_LOG_FILENAME="qoe" + ("_" + str(CAP_CONTENT_TO) + "s" if CAP_CONTENT_TO < TOTAL_DURATION else "") + ".log"
	csvwriters = {}
	csvfiles = {}

	if log:
		# Logfile for timings and per-page statistics
		for filename in CQM_INPUT_FILES:
			try:
				if os.path.exists(run + "json/" + filename):
					os.remove(run + "json/" + filename)
					logging.info("Deleted old " + run + "json/" + filename)
			except Exception as err:
				logging.warn("Could not delete " + run + "json/" + filename + ": " + str(err))
			if log_csv:
				try:
					csvfile = open(run + "json/" + filename, "w", newline='')
				except TypeError as e:
					logging.warn("Error opening " + run + "json/" + filename + ": " + str(e))
					csvfile = open(run + "json/" + filename, 'wb')
				csvwriters[filename] = csv.writer(csvfile, delimiter=",")
				csvfiles[filename] = csvfile

	starttimes = list(set( [ d["starttimestamp"] for d in data ] ))
	starttimes = sorted(starttimes)
	for startt in starttimes:
		thisdata = filter_timings(data, startt, "starttimestamp")
		jsonfile = run + "json/" + thisdata[0]["abr"] + "_" + thisdata[0]["policy"] + "_" + startt + ("_" + str(CAP_CONTENT_TO) + "s" if CAP_CONTENT_TO < TOTAL_DURATION else "") + ".json"
		logging.info("Reading " + jsonfile)
		try:
			jsonfile_open = open(jsonfile, 'r')
			jsoncontent = json.loads(jsonfile_open.read())
		except Exception:
			logging.warn("Could not open " + jsonfile + " -- skipping")
			continue
		stallings = jsoncontent["I23"]["stalling"]
		inid = stallings[0][1] # Initial playout delay
		logging.info("Inid: " + str(inid))
		sparr = [ s[0] for s in stallings[1:] ] # Stalling positions
		sdarr = [ s[1] for s in stallings[1:] ] # Stalling durations
		logging.info("sparr: " + str(sparr))
		logging.info("sdarr: " + str(sdarr))

		process = subprocess.Popen("python3.7 -m itu_p1203 --accept-notice --only-pv " + jsonfile, shell=True, stdout = subprocess.PIPE, stderr=subprocess.DEVNULL)
		output, error = process.communicate()
		decoded_output = json.loads(str((output.decode('ascii'))))
		if error:
			logging.info("Got error: " + str(error))
		logging.debug("Got output: " + str(decoded_output))
		sqarr = decoded_output[ jsonfile ]['video']['O22']
		logging.info("sqarr: (" + str(len(sqarr)) + " values)")

		if log_csv:
			thisinitdata = filter_timings(initdata, startt, "starttimestamp")[0]
			#logging.debug("Logging inid to " + CQM_INPUT_FILENAME_INID + " using " + str(csvwriters[CQM_INPUT_FILENAME_INID]))
			#logging.debug(str(thisinitdata))

			baseinfo = [thisdata[0]["workload"], thisdata[0]["scenario"], thisdata[0]["starttimestamp"], thisdata[0]["abr"], thisinitdata["policy"]]
			logging.debug(baseinfo)

			csvwriters[CQM_INPUT_FILENAME_INID].writerow(baseinfo + [inid])
			csvwriters[CQM_INPUT_FILENAME_SQARR].writerows([ baseinfo + [ind+1] + [sq] for (ind, sq) in enumerate(sqarr) ])
			csvwriters[CQM_INPUT_FILENAME_SPARR].writerows([ baseinfo + [ind+1] + [sp] for (ind, sp) in enumerate(sparr) ])
			csvwriters[CQM_INPUT_FILENAME_SDARR].writerows([ baseinfo + [ind+1] + [sd] for (ind, sd) in enumerate(sdarr) ])

	if log_csv:
		print("Logged CSV files")
		for csvfile in csvfiles.values():
			csvfile.close()

# Process runs, dump jsons for each of them
def main(argv=[]):
	global CAP_CONTENT_TO
	runfilter=None
	logtofile = True
	if (len(argv) > 1):
		runfilter = argv[1]
	if (len(argv) > 2):
		CAP_CONTENT_TO = int(argv[2])
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

		#createDirectory(run + "json/")
		runlabel= list(filter(None, run.split('/')))[-1]
		plotlabel = runlabel

		# Assumes that dump_json.py has run before, so its output exists
		if not os.path.exists(run + "json"):
			print("No data for " + run + " -- next")
			continue

		# Get all Navigation Timings as list of dicts
		data = read_abr_log_with_stallings(run)
		initdata = read_init_data(run)

		print("Processing " + run + ( "... with CAP_CONTENT_TO " + str(CAP_CONTENT_TO) if CAP_CONTENT_TO < 240 else ""))
		output_cqm(run, data, initdata, log=logtofile)
		#extract_qoe(data, initdata, run, log=logtofile)

		if logtofile:
			print("!!! Logged " + run + "!!!")

if __name__ == "__main__":
	main(sys.argv)
