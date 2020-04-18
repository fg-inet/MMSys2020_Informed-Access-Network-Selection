#!/usr/bin/env python3
#
# Author: Theresa Enghardt (theresa@inet.tu-berlin.de)
# 2019
#
# Dump stallings and video segment data to JSON file
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

CAP_CONTENT_TO=240
CODEC="h264"
FPS=24
DURATION=4
TOTAL_DURATION=240

DATA_FILENAME="abrdata_with_stallings.log"
INITDATA_FILENAME="initial_playout.log"
QOE_LOG_FILENAME="qoe" + ("_" + str(CAP_CONTENT_TO) + "s" if CAP_CONTENT_TO < TOTAL_DURATION else "") + ".log"

OVERWRITE_JSON=True
OVERWRITE_CSV=True


data_fields = [ "workload", "scenario", "starttimestamp", "segmentindex", "abr", "representation", "bufferlevel", "dl_rate_prev", "max_buffer", "client", "starttimestamp_numeric", "run", "policy", "timestamp_start", "timestamp_end", "downloadtime", "early_or_late", "supposed_to_be_there", "stalling", "stalling_duration", "actual_stalling", "actual_stalling_duration", "actual_stalling_frame", "page" ]
initdata_fields = [ "workload", "policy", "scenario", "starttimestamp", "timestamp_first_loc", "url", "timestamp_terminal_loaded", "timestamp_start_mpd_load", "timestamp_start_initsegs", "timestamp_end_initsegs", "timestamp_player_initialized", "timestamp_start_playout" ]


# Red bull 4s
resolutions = { "RedBull": [ "480x360", "480x360", "854x480", "854x480", "1280x720", "1280x720", "1920x1080" ],
	"BigBuckBunny": [ "480x360", "480x360", "854x480", "1280x720", "1280x720", "1920x1080", "1920x1080" ],
	"Valkaama": [ "480x360", "854x480", "854x480", "1280x720", "1280x720", "1440x1080", "1440x1080" ] }

bitrates = { "RedBull": [ 201.0, 395.0, 500.0, 892.0, 1498.2, 2465.8, 2995.8 ],
	"BigBuckBunny": [ 218.0, 378.0, 509.0, 783.0, 1473.8, 2087.3, 3936.3 ],
	"Valkaama": [ 210.0, 433.0, 574.0, 811.0, 1422.3, 1861.0, 3522.5 ] }

# This is for BBB 4s (all representations)
#resolutions = [ "320x240", "320x240", "320x240", "480x360", "480x360", "480x360", "480x360", "480x360", "854x480", "854x480", "1280x720", "1280x720", "1280x720", "1280x720", "1920x1080", "1920x1080", "1920x1080", "1920x1080", "1920x1080", "1920x1080" ]}
#bitrates = [ 45.0, 89.0, 129.0, 177.0, 218.0, 256.0, 323.0, 378.0, 509.0, 578.0, 783.0, 1008.7, 1207.2, 1473.8, 2087.3, 2409.7, 2944.3, 3340.5, 3613.8, 3936.3 ]					   

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

def output_json(data, initdata, run, printdata=True, log=False, overwrite=OVERWRITE_JSON):
	starttimes = list(set( [ d["starttimestamp"] for d in data ] ))
	starttimes = sorted(starttimes)
	duration_to_use = TOTAL_DURATION
	if CAP_CONTENT_TO < TOTAL_DURATION:
		duration_to_use = CAP_CONTENT_TO

	for startt in starttimes:
		thisdata = filter_timings(data, startt, "starttimestamp")
		thisinitdatafilter = filter_timings(initdata, startt, "starttimestamp")
		if len(thisdata) < 1 or len(thisinitdatafilter) < 1:
			logging.warn("No data for " + str(startt) + " -- skipping")
			continue
		thisinitdata = thisinitdatafilter[0]
		thisworkload = ( "BigBuckBunny" if "BigBuckBunny" in thisdata[0]["page"] else "RedBull" if "RedBull" in thisdata[0]["page"] else "Valkaama" if "Valkaama" in thisdata[0]["page"] else "unknown" )
		logging.debug("Processing data with starttime " + str(startt) + " (length " + str(len(thisdata)) + ")")
		logging.debug(thisdata[0])
		outputdict = { "I11" : { "segments": [ { "bitrate": 192, "codec": "aaclc", "duration": duration_to_use, "start": 0 } ] }, "I13": { "segments": [], "streamId": 1} , "IGen": { "displaySize": "1920x1080", "device": "pc", "viewingDistance": "150cm"}, "I23": { "streamId": 1, "stalling": [ [ 0, (float(thisinitdata["timestamp_start_playout"]) - float(thisinitdata["timestamp_start_mpd_load"]))/ 1000] ] }}
		if log:
			logfilename = run + "json/" + thisdata[0]["abr"] + "_" + thisdata[0]["policy"] + "_" + startt + ("_" + str(CAP_CONTENT_TO) + "s" if CAP_CONTENT_TO < TOTAL_DURATION else "") + ".json"
			logthisone = log
			try:
				if os.path.exists(logfilename):
					if overwrite:
						os.remove(logfilename)
						print("Deleted old " + logfilename)
					else:
						logthisone = False
			except Exception as err:
				print("Could not delete " + logfilename + ": " + str(err))
			if logthisone:
				try:
					jsonfile = open(logfilename, "w", newline='')
				except TypeError as e:
					print("Error opening " + logfilename + ": " + str(e))
					jsonfile = open(logfilename, 'wb')

		start = 0
		for segment in thisdata:
			representation = int(segment["representation"])
			logging.debug("\tSegment " + segment["segmentindex"] + " for workload " + str(thisworkload))
			logging.debug("\tSegment " + segment["segmentindex"] + ": rep = " + str(representation) + " (" + str(resolutions[thisworkload][representation]) + ", " + str(bitrates[thisworkload][representation]) + "kbit/s) - stalling? " + segment["stalling"])
			outputdict["I13"]["segments"].append( { "resolution": resolutions[thisworkload][representation], "start": start, "bitrate": bitrates[thisworkload][representation], "codec": CODEC, "fps": FPS, "duration": DURATION })

			if (segment["stalling"] == "1"):
				outputdict["I23"]["stalling"].append( [ start - 1/24, float(segment["stalling_duration"]) ])

			start += DURATION
			if start >= duration_to_use:
				logging.info("Break after " + str(start) + " seconds")
				break

		logging.debug(json.dumps(outputdict, sort_keys=True, indent=4))
		if log and logthisone:
			# log segments
			json.dump(outputdict, jsonfile, sort_keys=True, indent=4)

			jsonfile.close()
			print("Logged to " + logfilename)

def extract_qoe(data, initdata, run, log=False, overwrite_json=OVERWRITE_JSON, overwrite_csv=OVERWRITE_CSV):
	log_json = log
	log_csv = log
	QOE_LOG_FILENAME="qoe" + ("_" + str(CAP_CONTENT_TO) + "s" if CAP_CONTENT_TO < TOTAL_DURATION else "") + ".log"
	if log:
		# Logfile for timings and per-page statistics
		try:
			if os.path.exists(run + QOE_LOG_FILENAME):
				if overwrite_csv:
					os.remove(run + QOE_LOG_FILENAME)
					logging.info("Deleted old " + run + QOE_LOG_FILENAME)
				else:
					log_csv = False
					logging.info("Not overwriting " + QOE_LOG_FILENAME)
		except Exception as err:
			logging.warn("Could not delete " + run + QOE_LOG_FILENAME + ": " + str(err))
		if log_csv:
			try:
				csvfile = open(run + QOE_LOG_FILENAME, "w", newline='')
			except TypeError as e:
				logging.warn("Error opening " + run + QOE_LOG_FILENAME + ": " + str(e))
				csvfile = open(run + QOE_LOG_FILENAME, 'wb')
			csvwriter = csv.writer(csvfile, delimiter=",")

	starttimes = list(set( [ d["starttimestamp"] for d in data ] ))
	starttimes = sorted(starttimes)
	for startt in starttimes:
		thisdata = filter_timings(data, startt, "starttimestamp")
		inputfilename = run + "json/" + thisdata[0]["abr"] + "_" + thisdata[0]["policy"] + "_" + startt + ("_" + str(CAP_CONTENT_TO) + "s" if CAP_CONTENT_TO < TOTAL_DURATION else "") + ".json"
		thisworkload = ( "BigBuckBunny" if "BigBuckBunny" in thisdata[0]["page"] else "RedBull" if "RedBull" in thisdata[0]["page"] else "Valkaama" if "Valkaama" in thisdata[0]["page"] else "unknown" )
		logging.info("Reading " + inputfilename)
		if not os.path.exists(inputfilename):
			logging.warn("No JSON logfile for " + startt + " -- skipping")
			continue
		#qoeinputfilename = run + "json/" + thisdata[0]["abr"] + "_" + thisdata[0]["policy"] + "_" + startt + ("_" + str(CAP_CONTENT_TO) + "s" if CAP_CONTENT_TO < TOTAL_DURATION else "") + "_qoe.json"
		process = subprocess.Popen("python3 -m itu_p1203 --accept-notice " + inputfilename, shell=True, stdout = subprocess.PIPE, stderr=subprocess.DEVNULL)
		output, error = process.communicate()
		decoded_output = json.loads(str((output.decode('ascii'))))
		if error:
			logging.info("Got error: " + str(error))
		logging.debug("Got output: " + str(decoded_output))
		final_qoe = decoded_output[ inputfilename ]['O46']
		#try:
		#	jsonfile_qoe = open(qoeinputfilename, 'r')
		#	qoecontent = json.loads(jsonfile_qoe.read())

		#	final_qoe = qoecontent[ inputfilename ]['O46']
		#except FileNotFoundError:
		#	process = subprocess.Popen("python3 -m itu_p1203 --accept-notice " + inputfilename, shell=True, stdout = subprocess.PIPE, stderr=subprocess.DEVNULL)
		#	output, error = process.communicate()
		#	decoded_output = json.loads(str((output.decode('ascii'))))
		#	if error:
		#		logging.info("Got error: " + str(error))
		#	logging.debug("Got output: " + str(decoded_output))
		#	final_qoe = decoded_output[ inputfilename ]['O46']

		print("\tFinal QoE: " + str(round(final_qoe, 2)) + " (" + thisdata[0]["policy"] + " and " + thisdata[0]["abr"] + " for " + thisworkload + " at " + startt + ")")

		if log_json:
			logfilename = run + "json/" + thisdata[0]["abr"] + "_" + thisdata[0]["policy"] + "_" + startt + ("_" + str(CAP_CONTENT_TO) + "s" if CAP_CONTENT_TO < TOTAL_DURATION else "") + "_qoe.json"
			try:
				if os.path.exists(logfilename):
					if overwrite_json:
						os.remove(logfilename)
						logging.info("Deleted old " + logfilename)
					else:
						log_json = False
						logging.info("Not overwriting " + logfilename)
			except Exception as err:
				print("Could not delete " + logfilename + ": " + str(err))
			if log_json:
				try:
					jsonfile = open(logfilename, "w", newline='')
				except TypeError as e:
					print("Error opening " + logfilename + ": " + str(e))

				jsonfile = open(logfilename, 'w')
				json.dump(decoded_output, jsonfile, sort_keys=True, indent=4)
				jsonfile.close()
				logging.info("Logged to " + logfilename)

		if log_csv:
			thisdata = filter_timings(data, startt, "starttimestamp")[0]
			thisinitdata = filter_timings(initdata, startt, "starttimestamp")[0]
			csvwriter.writerow([thisdata["workload"], thisdata["scenario"], thisdata["starttimestamp"], thisdata["abr"], thisinitdata["policy"], final_qoe])

		log_json = log

	if log_csv:
		print("Logged csv to " + QOE_LOG_FILENAME)
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

		createDirectory(run + "json/")
		runlabel= list(filter(None, run.split('/')))[-1]
		plotlabel = runlabel

		# Get all Navigation Timings as list of dicts
		data = read_abr_log_with_stallings(run)
		initdata = read_init_data(run)
		if data is None or initdata is None:
			print("No data for " + run + " -- next")
			continue

		print("Processing " + run + "... with CAP_CONTENT_TO " + str(CAP_CONTENT_TO))
		output_json(data, initdata, run, log=logtofile)
		extract_qoe(data, initdata, run, log=logtofile)

		if logtofile:
			print("!!! Logged " + run + "!!!")

if __name__ == "__main__":
	main(sys.argv)
