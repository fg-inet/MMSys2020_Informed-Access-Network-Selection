# Using Informed Access Network Selection to Improve HTTP Adaptive Streaming Performance

This repository supplements the following paper:
Theresa Enghardt, Thomas Zinner, and Anja Feldmann. Using Informed Access Network Selection to Improve HTTP Adaptive Streaming Performance. 11th ACM Multimedia Systems Conference (MMSys). June 2020 (to appear).

Please contact [Theresa Enghardt](mailto:theresa@inet.tu-berlin.de) with any questions.

In the paper, we enable end-user devices to select between multiple available access networks, such as WiFi and cellular, or combine these networks to improve performance for HTTP Adaptive Streaming (HAS).

## Overview of Artifacts

Related to this paper, we present the following artifacts:

- The Socket Intents prototype, which implements Informed Access Network Selection (IANS), including the IANS policies used for HAS: The Optimist Policy, the Pessimist Policy, and the Selective MPTCP Policy. The Socket Intents prototype is available [on Github](https://github.com/fg-inet/socket-intents). In this repository, we reference a specific revision of the prototype, which was used in the evaluation, as a submodule.

- Our modified version of the GPAC video player, which can use the Socket Intents prototype to load videos using HAS, thus, taking advantage of the prototype. Our fork of the GPAC player is available [on Github](https://github.com/fg-inet/gpac). In this repository, we reference a specific revision of the player, which was used in the evaluation, as a submodule.

- Scripts that we used to run our experiments and to evaluate the results. These scripts are included in this repository.

- A virtual machine on which both the Socket Intents prototype and the modified GPAC player are installed. This virtual machine is available [on Zenodo](https://doi.org/10.5281/zenodo.3732191), along with the dataset of the paper's evaluation.

- The dataset on which the evaluation in the paper was based. The dataset is available [on Zenodo](https://doi.org/10.5281/zenodo.3732191), along with the virtual machine.

## Steps to Reproduce

This section describes how to load a video using the GPAC player and the Socket Intents prototype.

Possible workloads are all videos using Dynamic Adaptive Streaming over HTTP (MPEG-DASH).
In the paper, we use our own copy of the [DASH dataset of Uni Klagenfurt (MMSys 2014](http://www-itec.uni-klu.ac.at/ftp/datasets/DASHDataset2014/), i.e., "Big Buck Bunny" (BBB), "Red Bull" (RB), and "Valkaama" (V).

### Load Video Using the Virtual Machine

The virtual machine (VM) [available on Zenodo](https://doi.org/10.5281/zenodo.3732191) has both the Socket Intents prototype and the modified GPAC player installed. Furthermore, it contains the same experiment scripts that were used in the paper. The scripts were modified so that the virtual machine is able to run the software with a simple setup: The machine has two virtual network interfaces, and it can load a video using either single interface or both interfaces using IANS with the Optimist and Pessimist policy.

**Prerequisites**: virtualbox or alternative software to run virtual machines

1. Download the VM from [Zenodo](https://doi.org/10.5281/zenodo.3732191): **ians_video.vdi**

2. Boot the virtual machine and log in using the credentials: User name: *osboxes*, Password: *osboxes.org*

3. Once booted, open a terminal: Click on "Activities" in the top left corner, search for "Terminal".

4. (Optional) Adjust parameters in ``stream_video.sh``, e.g., video playout duration, Adaptive BitRate algorithm (ABR) to use, and IANS policies to use. This will greatly influence the duration of the test. Note that the VM does not support Multipath TCP (MPTCP), so running this policy will not have an effect.

5. (Optional) Choose a workload by creating a text file containing the URL of an MPEG-DASH .mpd file.

6. Run ``./run.sh`` (or, if you want to use a different URL file, ``./run.sh default.conf $URLFILE``) using a different URL file) to run a test. By default, the test will only load the first 12 seconds and only use BOLA_O as ABR, as the test becomes very long otherwise. By default, the test will first load the video using only network interface 1, then network interface 2, then both using the Optimist policy, then both using the Pessimist policy. Note that for the RB video, it takes a long time until any non-blank frame is painted because the VM is very slow.

7. Find the experiment output directories using ``cd data/; ls`` - each experiment run corresponds to one directory. Note that there See section "Analyze the Data" for instructions how to look at the data.

### Load Video Using Your Own Setup 

**Prerequisites**:
- A Linux machine (tested on Debian and Ubuntu) and being able to access a server hosting the video workload via at least two network interfaces (note that policy routing may be required)
- build-essentials for building C code, Python3 (including packages scipy and matplotlib), R (including packages zoo, lattice, and viridis, plus dependencies), wget

1. Build and install the Socket Intents prototype, see [README.md in the relevant repository](https://github.com/fg-inet/socket-intents). Create config files for each IANS policy you want to use as shown in the "Testing the Socket Intents Prototype" section.

2. Build and install the modified GPAC player, see [repository](https://github.com/fg-inet/gpac)

3. Download the [P.1203 repository](https://github.com/itu-p1203/itu-p1203) and the necessary dependencies, such as python3-matplotlib and python3-scipy. Create a symbolic link from the evaluation script directory, ``performance-test/video/eval``, to the directory called itu_p1203 within the P.1203 repository. Note: Make sure you do not create this link to the top-level directory of P.1203, but the subdirectory that contains the actual code. You need to be able to execute this code, e.g., by invoking ``python3 -m itu_p1203 $FILE`` from the command line.

4. From from the evaluation script directory, ``performance-test/video/eval``, create a symbolic link to your "data" directory. E.g. run the following in the directory: ``ln -s /home/yourname/data data``

5. From the top-level directory of this repo, create the following symbolic links: ``ln -s performance-test/video/load_stuff.sh load_stuff.sh`` and ``ln -s performance-test/video/stream_video.sh stream_video.sh`` and ``ln -s performance-test/video/run.sh run.sh``

### Analyze the Data

1. In performance-test/video/eval (either on the VM or on your own machine with this repository checked out), execute ``./stallings.R`` to compute stall events and durations. 

2. Run ``./dump_json.py`` to compute produce the QoE values using P.1203.

3. Run ``./plot_qoe.R`` to produce barplots for the QoE.

4. Run ``./heatmap_video.R`` to produce heatmaps.

Note: If you have multiple runs in data/, you may want to explicitly select the run(s) to plot or compute data for, e.g., to only compute data for the first run, you must execute ``./stallings.R 1 1`` (start with the first run and only compute data for this one run). To plot data for the first four runs, execute ``./plot_qoe.R 1 4``


#### Detailed look at log files

Note that the directory contains lots of log files, and some more are produced using the analysis and plot scripts above. For example, notable files include:

- **initial_playout.log**: Timestamps for launching the player, loading the manifest file and initial segments, and starting the video playout for each video load. We use the following format: ``Video, IANS policy used, Scenario, Starttimestamp for this load, Timestamp when first Line of Code in player is executed, URL of the manifest file, Timestamp of when the terminal is loaded, Timestamp when starting the manifest file load, Timestamp when starting to load the initial segments, Timestamp when loading the initial segments ends, Timestamp when the player is initialized, and Timestamp when the actual playout starts``.

- **abrdata_with_stallings.log**: For each loaded video segment, the start timestamp, end timestamp, buffer level, and whether there was a stalling event (=whether the video playback was interrupted) before playing out this segment. Format: ``Workload, Scenario, Starttimestamp for this load, Segment index, Adaptive BitRate algorithm used, Representation loaded for this segment, buffer level when loading this segment, estimated download rate prior to loading this segment, maximum buffer size, client, starttimestamp as UNX timestamp, run ID, IANS policy used, timestamp when segment load started, timestamp when segment load ended, download time for timestamp, by how many milliseconds this segment arrived early or late, when the segment was supposed to be there for uninterrupted playout, whether there was supposed to be any stalling event based on our script's computation (0 for No, 1 for Yes), computed stalling duration, whether there was actually any stalling based on frame paint time, actual stalling duration, which frame was stalled, and the file name of the manifest file``.

- **qoe.log**: The Quality of Experience as Mean Opinion Score (MOS), computed using the P.1203 model, for each video load. We use the following format: ``Video, Scenario, Starttimestamp for this load, Adaptive BitRate algorithm used, IANS policy used, MOS``. qoe_120s .. qoe_210s contain the MOS values computed using the P.1203 model while only using the first 120s .. 210s of content.

- **json/**: This directory contains input and output for the P.1203 model software. Files ending just with .json are the input files, with representations per segment and stalling events, while files ending with qoe.json contain the output of the P.1203 software, with audiovisual quality scores and the final score logged in qoe.log.

## Structure of the Dataset Used in the Paper

The [dataset of the original experiment](https://doi.org/10.5281/zenodo.3732191) runs contains logs produced by the Socket Intents prototype and our modified GPAC player during experiments conducted between 20th June and 02nd September 2019.

In these experiments, we load the first four minutes of each of the following videos:

- "Big Buck Bunny" (BBB), an animation video,
- "Red Bull Playstreets" (RB), a sports video, and
- "Valkaama" (V), a movie.

We conduct our experiments in a fully controlled testbed, using both scenarios with variable network capacity and with cross-traffic.

For easier handling, our dataset is split into the following files:

- **variable_bbb.tar.gz**: Results for the variable capacity scenarios when loading "Big Buck Bunny",
- **variable_rb.tar.gz**: Results for the variable capacity scenarios when loading "Red Bull Playstreets",
- **variable_v.tar.gz**: Results for the variable capacity scenarios when loading "Valkaama", and
- **crosstraffic.tar.gz**: Results for the cross-traffic scenarios when loading all videos.

Each .tar.gz file contains different the different runs as directories. Each directory name corresponds to one run, annonated with the date and time at which the experiment started and with the network conditions emulated in the network scenario for this particular run.

To analyze data, please refer to the section above called "Analyze the Data".


## Acknowledgments

When using code or data related to this repository, please cite the following paper:

Theresa Enghardt, Thomas Zinner, and Anja Feldmann. Using Informed Access Network Selection to Improve HTTP Adaptive Streaming Performance. 11th ACM Multimedia Systems Conference (MMSys). June 2020.