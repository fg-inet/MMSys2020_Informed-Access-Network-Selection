#!/bin/bash
# Start MAMma with config adjusted, then execute a workload script
# Move results to their own directory
# -- simplified version just for video --

# Requires:
# * CONF           - the interface config, a bash script that defines IFACES, IFACE_NETS and IFACE_ADDRS
# * WORKLOADSCRIPT - a script that generates traffic. It can use all variables from this one.
# * urlfile        - a file that contains URLs for the workloadscript, one per line
# * TIMES          - how many times will the URL be fetched
# * SCENARIO       - additional string to describe and/or influence the scenario - e.g., noproxy
#  "bypass" (default) <-> "shaper" (if config file name contains "shaper")
#     -- "bypass": Do not shape traffic, or do not go via shaper
#                 (depends on your default route)


yes| rm *.log 2>/dev/null

exec > >(tee -ia run_stdout.log)
exec 2> >(tee -ia run_stderr.log)

WORKLOADSCRIPT="./stream_video.sh"

MAM_LOGFILE="mam_output.log"
WORKLOAD_LOGFILE="workload_output.log"


if [ ! -x "$WORKLOADSCRIPT" ]
then
	WORKLOADSCRIPT="./stream_video.sh"
fi

if [ "$CONF" == "" -a "$1" != "" ]
then
	CONF="$1"
else
	CONF="default.conf"
fi

if [ "$2" == "" ]
then
	urlfile="RB_url"
else
	urlfile="$2"
fi

if [ "$3" == "" ]
then
    TIMES=1
else
    TIMES=$3
fi

if [ "$4" != "" ]
then
    SCENARIO="$4"
fi



. "$CONF"

DATERUN=$(date +%Y-%m-%dT%H:%M)

if [ "$IFACE_NETS" == "" ]
then
	echo "Using default"
	IFACES=("enp0s3" "enp0s8")
	IFACE_ADDRS=("10.0.2.15" "10.0.3.15")
	IFACE_NETS=("10.0.2.0/24" "10.0.3.0/24")
fi

ifaces_length=${#IFACES[@]}
ifaces_maxind=$((${#IFACES[@]}-1))

echo "Configfile: $CONF, urlfile: $urlfile"
cp "$CONF" "config.${CONF}.log"
cp "$urlfile" "urlfile-${urlfile}.log"

scenarioname="${SCENARIO}_${CONF%.conf}"

if [[ "$SCENARIO" == *cross* ]]; then
    CROSSTRAFFIC=${SCENARIO##*cross}
    #scenarioname="${scenarioname}${CROSSTRAFFIC}Bit${CROSSTRAFFICLABEL}"
fi

if [[ "$CONF" == shaper*.conf ]]
then
	echo "Checking shaping..."
	. "functions.sh"
	echo "Scenario name: $scenarioname"
    if [ "$VARIABLE_SHAPING" != "" ]
    then
        scenarioname="${scenarioname}-$VARIABLE_SHAPING"
        #ssh $SHAPER "sudo ./variable_shaping.sh logs/$VARIABLE_SHAPING.log" &
    fi
    if [ "$SHAPING" != "" ]
    then
        for i in `seq 0 1 $ifaces_maxind`
        do
            echo "Set shaping: ${SHAPING[$i]}"
            setshaping "shaper-profiles/${SHAPING[$i]}.json" "${IFACE_ADDRS[$i]}" "$ATCD"
            scenarioname="${scenarioname}_${SHAPING[$i]}"
            if [[ ! "${SHAPING[$i]}" == *loss* ]]; then
                scenarioname="${scenarioname}-0loss"
            fi
            echo "Scenario name now: $scenarioname"
            cp "shaper-profiles/${SHAPING[$i]}.json" "shaping-$i-${SHAPING[$i]}-${IFACE[$i]}.json.log"
        done
        . "$CONF"
        killall -q ssh
        echo "Verifying shaping on $SHAPER"
        catchiptablesbug "$SHAPER" $ifaces_length
        ret=$?
        if [ $ret != 0 ]; then
            echo "iptables Bug! flushing table, shaping again"
            ssh "$SHAPER" "sudo iptables -t mangle -F"
            for i in `seq 0 1 $ifaces_maxind`
            do
                setshaping "shaper-profiles/${SHAPING[$i]}.json" "${IFACE_ADDRS[$i]}" "$ATCD"
            done
        fi
    fi
    if [ "$SHAPE_BW" != "" ]
    then
        for i in `seq 0 1 $((${#SHAPE_BW[@]}-1))`
        do
            reset_shaping $SHAPER ${SHAPE_BW_ON[$i]}
            shape_bw ${SHAPE_BW[$i]} $SHAPER ${SHAPE_BW_ON[$i]}
            scenarioname="${scenarioname}-${SHAPE_BW[$i]}"
        done
    fi
    if [ "$SHAPE_DELAY" != "" ]
    then
        for i in `seq 0 1 $((${#SHAPE_DELAY[@]}-1))`
        do
            shape_delay ${SHAPE_DELAY[$i]} $SHAPER ${SHAPE_DELAY_ON[$i]} ${SHAPE_DELAY_QSIZE[$i]}
            scenarioname="${scenarioname}-${SHAPE_DELAY[$i]}"
        done
    fi


    . "$CONF"
    if [[ $QUEUE_SIZE != "" ]]; then
        setqueuesize $QUEUE_SIZE $SHAPER
        scenarioname="${scenarioname}-qsize${QUEUE_SIZE}"
    fi
    if [[ $QUEUE_SIZE1 != "" ]]; then
        # Set queue size only for the queue matching a specific filter
        setqueuesize $QUEUE_SIZE1 $SHAPER "eth1" ${QUEUE_FILTER1}
        scenarioname="${scenarioname}-qsize${QUEUE_SIZE1}"
    fi
    if [[ $QUEUE_SIZE2 != "" ]]; then
        # Set queue size only for the queue matching a specific filter
        setqueuesize $QUEUE_SIZE2 $SHAPER "eth1" ${QUEUE_FILTER2}
        scenarioname="${scenarioname}-qsize${QUEUE_SIZE2}"
    fi
fi

echo "Scenario name finally: $scenarioname"

echo ""
echo "Set up $scenarioname"
echo ""
echo ""

LOGPREFIX="data/run-$DATERUN-$scenarioname"
mkdir -p "data"
mkdir -p $LOGPREFIX

if [ "$CROSSTRAFFIC" != "" ]; then
    killall -q harpoon
    ssh $CROSSCLIENT "killall -q harpoon"
    ssh $CROSSCLIENT "killall -q iperf"
    if [ "$CROSSSERVER" != "" ]; then
        ssh $CROSSSERVER "killall -q iperf"
    fi
    crosstrafficcounter=1
    if [ "$CROSSTRAFFICLABEL" == "harpoon" ]; then
        #/usr/local/harpoon/run_harpoon.sh -v3 -c -w 600 -f "software/harpoon/examples/tcp_client_ex2.xml" 2>"log/harpoon_${crosstrafficcounter}.log" &

        crosstrafficcounter=`expr $crosstrafficcounter + 1`
        if [ "$HARPOON_SESSIONS" == "" ]; then
            HARPOON_SESSIONS=1
        fi
        #ssh ns3sim "killall harpoon; sleep 5; sed 's/active_sessions> .* </active_sessions> $HARPOON_SESSIONS </' -i harpoon/examples/tcp_server_ex2.xml;"
        #ssh ns3sim "cd harpoon; /usr/local/harpoon/run_harpoon.sh -v 10 -w600 -c -f examples/tcp_server_ex2.xml &"
        ssh $CROSSCLIENT "mkdir -p log; sed 's/active_sessions> .* </active_sessions> $HARPOON_SESSIONS </' -i harpoon/examples/tcp_client_ex2.xml; /usr/local/harpoon/run_harpoon.sh -v3 -c -w 600 -f harpoon/examples/tcp_client_ex2.xml 2> log/harpoon_${crosstrafficcounter}.log" &
        #ssh $CROSSCLIENT "mkdir -p log; /usr/local/harpoon/run_harpoon.sh -v3 -c -w 600 -f harpoon/examples/tcp_client_ex2.xml 2> log/harpoon_${crosstrafficcounter}.log" &
        iptablesline=$(ssh $SHAPER "sudo iptables -L FORWARD -t mangle -v|grep $HARPOON_ON_SAME_BOTTLENECK_AS | grep eth1")
        id_to_shape=${iptablesline##*x}
        echo "To get harpoon on same bottleneck as $HARPOON_ON_SAME_BOTTLENECK_AS, got iptablesline $iptablesline, Putting harpoon on queue $id_to_shape"
        ssh $SHAPER "sudo iptables -t mangle -A FORWARD -i eth0 -d $CROSSCLIENTIP -j MARK --set-mark 0x$id_to_shape"
        ssh $SHAPER "sudo iptables -t mangle -A FORWARD -i eth1 -s $CROSSCLIENTIP -j MARK --set-mark 0x$id_to_shape"
        #ssh $SHAPER "sudo iptables -t mangle -A FORWARD -i eth0 -d $FAKE_DEFAULT_ADDR -j MARK --set-mark 0x2"
        #ssh $SHAPER "sudo iptables -t mangle -A FORWARD -i eth1 -s $FAKE_DEFAULT_ADDR -j MARK --set-mark 0x2"
    else
        for client in "${CROSSCLIENT[@]}"; do
            echo "Setting up ${crosstrafficcounter}. crosstraffic from $client to $CROSSSERVER ($CROSSDST): $CROSSTRAFFIC"
            ssh $CROSSSERVER "mkdir -p /tmp/log; iperf -s -u -p `expr 5000 + $crosstrafficcounter` -i 1 > /tmp/log/iperf_${crosstrafficcounter}.log" &
            sleep 1
            ssh $client "mkdir -p log; iperf -c $CROSSDST -u -p `expr 5000 + $crosstrafficcounter` -b $CROSSTRAFFIC -t 90000 -i 1 > log/iperf_${crosstrafficcounter}.log" &
            crosstrafficcounter=`expr $crosstrafficcounter + 1`
        done
    fi
fi

sleep 5


echo "Running workload script $WORKLOADSCRIPT $urlfile $TIMES $scenarioname $LOGPREFIX"

if [[ "$CONF" == shaper*.conf ]]
then
    killall -q ssh
    . "$CONF"
    # put default interface down for the run, so it does not get used by MPTCP
    sudo ip link set "$DEFAULT" down
    sudo ip ro add default via $FAKE_DEFAULT_GATEWAY src $FAKE_DEFAULT_ADDR
    wpa_supplicant_pids=$(pgrep "wpa_supplicant -Dnl80211 -i${DEFAULT}" -f | awk NF=NF RS= OFS=' ')
    echo "PIDs to kill: ${wpa_supplicant_pids}"
    sudo kill ${wpa_supplicant_pids} 2>&1 >/dev/null
    sleep 1
    #ps aux|grep wpa_supplicant

    # Drop MPTCP over fake default interface
    sudo iptables -A OUTPUT -s $FAKE_DEFAULT_ADDR -p tcp --tcp-option 30 -j DROP
    echo "Put default interface $DEFAULT down and put default route on $FAKE_DEFAULT_IF"
fi

# this is for fetchurl (browser instrumentation):
if [[ ! $SCENARIO == *demo* ]]; then
    source "$WORKLOADSCRIPT" "$urlfile" "$TIMES" "$scenarioname" "$LOGPREFIX" 2>&1 >"$WORKLOAD_LOGFILE"
else
    # Output to stdout
    source "$WORKLOADSCRIPT" "$urlfile" "$TIMES" "$scenarioname" "$LOGPREFIX" 2>&1 | tee "$WORKLOAD_LOGFILE"
fi

if [ "$CROSSCLIENT" != "" ]; then
    for client in "${CROSSCLIENT[@]}"; do
        ssh $client "killall iperf" &
        ssh $client "killall harpoon" &
    done
fi


if [ "$CROSSCLIENT" != "" ]; then
    crosstrafficcounter=1
    killall -q harpoon
    for client in "${CROSSCLIENT[@]}"; do
        scp "$client:log/iperf_${crosstrafficcounter}.log" "data/run-$DATERUN-$scenarioname/client${crosstrafficcounter}-iperf.log"
        scp "$CROSSSERVER:/tmp/log/iperf_${crosstrafficcounter}.log" "data/run-$DATERUN-$scenarioname/server${crosstrafficcounter}-iperf.log"
        scp "$client:log/harpoon_${crosstrafficcounter}.log" "data/run-$DATERUN-$scenarioname/client${crosstrafficcounter}-harpoon.log"
        crosstrafficcounter=`expr $crosstrafficcounter + 1`
    done
fi



if [[ "$CONF" == shaper*.conf ]]
then
    . "$CONF"
    echo "setting up default again because CONF=$CONF"
    # set default up again?
    sudo ip link set "$DEFAULT" up
    sleep 1
    sudo rm "/tmp/wifi_${DEFAULT_WIFICONF}.log"
    sudo wpa_supplicant -Dnl80211 -i"${DEFAULT}" -c"network-config/${DEFAULT_WIFICONF}" 2>&1 >"/tmp/wifi_${DEFAULT_WIFICONF}.log" &
    sleep 3
    #ps aux|grep wpa_supplicant

    sudo ip ro del default via "$IFACE_GATEWAYS"
    sudo ip ro add default via "$DEFAULT_GATEWAY" dev "$DEFAULT" proto static metric 1
    sudo ip ro del default via "$FAKE_DEFAULT_GATEWAY"

    echo "Put default interface $DEFAULT up again"
fi

echo ""

#./clean_workload_output.sh

echo "Done with workload, exiting"

killall -q mamma

echo "done"

rm -rf /tmp/foo2020*

cp "$CONF" "$LOGPREFIX/"
mv *.log "$LOGPREFIX/"
