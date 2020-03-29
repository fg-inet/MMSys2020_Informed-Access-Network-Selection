#!/bin/bash
# Script that streams a video using GPAC and logs bitrates and stuff
#

LOGPREFIX="./log"

LOGFILE="log.log"

SCENARIO="firsttry"

TIMES=1 # How often to do the thing

# Duration of each video playout in seconds.
#PLAYDURATION=12
#PLAYDURATION=24
PLAYDURATION=60
#PLAYDURATION=240

# Adaptive BitRate algorithms to use.
ABRS=("BOLA_O")
#ABRS=("BOLA_O" "BOLA_BASIC" "BBA-0")
#ABRS=("BBA-0" "BOLA_BASIC" "BOLA_O")
#ABRS=("bandwidth" "buffer" "BBA-0" "BOLA_BASIC" "BOLA_FINITE" "BOLA_U" "BOLA_O")

# IANS policies to use.
if [ "$POLICIES" == '' ];
then
	#POLICIES=("$HOME/policy_network1.conf")
	#POLICIES=("$HOME/policy_network2.conf")
	POLICIES=("$HOME/policy_network1.conf" "$HOME/policy_network2.conf" "$HOME/policy_optimist.conf" "$HOME/policy_pessimist.conf")
	#POLICIES=("$HOME/policy_network1.conf" "$HOME/policy_network2.conf" "$HOME/policy_optimist.conf" "$HOME/policy_pessimist.conf" "$HOME/policy_mptcp.conf")
fi

echo "Policies to use: ${POLICIES[@]}"

if [ "$1" == '' ];
then
	echo "Usage: $0 <URLFILE> [<SCENARIO>] [<LOGPREFIX>]"
	echo "URLFILE: Text file containing one or more URLs, one each line"
	echo "SCENARIO: String describing the scenario"
	echo "LOGPREFIX: Where to store the results (default: $LOGPREFIX)"
	exit 1
else
	URLFILE="$1"
fi

if [ "$2" != '' ];
then
	TIMES="$2"
fi

if [ "$3" != '' ];
then
	SCENARIO="$3"
fi

DATERUN=$(date +%Y-%m-%dT%H:%M)

if [ "$4" != '' ];
then
	LOGPREFIX="$4"
else
    LOGPREFIX="data/run-$DATERUN-gpac"
fi

mkdir -p "$LOGPREFIX"

killall -q MP4client

kill `pgrep -f stuff`

readarray -t URLS < "$URLFILE"

echo "URLS: ${URLS[@]}"

# Outer loop: Try number 1 .. n
for (( i=0; i < $TIMES; ++i )); #i in `seq 1 "$TIMES"`;
do
    i=0
	echo "Try $((i+1))/$TIMES"
    pkill "mamma"
    sleep 2

    # First middle loop: ABRs
    for abr in ${ABRS[@]};
    do
        echo "ABR: $abr"
        sed 's/NetworkAdaptation=.*/NetworkAdaptation='"$abr"'/' -i $HOME/.gpac/GPAC.cfg

        # Second middle loop: URLs
        for line in ${URLS[@]}
        do
            u="$line"

            echo "Fetching $u"

            if [[ "$SCENARIO" == *"shufflepolicies"* ]]
            then
                policies_shuffled=( $(shuf -e "${POLICIES[@]}" | awk NF=NF RS= OFS=' '))
                echo "Shuffled policies: ${policies_shuffled[@]}"
            else
                policies_shuffled=("${POLICIES[@]}")
                echo "Using policies: ${policies_shuffled[@]}"
            fi
            ln -sf ${policies_shuffled[0]} ~/policy.conf

            if ! pgrep "mamma" > /dev/null
            then
                DATETRY=$(date +%Y-%m-%dT%H:%M:%S)
                echo "mamma not running -- restarting, logging to $LOGPREFIX/mamma-${DATETRY}.log"
                mamma ~/policy.conf &> "$LOGPREFIX/mamma-${DATETRY}.log" &
                sleep 1

                . $CONF
                IPERF_LISTEN_PORTS=("5002" "5003")
                j=0
                if [[ "$SCENARIO" == *"shufflepolicies"* ]]; then
                    for iface in ${IFACE_ADDRS[@]}; do
                        echo "iperf -c $TESTDESTINATION -B $iface -r -L ${IPERF_LISTEN_PORTS[$j]}"
                        iperf -c $TESTDESTINATION -B $iface -r -L ${IPERF_LISTEN_PORTS[$j]}
                        j=`expr $j + 1`
                        echo "iperf done"
                        sleep 1
                    done
                fi
            fi

            # Inner loop: Policies
            for (( p=0; p<${#policies_shuffled[@]}; ++p ));
            do
                DATERUN=$(date +%Y-%m-%dT%H:%M:%S)
                echo "Policy: ${policies_shuffled[$p]}"
                policyname=${policies_shuffled[$p]##*/}
                policyname=${policyname%.conf}
                policyname=${policyname#policy_}
                echo "Running with policy: $policyname"

                ln -sf ${policies_shuffled[$p]} ~/policy.conf

                if ! pgrep "mamma" > /dev/null
                then
                    echo "mamma not running -- can't use this iteration!"
                    continue
                else
                    echo "Doing SIGHUP to mamma at beginning of loop"
                    pkill -SIGHUP "mamma"
                fi
                echo "mamma is using policy: `readlink ~/policy.conf`"
                sleep 1

                if [[ $VARIABLE_SHAPING != "" ]]; then
                    echo "On $SHAPER: sudo ./variable_shaping.sh logs/$VARIABLE_SHAPING.log $VARIABLE_SHAPING_IP"
                    ssh $SHAPER "sudo ./variable_shaping.sh logs/$VARIABLE_SHAPING.log $VARIABLE_SHAPING_IP" &
                fi

                # Invoke GPAC - actually start streaming the video
                #echo "calling: ${METHODOLOGY} $u $policyname,$SCENARIO 1 $LOGPREFIX/" "$policyname"
                echo "calling: MP4Client -exit -no-regulation $u >gpac_stdout.log 2>gpac_stderr.log"
                MP4Client -exit $u >gpac_stdout.log 2>gpac_stderr.log &
                ./load_stuff.sh >load_stuff.log 2>load_stuff.log &
                sleep $PLAYDURATION
                killall MP4Client
                kill `pgrep -f stuff`
                #perf record -- ./webtimings.py $u "$policyname,$SCENARIO" 1 "$LOGPREFIX/" "$policyname"
                exitstatus=$?
                #mv perf.data "$LOGPREFIX/${u##*/}_$policyname_${i}_perf.data"
                #if [[ ! $SCENARIO == *noclose* ]]; then
                #    killall -q firefox
                #fi
                if [[ $VARIABLE_SHAPING != "" ]]; then
                    echo "Killing variable shaping on $SHAPER"
                    ssh $SHAPER 'sudo kill `pgrep  -f variable_shaping`'
                    ssh $SHAPER "ps aux|grep variable"
                fi

                echo "Scenario: $SCENARIO"

                echo "Got exit status $exitstatus"

                echo "Done streaming $u ($((i+1)) out of $TIMES times)"
                echo ""

                sed 's/^/'"${u##*/},$policyname,$SCENARIO,$DATERUN,$abr,"'/' -i abr.log
                sed 's/^/'"${u##*/},$policyname,$SCENARIO,$DATERUN,"'/' -i download.log
                sed '/^$/d' -i initial_playout.log
                sed 's/$/\n/' -i initial_playout.log
                sed 's/^/'"${u##*/},$policyname,$SCENARIO,$DATERUN,"'/' -i initial_playout.log
                sed 's/^/'"${u##*/},$policyname,$SCENARIO,$DATERUN,"'/' -i frame_drawn.log
                cat abr.log >> $LOGPREFIX/abr.log
                cat download.log >> $LOGPREFIX/download.log
                cat initial_playout.log >> $LOGPREFIX/initial_playout.log
                cat frame_drawn.log >> $LOGPREFIX/frame_drawn.log
                rm abr.log
                rm download.log
                rm initial_playout.log
                rm frame_drawn.log

                mv gpac_stdout.log $LOGPREFIX/gpac_stdout_$DATERUN.log
                mv gpac_stderr.log $LOGPREFIX/gpac_stderr_$DATERUN.log

                cat $policyname.log >> $LOGPREFIX/$policyname.log
                sed 's/^/'"${u##*/},$policyname,$SCENARIO,$DATERUN,"'/' -i $policyname.log
                cat $policyname.log >> $LOGPREFIX/policy.log

                cat /tmp/metrics_interface.log >> $LOGPREFIX/interface-combined.log
                cat /tmp/metrics_prefix.log >> $LOGPREFIX/prefix-combined.log
                rm /tmp/metrics_interface.log
                rm /tmp/metrics_prefix.log
                rm $policyname.log

                if [[ $policyname == *noproxy* ]]; then
                    sudo ip ro del default
                    sudo ip ro add default via $FAKE_DEFAULT_GATEWAY src $FAKE_DEFAULT_ADDR
                    echo "Restored default route:"
                    sudo ip ro
                fi
            done

            echo "Done with $policyname and $u"

        done # with URLs
        echo "Done with $u and $abr"
    done # with ABRs
    echo "Entirely done"

    if [[ ! $SCENARIO == *noclose* ]]; then
        killall -q firefox
    fi
done
echo "Finally done with all tries"
pkill "mamma"

mv *.log "$LOGPREFIX/"
echo "Moved log files to $LOGPREFIX"
mkdir -p $LOGPREFIX/plots
