#!/bin/bash

if [ "$1" != "" ]
then
	addr_to_bind="$1"
else
	addr_to_bind="10.1.2.4"
fi

echo "Loading $CONF"

. "$CONF"

for i in `seq 0 100`; do
    sleep 3
    echo "$i. load of 100KB using $addr_to_bind"
    wget "http://130.149.221.201/katze-guckt-100kb-8.jpg" --delete-after --intent-category=controltraffic -e robots=off -nv
    #wget "http://130.149.221.201/katze-guckt-100kb-8.jpg" --delete-after --bind-address=$addr_to_bind -e robots=off -nv
done

