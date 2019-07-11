#!/bin/bash
TEST_NAME=signals

PORT_OPTIONS="dpdk:eth_af_packet,iface=lo"

nohup ../../build.sh run $TEST_NAME -p $PORT_OPTIONS -c 1 &
# Extra time to load the signaler
sleep 5
PID=`pidof signals`
kill -HUP "$PID"
sleep 3
kill -TERM "$PID"
sleep 3

echo ----

pidof signals
if [[ $? == 0 ]]; then
    kill -9 "$PID"
    echo "FAIL: process still running"
    exit 1
fi

cat test.log | tee /dev/tty | diff - data/expect.out
if [[ $? != 0 ]]; then
    echo "FAIL: wrong output"
    exit 1
fi

echo "PASS"
