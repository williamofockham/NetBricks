#!/bin/bash
TEST_NAME=ndp-router-advertisement

PORT_OPTIONS1="dpdk:eth_pcap0,rx_pcap=data/ndp_router_advertisement.pcap,tx_pcap=/tmp/out.pcap"

../../build.sh run $TEST_NAME -p $PORT_OPTIONS1 -c 0 -c 1 -d 1
tcpdump -tner /tmp/out.pcap | tee /dev/tty | diff - data/ndp_router_advertisement.out

result=$?
echo ----
if [[ $result != 0 ]]; then
  echo FAIL
  exit $result
else
  echo PASS
fi
