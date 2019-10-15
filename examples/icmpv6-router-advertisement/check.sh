#!/bin/bash
TEST_NAME=icmpv6-router-advertisement

PORT_OPTIONS="dpdk:eth_pcap0,rx_pcap=data/icmpv6_router_advertisement.pcap,tx_pcap=/tmp/out.pcap"

../../build.sh run $TEST_NAME -p $PORT_OPTIONS -c 0 -c 1 -d 1
tcpdump -tner /tmp/out.pcap | tee /dev/tty | diff - data/expect_icmpv6_router_advertisement.out

result=$?
echo ----
if [[ $result != 0 ]]; then
  echo FAIL
  exit $result
else
  echo PASS
fi
