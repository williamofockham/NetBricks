#!/bin/bash
# Lists all the examples that are runnable and/or testable.
# This is used by the build script.
export examples=(
        examples/echo-reply
        examples/ipv4or6
        examples/macswap
        examples/mtu-too-big
        examples/op-errors
        examples/signals
        ### Runnable examples | No Tests associated
        ### =======================================
        examples/acl-fw
        examples/chain
        examples/collect-metrics
        examples/embedded-scheduler
        examples/embedded-scheduler-dependency
        examples/icmpv6-router-advertisement
        examples/lpm
        examples/nat-tcp-v4
        examples/sctp
        examples/maglev
        # examples/tcp-reconstruction
)
