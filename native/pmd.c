#include <rte_bus_pci.h>
#include <rte_config.h>
#include <rte_eal.h>
#include <rte_ethdev.h>
#include <rte_malloc.h>
#include <rte_pci.h>
#include <rte_version.h>
#include "mempool.h"

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define CHECK_INTERVAL 100  /* 100ms */
#define MAX_REPEAT_TIMES 90 /* 9s (90 * 100ms) in total */

static int log_eth_dev_info(struct rte_eth_dev_info *dev_info) {
    if (!dev_info)
        return -1;
    RTE_LOG(INFO, PMD, "driver_name: %s (if_index: %d)\n", dev_info->driver_name, dev_info->if_index);
    RTE_LOG(INFO, PMD, "nb_rx_queues: %d\n", dev_info->nb_rx_queues);
    RTE_LOG(INFO, PMD, "nb_tx_queues: %d\n", dev_info->nb_tx_queues);
    RTE_LOG(INFO, PMD, "rx_offload_capa: %lx\n", dev_info->rx_offload_capa);
    RTE_LOG(INFO, PMD, "rx_queue_offload_capa: %lx\n", dev_info->rx_queue_offload_capa);
    RTE_LOG(INFO, PMD, "tx_offload_capa: %lx\n", dev_info->tx_offload_capa);
    RTE_LOG(INFO, PMD, "tx_queue_offload_capa: %lx\n", dev_info->tx_queue_offload_capa);
    RTE_LOG(INFO, PMD, "flow_type_rss_offloads: %lx\n", dev_info->flow_type_rss_offloads);
    return 0;
}

static int log_eth_conf(struct rte_eth_conf *eth_conf) {
    if (!eth_conf)
        return -1;
    RTE_LOG(INFO, PMD, "tx_offload: %lx\n", eth_conf->txmode.offloads);
    RTE_LOG(INFO, PMD, "rx_offload: %lx\n", eth_conf->rxmode.offloads);
    return 0;
}

static int log_eth_rxconf(struct rte_eth_rxconf *rxconf) {
    if (!rxconf)
        return -1;
    RTE_LOG(INFO, PMD, "rx_thresh (p,h,w): (%d, %d, %d)\n", rxconf->rx_thresh.pthresh,
            rxconf->rx_thresh.hthresh, rxconf->rx_thresh.wthresh);
    RTE_LOG(INFO, PMD, "rx_free_thresh: %d\n", rxconf->rx_free_thresh);
    RTE_LOG(INFO, PMD, "rx_drop_en: %d\n", rxconf->rx_drop_en);
    RTE_LOG(INFO, PMD, "rx_deferred_start: %d\n", rxconf->rx_deferred_start);

    return 0;
}

static int log_eth_txconf(struct rte_eth_txconf *txconf) {
    if (!txconf)
        return -1;
    RTE_LOG(INFO, PMD, "tx_thresh (p,h,w): (%d, %d, %d)\n", txconf->tx_thresh.pthresh,
            txconf->tx_thresh.hthresh, txconf->tx_thresh.wthresh);
    RTE_LOG(INFO, PMD, "tx_free_thresh: %d\n", txconf->tx_free_thresh);
    RTE_LOG(INFO, PMD, "tx_rs_thresh: %d\n", txconf->tx_rs_thresh);
    RTE_LOG(INFO, PMD, "tx_deferred_start: %d\n", txconf->tx_deferred_start);
    RTE_LOG(INFO, PMD, "tx_offloads: 0x%lx\n", txconf->offloads);

    return 0;
}

static const struct rte_eth_conf default_eth_conf = {
    .link_speeds = ETH_LINK_SPEED_AUTONEG, /* auto negotiate speed */
    /*.link_duplex = ETH_LINK_AUTONEG_DUPLEX,	[> auto negotiation duplex <]*/
    .lpbk_mode = 0,
    .rxmode =
        {
            .mq_mode        = ETH_MQ_RX_RSS, /* Use RSS without DCB or VMDQ */
            .max_rx_pkt_len = 0,             /* valid only if jumbo is on */
            .split_hdr_size = 0,             /* valid only if HS is on */
            .offloads       = 0,             /* offloads api */
        },
    .txmode =
        {
            .mq_mode                 = ETH_MQ_TX_NONE, /* Disable DCB and VMDQ */
            .hw_vlan_reject_tagged   = 0,
            .hw_vlan_reject_untagged = 0,
            .hw_vlan_insert_pvid     = 0,
            .offloads                = 0,
        },
    .rx_adv_conf.rss_conf =
        {
            .rss_hf  = ETH_RSS_IP | ETH_RSS_UDP | ETH_RSS_TCP | ETH_RSS_SCTP,
            .rss_key = NULL,
        },
    // turned flow director off ~ : https://doc.dpdk.org/dts/test_plans/fdir_test_plan.html
    .fdir_conf =
        {
            .mode = RTE_FDIR_MODE_NONE,
        },
    /* No interrupt */
    .intr_conf =
        {
            .lsc = 0,
        },
};

int num_pmd_ports() {
    return rte_eth_dev_count_avail();
}

int get_pmd_ports(struct rte_eth_dev_info *info, int len) {
    int num_ports   = rte_eth_dev_count_avail();
    int num_entries = MIN(num_ports, len);
    for (int i = 0; i < num_entries; i++) {
        memset(&info[i], 0, sizeof(struct rte_eth_dev_info));
        rte_eth_dev_info_get(i, &info[i]);
    }
    return num_entries;
}

int get_rte_eth_dev_info(int dev, struct rte_eth_dev_info *info) {
    if (dev >= rte_eth_dev_count_avail()) {
        return -ENODEV;
    } else {
        rte_eth_dev_info_get(dev, info);
        return 0;
    }
}

int max_rxqs(int dev) {
    struct rte_eth_dev_info info;
    if (get_rte_eth_dev_info(dev, &info) != 0) {
        return -ENODEV;
    } else {
        return info.max_rx_queues;
    }
}

int max_txqs(int dev) {
    struct rte_eth_dev_info info;
    if (get_rte_eth_dev_info(dev, &info) != 0) {
        return -ENODEV;
    } else {
        return info.max_tx_queues;
    }
}

void enumerate_pmd_ports() {
    int num_dpdk_ports = rte_eth_dev_count_avail();

    printf("%d DPDK PMD ports have been recognized:\n", num_dpdk_ports);
    for (int i = 0; i < num_dpdk_ports; i++) {
        struct rte_eth_dev_info dev_info;
        const struct rte_pci_device *pci_dev;

        memset(&dev_info, 0, sizeof(dev_info));
        rte_eth_dev_info_get(i, &dev_info);

        printf("DPDK port_id %d (%s)   RXQ %hu TXQ %hu  ", i, dev_info.driver_name,
               dev_info.max_rx_queues, dev_info.max_tx_queues);

        pci_dev = RTE_DEV_TO_PCI(dev_info.device);

        if (pci_dev) {
            printf("%04x:%02hhx:%02hhx.%02hhx %04hx:%04hx  ", pci_dev->addr.domain, pci_dev->addr.bus,
                   pci_dev->addr.devid, pci_dev->addr.function, pci_dev->id.vendor_id, pci_dev->id.device_id);
        }

        printf("\n");
    }
}

static void assert_link_status(int port_id) {
    struct rte_eth_link link;
    uint8_t rep_cnt = MAX_REPEAT_TIMES;

    memset(&link, 0, sizeof(link));
    do {
        rte_eth_link_get(port_id, &link);
        if (link.link_status == ETH_LINK_UP)
            break;
        rte_delay_ms(CHECK_INTERVAL);
    } while (--rep_cnt);

    if (link.link_status == ETH_LINK_DOWN)
        rte_exit(EXIT_FAILURE, ":: error: link is still down\n");
}

int init_pmd_port(int port, int rxqs, int txqs, int rxq_core[], int txq_core[], int nrxd, int ntxd,
                  int loopback, int tso, int csumoffload) {
    struct rte_eth_dev_info dev_info = {};
    struct rte_eth_conf eth_conf;
    struct rte_eth_rxconf eth_rxconf;
    struct rte_eth_txconf eth_txconf;
    int ret, i;

    /* Need to access rte_eth_devices manually since DPDK currently
     * provides no other mechanism for checking whether something is
     * attached */
    if (port >= RTE_MAX_ETHPORTS || (rte_eth_devices[port].state != RTE_ETH_DEV_ATTACHED)) {
        RTE_LOG(WARNING, PMD, "attach_port: null pointers not allowed\n");
        return -ENODEV;
    }

    eth_conf           = default_eth_conf;
    eth_conf.lpbk_mode = !(!loopback);

    /* Use default rx/tx configuration as provided by PMD drivers,
     * with minor tweaks */
    rte_eth_dev_info_get(port, &dev_info);
    eth_conf.rx_adv_conf.rss_conf.rss_hf = dev_info.flow_type_rss_offloads;
    if (csumoffload) {
        eth_conf.txmode.offloads = DEV_TX_OFFLOAD_IPV4_CKSUM | DEV_TX_OFFLOAD_UDP_CKSUM | DEV_TX_OFFLOAD_TCP_CKSUM;
        eth_conf.rxmode.offloads = DEV_RX_OFFLOAD_IPV4_CKSUM | DEV_RX_OFFLOAD_UDP_CKSUM | DEV_RX_OFFLOAD_TCP_CKSUM;
    }
    eth_rxconf = dev_info.default_rxconf;

    /* Drop packets when no descriptors are available */
    // eth_rxconf.rx_drop_en = 0; // changed that to 0, because 82574L seems not supporting this
    // eth_rxconf.rx_drop_en = 1;
    // eth_rxconf.rx_thresh.pthresh=RX_PTHRESH;
    // eth_rxconf.rx_thresh.hthresh=RX_HTHRESH;
    // eth_rxconf.rx_thresh.wthresh=RX_WTHRESH;
    // eth_rxconf.rx_free_thresh=RX_FREE_THRESH;

    eth_txconf  = dev_info.default_txconf;
    tso         = !(!tso);
    csumoffload = !(!csumoffload);

    ret = rte_eth_dev_configure(port, rxqs, txqs, &eth_conf);

    rte_eth_dev_info_get(port, &dev_info);

    // Logs on Init
    /* RTE_LOG(DEBUG, PMD, "--- rte_eth_dev_info:\n"); */
    /* log_eth_dev_info(&dev_info); */
    /* RTE_LOG(DEBUG, PMD, "--- rte_eth_conf:\n"); */
    /* log_eth_conf(&eth_conf); */
    /* RTE_LOG(DEBUG, PMD, "--- default eth_rxconf:\n"); */
    /* log_eth_rxconf(&dev_info.default_rxconf); */
    /* RTE_LOG(DEBUG, PMD, "--- using eth_rxconf:\n"); */
    /* log_eth_rxconf(&eth_rxconf); */
    /* RTE_LOG(DEBUG, PMD, "--- using eth_txconf:\n"); */
    /* log_eth_txconf(&eth_txconf); */

    if (ret != 0) {
        RTE_LOG(CRIT, PMD, "Failed to configure port \n");
        return ret; /* Don't need to clean up here */
    }

    /* Set to promiscuous mode */
    rte_eth_promiscuous_enable(port);

    for (i = 0; i < rxqs; i++) {
        int sid = rte_lcore_to_socket_id(rxq_core[i]);
        ret = rte_eth_rx_queue_setup(port, i, nrxd, sid, &eth_rxconf, get_pframe_pool(rxq_core[i], sid));
        if (ret != 0) {
            RTE_LOG(CRIT, PMD, "Failed to initialize rxq\n");
            return ret; /* Clean things up? */
        }
    }

    for (i = 0; i < txqs; i++) {
        int sid = rte_lcore_to_socket_id(txq_core[i]);
        ret     = rte_eth_tx_queue_setup(port, i, ntxd, sid, &eth_txconf);
        if (ret != 0) {
            RTE_LOG(CRIT, PMD, "Failed to initialize txq\n");
            return ret; /* Clean things up */
        }
    }

    ret = rte_eth_dev_start(port);
    if (ret != 0) {
        RTE_LOG(CRIT, PMD, "Failed to configure port \n");
        return ret; /* Clean up things */
    }

    assert_link_status(port);
    RTE_LOG(INFO, PMD, "pmd port %d configured successfully\n", port);

    return 0;
}

void free_pmd_port(int port) {
    rte_eth_dev_stop(port);
    rte_eth_dev_close(port);
}

int recv_pkts(int port, int qid, mbuf_array_t pkts, int len) {
    int ret = rte_eth_rx_burst(port, qid, (struct rte_mbuf **)pkts, len);
/* Removed prefetching since the benefit in performance for single core was
 * outweighed by the loss in performance with several cores. */
#if 0
    for (int i = 0; i < ret; i++) {
        rte_prefetch0(rte_pktmbuf_mtod(pkts[i], void*));
    }
#endif
    return ret;
}

int send_pkts(int port, int qid, mbuf_array_t pkts, int len) {
    return rte_eth_tx_burst(port, (uint16_t)qid, (struct rte_mbuf **)pkts, (uint16_t)len);
}

int find_port_with_pci_address(const char *pci) {
    struct rte_pci_addr addr;
    char devargs[1024];
    uint8_t port_id;
    struct rte_dev_iterator iterator;

    // Cannot parse address
    if (eal_parse_pci_DomBDF(pci, &addr) != 0 && eal_parse_pci_BDF(pci, &addr) != 0) {
        return -1;
    }

    int n_devices = rte_eth_dev_count_avail();

    for (int i = 0; i < n_devices; i++) {
        struct rte_eth_dev_info dev_info;
        const struct rte_pci_device *pci_dev;

        rte_eth_dev_info_get(i, &dev_info);

        pci_dev = RTE_DEV_TO_PCI(dev_info.device);

        if (pci_dev) {
            if (rte_eal_compare_pci_addr(&addr, &pci_dev->addr)) {
                return i;
            }
        }
    }

    /* If not found, maybe the device has not been attached yet */
    snprintf(devargs, 1024, "%04x:%02x:%02x.%02x", addr.domain, addr.bus, addr.devid, addr.function);

    RTE_ETH_FOREACH_MATCHING_DEV(port_id, devargs, &iterator) {
        /* If a break is done - must call rte_eth_iterator_cleanup. */
        rte_eth_iterator_cleanup(&iterator);
        break;
    }

    return (int)port_id;
}

int attach_device(char *identifier, int *portid_ptr, int max_ports) {
    int pi;
    struct rte_dev_iterator iterator;

    if (identifier == NULL || portid_ptr == NULL) {
        RTE_LOG(WARNING, PMD, "attach_port: null pointers not allowed\n");
        return -1;
    }

    // RTE_LOG(DEBUG, PMD, "trying to attach device %s\n", identifier);

    if (rte_dev_probe(identifier) != 0) {
        RTE_LOG(WARNING, PMD, "Failed to attach device %s\n", identifier);
        return -ENODEV;
    }

    int *ptr;
    ptr       = portid_ptr;
    int count = 0;
    RTE_ETH_FOREACH_MATCHING_DEV(pi, identifier, &iterator) {
        /* setup ports matching the devargs used for probing */
        *ptr = pi;
        ptr++;
        count++;
        if (count >= max_ports)
            break;
    }
    return count;
}

/* FIXME: Add function to modify RSS hash function using
 * rte_eth_dev_rss_hash_update */
