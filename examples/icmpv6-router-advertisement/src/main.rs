#[macro_use]
extern crate netbricks;
extern crate fallible_iterator;

use fallible_iterator::FallibleIterator;
use netbricks::common::Result;
use netbricks::config::load_config;
use netbricks::interface::{PacketRx, PacketTx};
use netbricks::operators::{Batch, ReceiveBatch};
use netbricks::packets::icmp::v6::ndp::NdpPacket;
use netbricks::packets::icmp::v6::Icmpv6Message;
use netbricks::packets::icmp::v6::Icmpv6Parse;
use netbricks::packets::icmp::v6::{Icmpv6, PacketTooBig};
use netbricks::packets::ip::v6::{Ipv6, Ipv6Packet, IPV6_MIN_MTU};
use netbricks::packets::ip::ProtocolNumbers;
use netbricks::packets::{EtherTypes, Ethernet, EthernetHeader, Fixed, Packet};
use netbricks::runtime::Runtime;
use netbricks::scheduler::Scheduler;
use std::fmt::Display;

fn install<T, S>(ports: Vec<T>, sched: &mut S)
where
    T: PacketRx + PacketTx + Display + Clone + 'static,
    S: Scheduler + Sized,
{
    println!("Receiving started");

    let pipelines: Vec<_> = ports
        .iter()
        .map(|port| {
            ReceiveBatch::new(port.clone())
                .map(|p| p.parse::<Ethernet>())
                .filter(|p| p.ether_type() == EtherTypes::Ipv6)
                .group_by(
                    |ipv6| ipv6.next_header(),
                    |groups| {
                        compose! {
                            groups,
                            ProtocolNumbers::Icmpv6 => |group| {
                                group.filter_map(process_icpv6).emit()
                            },

                            _ => |group| {
                                group.filter(|_| {
                                    false
                                })
                            }
                        }
                    },
                )
                .send(port.clone())
        })
        .collect();

    println!("Running {} pipelines", pipelines.len());

    for pipeline in pipelines {
        sched.add_task(pipeline).unwrap();
    }
}

pub fn process_icmpv6(ipv6: Ipv6) -> Result<Option<Ipv6>> {
    if let Ok(Icmpv6Message::RouterAdvertisement(advert)) = ipv6.parse_icmpv6() {
        let mut iter = advert.options();
        while let Ok(Some(option)) = iter.next() {
            if let NdpOptions::SourceLinkLayerAddress(option_type) = option {}
        }
    }
    Ok(None)
}

fn main() -> Result<()> {
    let configuration = load_config()?;
    println!("{}", configuration);
    let mut runtime = Runtime::init(&configuration)?;
    runtime.add_pipeline_to_run(install);
    runtime.execute()
}
