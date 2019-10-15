#[macro_use]
extern crate netbricks;
extern crate fallible_iterator;

use fallible_iterator::FallibleIterator;
use netbricks::common::Result;
use netbricks::config::load_config;
use netbricks::interface::{PacketRx, PacketTx};
use netbricks::operators::{Batch, ReceiveBatch};
use netbricks::packets::icmp::v6::ndp::{NdpOptions, NdpPacket};
use netbricks::packets::icmp::v6::Icmpv6Message;
use netbricks::packets::icmp::v6::Icmpv6Parse;
use netbricks::packets::ip::v6::Ipv6;
use netbricks::packets::ip::v6::Ipv6Packet;
use netbricks::packets::ip::ProtocolNumbers;
use netbricks::packets::{EtherTypes, Ethernet, Packet};
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
                .map(|p| p.parse::<Ipv6>())
                .group_by(
                    |ipv6| ipv6.next_header(),
                    |groups| {
                        compose! {
                            groups,
                            ProtocolNumbers::Icmpv6 => |group| {
                                group.filter_map(process_icmpv6)
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
            if let NdpOptions::SourceLinkLayerAddress(_option_type) = option {
                //process options here
            }
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
