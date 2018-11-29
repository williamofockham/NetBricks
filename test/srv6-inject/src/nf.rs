use colored::*;
use generic_array::typenum::*;
use generic_array::GenericArray;
use netbricks::headers::*;
use netbricks::interface::*;
use netbricks::operators::*;
use netbricks::utils::*;
use std::default::Default;
use std::net::Ipv6Addr;
use std::str::FromStr;
use CACHE;

#[allow(dead_code)]
enum NewSegmentsAction {
    Prepend,
    Append,
    Overwrite,
}

#[derive(Debug)]
struct MetaDataz {
    flow: FlowV6,
    payload_diff: i8,
    segment_dst: Ipv6Addr,
}

impl Default for MetaDataz {
    fn default() -> MetaDataz {
        MetaDataz {
            flow: FlowV6::default(),
            payload_diff: 0,
            segment_dst: Ipv6Addr::UNSPECIFIED,
        }
    }
}

fn srh_change_packet(
    pkt: &mut Packet<SRH<Ipv6Header>, MetaDataz>,
    mut segvec: Vec<Ipv6Addr>,
    seg_action: NewSegmentsAction,
) -> Option<isize> {
    match seg_action {
        NewSegmentsAction::Append => {
            segvec.splice(0..0, pkt.get_header().segments().unwrap().iter().cloned());
            ()
        }
        NewSegmentsAction::Prepend => {
            segvec.extend_from_slice(pkt.get_header().segments().unwrap())
        }
        NewSegmentsAction::Overwrite => (),
    }

    srh_swap!(segvec,
              pkt,
              (
                  pkt.get_header().next_header(),
                  pkt.get_header().segments_left(),
                  pkt.get_header().flags(),
                  pkt.get_header().tag()
              ),
              Ipv6Header,
              [1 => U1, 2=> U2, 3 => U3, 4 => U4, 5 => U5, 6 => U6, 7 => U7, 8 => U8, 9 => U9, 10 => U10, 11 => U11, 12 => U12])
}

#[inline]
fn tcp_sr_nf<T: 'static + Batch<Header = Ipv6Header>>(parent: T) -> CompositionBatch {
    parent
        .metadata(box |pkt| {
            let v6h = pkt.get_header();
            let flow = v6h.flow().unwrap();

            MetaDataz {
                flow: FlowV6 {
                    src_port: flow.src_port,
                    dst_port: flow.dst_port,
                    proto: flow.proto,
                    ..Default::default()
                },
                ..Default::default()
            }
        })
        .parse::<SRH<Ipv6Header>>()
        .transform(box |pkt| {
            if let Some(payload_diff) = srh_change_packet(
                pkt,
                vec![Ipv6Addr::from_str("fe80::a").unwrap()],
                NewSegmentsAction::Prepend,
            ) {
                let flow = pkt.read_metadata().flow;
                let segments_left = pkt.get_header().segments_left();
                let segments = pkt.get_header().segments().unwrap().to_vec();

                pkt.write_metadata({
                    &MetaDataz {
                        flow: flow,
                        payload_diff: payload_diff as i8,
                        segment_dst: segments[segments_left as usize],
                        ..Default::default()
                    }
                }).unwrap();
            }
        })
        .parse::<TcpHeader<SRH<Ipv6Header>>>()
        .reset()
        .parse::<MacHeader>()
        .parse::<Ipv6Header>()
        .metadata(box |pkt| {
            // Bring back metadata for regular read.
            MetaDataz {
                flow: pkt.emit_metadata::<MetaDataz>().flow,
                payload_diff: pkt.emit_metadata::<MetaDataz>().payload_diff,
                segment_dst: pkt.emit_metadata::<MetaDataz>().segment_dst,
                ..Default::default()
            }
        })
        .transform(box |pkt| {
            let curr_payload_len = pkt.get_header().payload_len();
            let segment_dst = pkt.read_metadata().segment_dst;
            let payload_diff = pkt.read_metadata().payload_diff;

            let v6h = pkt.get_mut_header();
            v6h.set_dst(segment_dst);
            v6h.set_payload_len((curr_payload_len as i8 + payload_diff) as u16);
        })
        .parse::<SRH<Ipv6Header>>()
        .map(box |pkt| {
            let cache = &mut *CACHE.write().unwrap();
            cache
                .entry(flow_hash(&Flows::V6(pkt.read_metadata().flow)))
                .or_insert(pkt.get_header().segments().unwrap().to_vec());
        })
        .parse::<TcpHeader<SRH<Ipv6Header>>>()
        .compose()
}

pub fn nf<T: 'static + Batch<Header = NullHeader>>(parent: T) -> CompositionBatch {
    let pipeline = parent
        .parse::<MacHeader>()
        .filter(box |pkt| match pkt.get_header().etype() {
            Some(EtherType::IPv6) => true,
            _ => false,
        })
        .parse::<Ipv6Header>()
        .filter(box |pkt| match pkt.get_header().next_header() {
            Some(NextHeader::Routing) => true,
            Some(NextHeader::Tcp) => true,
            _ => false,
        });
    tcp_sr_nf(pipeline)
}
