use check::{check, egress_check, ingress_check};
use netbricks::headers::*;
use netbricks::operators::*;
use netbricks::scheduler::*;
use std::fs::File;

struct Meta {
    newv6: Ipv6Header,
}

pub fn nf<T: 'static + Batch<Header = NullHeader>, S: Scheduler + Sized>(
    parent: T,
    sched: &mut S,
) -> CompositionBatch {
    let mut groups = parent.parse::<MacHeader>().group_by(
        2,
        box |pkt| match pkt.get_payload().len() as u32 > IPV6_MIN_MTU {
            true => {
                assert_eq!(pkt.get_payload().len() + MacHeader::size(), pkt.data_len());
                0
            }
            _ => 1,
        },
        sched,
    );

    let toobig = groups.get_group(0).unwrap();
    let otherwise = groups.get_group(1).unwrap();

    merge(vec![send_too_big(toobig), pass(otherwise)]).compose()
}

#[check(IPV6_MIN_MTU = 1280)]
fn send_too_big<T: 'static + Batch<Header = MacHeader>>(parent: T) -> CompositionBatch {
    parent
        .pre(box |pkt| {
            flame::start("send too big");
            flame::start("pre");

            ingress_check! {
            input: pkt,
            order: [MacHeader => Ipv6Header => TcpHeader<Ipv6Header>],
            checks: [(payload_len[Ipv6Header] as u32, gt, IPV6_MIN_MTU)]
            }

            flame::end("pre");
            ()
        })
        .transform(box |pkt| {
            flame::start("mac_transform");
            let mach = pkt.get_mut_header();
            mach.swap_addresses();
            flame::end("mac_transform");
            ()
        })
        .parse::<Ipv6Header>()
        .transform(box |pkt| {
            flame::start("v6_transform");
            assert_eq!(
                pkt.get_header().payload_len() as usize,
                pkt.get_payload().len()
            );
            // create new ipv6 header from current header and store in metadata
            let ipv6h_src = pkt.get_header().src();
            let ipv6h_dst = pkt.get_header().dst();
            let mut ipv6h_new = Ipv6Header::new();
            ipv6h_new.set_src(ipv6h_dst);
            ipv6h_new.set_dst(ipv6h_src);
            ipv6h_new.set_payload_len(IPV6_TOO_BIG_PAYLOAD_LEN);
            ipv6h_new.set_next_header(NextHeader::Icmp);
            flame::start("write metadata");
            pkt.write_metadata({ &Meta { newv6: ipv6h_new } }).unwrap();
            flame::end("write metadata");
            flame::end("v6_transform");
            ()
        })
        // We reset and begin process of going from invalid Ipv6 packet into an
        // Icmpv6 - Packet Too Big one.
        //
        // Note: Due to PreviousHeader assoc. type needs, the better bet was to
        // push a new Ipv6 and Icmpv6 Header onto the packet, and let the rest
        // push downward, as we need it as part of the Icmpv6 payload.
        // Then, we trim at the end to get to the allotted set of bytes ~ 1294.
        .reset()
        .parse::<MacHeader>()
        .transform(box |pkt| {
            flame::start("->icmp_v6_insert");
            let ipv6h_new = pkt.emit_metadata::<Meta>().newv6;
            pkt.insert_header(NextHeader::NoNextHeader, &ipv6h_new)
                .unwrap();
            flame::end("->icmp_v6_insert");
            ()
        })
        .parse::<Ipv6Header>()
        .transform(box |pkt| {
            flame::start("->icmp_insert_icmp");
            // Write IcmpHeader
            let icmpv6 = <Icmpv6PktTooBig<Ipv6Header>>::new();
            // push icmpc6header
            pkt.insert_header(NextHeader::Icmp, &icmpv6).unwrap();
            flame::end("->icmp_insert_icmp");
            ()
        })
        .parse::<Icmpv6PktTooBig<Ipv6Header>>()
        .transform(box |pkt| {
            flame::start("->icmp_transform");
            let payload_len = pkt.get_payload().len();
            // Trim Invalid Ipv6 -> End payload until we reach IPV6_MIN_MTU
            pkt.trim_payload_size(
                payload_len
                    - ((IPV6_TOO_BIG_PAYLOAD_LEN as usize) - <Icmpv6PktTooBig<Ipv6Header>>::size()),
            );
            // Assure that we're 1240 - 8 (8 being the icmpv6 pkt too big size)
            assert_eq!(pkt.get_payload().len(), 1232);
            // Generate Checksum for Packet
            // TODO: Is this Offloadable?
            flame::start("->icmp_checksum_calc");
            let segment_length = pkt.segment_length(Protocol::Icmp);
            flame::start("emit metadata");
            let ipv6h_new = pkt.emit_metadata::<Meta>().newv6;
            let icmpv6_toobig = pkt.get_mut_header();
            flame::end("emit metadata");
            icmpv6_toobig.icmp.update_v6_checksum(
                segment_length,
                ipv6h_new.src(),
                ipv6h_new.dst(),
                Protocol::Icmp,
            );
            flame::end("->icmp_checksum_calc");
            flame::end("->icmp_transform");
            ()
        })
        .post(box |pkt| {
            flame::start("post");
            egress_check! {
                input: pkt,
                order: [MacHeader => Ipv6Header => Icmpv6PktTooBig<Ipv6Header>],
                checks: [(checksum[Icmpv6PktTooBig], neq, checksum[TcpHeader<Ipv6Header>]),
                         (payload_len[Ipv6Header], eq, 1240),
                         (src[Ipv6Header], eq, dst[Ipv6Header]),
                         (dst[Ipv6Header], eq, src[Ipv6Header]),
                         (.src[MacHeader], eq, .dst[MacHeader]),
                         (.dst[MacHeader], eq, .src[MacHeader])
                ]
            }
            flame::end("post");
            flame::end("send too big");
            flame::dump_html(&mut File::create("flame-graph.html").unwrap()).unwrap();
            ()
        })
        .compose()
}

fn pass<T: 'static + Batch<Header = MacHeader>>(parent: T) -> CompositionBatch {
    parent.compose()
}
