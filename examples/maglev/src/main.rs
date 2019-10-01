extern crate fnv;
#[macro_use]
extern crate lazy_static;
extern crate netbricks;
extern crate twox_hash;
use fnv::FnvHasher;
use netbricks::common::Result;
use netbricks::config::load_config;
use netbricks::interface::{PacketRx, PacketTx};
use netbricks::operators::{Batch, ReceiveBatch};
use netbricks::packets::ip::v4::Ipv4;
use netbricks::packets::ip::{Flow, IpPacket};
use netbricks::packets::{Ethernet, Packet, Tcp};
use netbricks::runtime::Runtime;
use netbricks::scheduler::Scheduler;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::{BuildHasher, BuildHasherDefault, Hash, Hasher};
use std::net::{IpAddr, Ipv4Addr};
use std::sync::{Arc, RwLock};
use std::{mem, slice};
use twox_hash::XxHash;

const ENTRY_NUM: usize = 65537;

type FnvHash = BuildHasherDefault<FnvHasher>;
type XxHashFactory = BuildHasherDefault<XxHash>;

lazy_static! {
    static ref FLOW_CACHE: Arc<RwLock<HashMap<usize, usize, FnvHash>>> = {
        let m = HashMap::<usize, usize, FnvHash>::with_hasher(Default::default());
        Arc::new(RwLock::new(m))
    };
    static ref BACKENDS: Vec<String> = {
        let mut v = Vec::new();
        v.push("Larry".to_string());
        v.push("Moe".to_string());
        v.push("Curly".to_string());
        v
    };
    static ref LUT: Maglev = Maglev::new(ENTRY_NUM);
}

trait Stamper {
    fn stamp_flow(&mut self, dst_ip: usize) -> Result<()>;
}

impl<E: IpPacket> Stamper for Tcp<E> {
    fn stamp_flow(&mut self, dst_ip: usize) -> Result<()> {
        self.set_dst_ip(IpAddr::V4(Ipv4Addr::new(
            ((dst_ip >> 24) & 0xFF) as u8,
            ((dst_ip >> 16) & 0xFF) as u8,
            ((dst_ip >> 8) & 0xFF) as u8,
            (dst_ip & 0xFF) as u8,
        )))?;
        Ok(())
    }
}

pub struct Maglev {
    lut: Vec<usize>,
    lut_size: usize,
}

impl Maglev {
    pub fn offset_skip_for_name(
        name: &str,
        h1: &FnvHash,
        h2: &XxHashFactory,
        lsize: usize,
    ) -> (usize, usize) {
        let mut fnv_state = h1.build_hasher();
        name.hash(&mut fnv_state);
        let hash1 = fnv_state.finish() as usize;
        let mut xx_state = h2.build_hasher();
        name.hash(&mut xx_state);
        let hash2 = xx_state.finish() as usize;
        let offset = hash2 % lsize;
        let skip = hash1 % (lsize - 1) + 1;
        (offset, skip)
    }

    pub fn generate_permutations(lsize: usize) -> Vec<Vec<usize>> {
        println!("Generating permutations");
        let fnv_hasher: FnvHash = Default::default();
        let xx_hasher: XxHashFactory = Default::default();
        BACKENDS
            .iter()
            .map(|n| Maglev::offset_skip_for_name(n, &fnv_hasher, &xx_hasher, lsize))
            .map(|(offset, skip)| (0..lsize).map(|j| (offset + j * skip) % lsize).collect())
            .collect()
    }

    fn generate_lut(permutations: Vec<Vec<usize>>, size: usize) -> Vec<usize> {
        let mut next: Vec<_> = permutations.iter().map(|_| 0).collect();
        let mut entry: Vec<usize> = (0..size).map(|_| 0x8000).collect();
        let mut n = 0;
        println!("Generating LUT");
        while n < size {
            for i in 0..next.len() {
                let mut c = permutations[i][next[i]];
                while entry[c as usize] != 0x8000 {
                    next[i] += 1;
                    c = permutations[i][next[i]];
                }
                if entry[c as usize] == 0x8000 {
                    entry[c as usize] = i as usize;
                    next[i] += 1;
                    n += 1;
                }
                if n >= size {
                    break;
                }
            }
        }
        println!("Done Generating LUT");
        entry
    }

    pub fn new(lsize: usize) -> Maglev {
        let permutations = Maglev::generate_permutations(lsize);
        Maglev {
            lut: Maglev::generate_lut(permutations, lsize),
            lut_size: lsize,
        }
    }

    #[inline]
    pub fn lookup(&self, hash: usize) -> usize {
        let idx = hash % self.lut_size;
        self.lut[idx]
    }

    #[inline]
    fn flow_as_u8(flow: &Flow) -> &[u8] {
        let size = mem::size_of::<Flow>();
        unsafe { slice::from_raw_parts((flow as *const Flow) as *const u8, size) }
    }

    #[inline]
    fn flow_hash(flow: &Flow) -> usize {
        let mut hasher = FnvHasher::default();
        hasher.write(Maglev::flow_as_u8(flow));
        hasher.finish() as usize
    }
}

fn install<T, S>(ports: Vec<T>, sched: &mut S)
where
    T: PacketRx + PacketTx + Display + Clone + 'static,
    S: Scheduler + Sized,
{
    println!("Receiving started");

    let pipelines: Vec<_> = ports
        .iter()
        .map(move |port| {
            ReceiveBatch::new(port.clone())
                .map(|p| {
                    let mut ethernet = p.parse::<Ethernet>()?;
                    ethernet.swap_addresses();
                    let v4 = ethernet.parse::<Ipv4>()?;
                    let mut tcp = v4.parse::<Tcp<Ipv4>>()?;
                    let hash = Maglev::flow_hash(&tcp.flow());
                    let mut lock = FLOW_CACHE.write().unwrap();
                    let assigned = lock.entry(hash).or_insert_with(|| LUT.lookup(hash));
                    tcp.stamp_flow(*assigned)?;
                    Ok(tcp)
                })
                .send(port.clone())
        })
        .collect();

    println!("Running {} pipelines", pipelines.len());
    for pipeline in pipelines {
        sched.add_task(pipeline).unwrap();
    }
}

fn main() -> Result<()> {
    let configuration = load_config()?;
    println!("{}", configuration);
    let mut runtime = Runtime::init(&configuration)?;
    runtime.add_pipeline_to_run(|ports, sched| install(ports, sched));
    runtime.execute()
}
