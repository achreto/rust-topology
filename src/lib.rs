//! Interface to query information about the underlying hardware.
//!
//! Our naming scheme follows the Intel x86/ACPI convention
//! which has a thread/core/package/NUMA node hierarchy:
//!
//! * thread: Hardware scheduling unit (has an APIC, is an app or core bsp core)
//! * core: one or more threads (usually 2)
//! * package: one or more cores (usually a socket with a shared LLC)
//! * affinity region: a NUMA node (consists of a bunch of threads/core/packages and memory regions)
//!
//! Intel Topology is a pretty complicated subject (unfortunately), relevant documentation is here:
//! * https://software.intel.com/en-us/articles/intel-64-architecture-processor-topology-enumeration/
//! * https://acpica.org/documentation
#![cfg_attr(target_os = "none", no_std)]

extern crate alloc;

#[cfg(target_os = "none")]
mod acpi;
mod acpi_types;

use alloc::vec::Vec;
#[cfg(target_os = "none")]
use core::convert::TryInto;
use core::fmt;

use lazy_static::lazy_static;

#[cfg(target_arch = "x86_64")]
#[path = "arch/x86.rs"]
mod arch;

#[cfg(target_arch = "aarch64")]
#[path = "arch/aarch64.rs"]
mod arch;

#[cfg(target_os = "none")]
pub use arch::MACHINE_TOPOLOGY;

#[cfg(target_os = "none")]
use acpi::{process_madt, process_msct, process_nfit, process_srat};
pub use acpi_types::MemoryType;
use acpi_types::{
    IoApic, LocalApic, LocalX2Apic, MaximumProximityDomainInfo, MaximumSystemCharacteristics,
    MemoryAffinity, MemoryDescriptor,
};

/// A system global ID for a CPU.
pub type GlobalThreadId = usize;

/// A hardware scheduling unit (has an APIC), (unique within a core).
pub type ThreadId = usize;

/// A core, with one or more threads (unique within a packet).
pub type CoreId = usize;

/// A socket with one or more cores (usually with a shared LLC).
pub type PackageId = usize;

/// Affinity region, a NUMA node (consists of a bunch of threads/core/packages and memory regions).
pub type NodeId = usize;

/// Differentiate between local APICs and X2APICs.
#[derive(Eq, PartialEq, Debug, Ord, PartialOrd, Clone, Copy)]
pub enum HwId {
    Apic(u32),
    X2Apic(u32),
    Mpid(u32),
}

impl HwId {
    fn id(&self) -> u32 {
        match &self {
            HwId::Apic(apic) => *apic,
            HwId::X2Apic(x2apic) => *x2apic,
            HwId::Mpid(mpid) => *mpid,
        }
    }
}

/// Represents an SMT thread in the system.
#[derive(Ord, PartialOrd)]
pub struct Thread {
    /// ID the thread, global within a system.
    pub id: GlobalThreadId,
    /// ID of the NUMA node (system global)
    pub node_id: Option<NodeId>,
    /// ID of the package (system global)
    pub package_id: PackageId,
    /// ID of the core
    pub core_id: CoreId,
    /// ID of the thread (usually between 0..1)
    pub thread_id: ThreadId,
    /// Thread is represented either by a LocalApic or LocalX2Apic entry.
    hwid: HwId,
}

impl PartialEq for Thread {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Thread {}

impl fmt::Debug for Thread {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Thread")
            .field("id", &self.id)
            .field("apic_id", &self.hwid.id())
            .field(
                "thread/core/package",
                &(self.thread_id, self.core_id, self.package_id),
            )
            .field("numa_node", &self.node_id)
            .finish()
    }
}

impl Thread {
    /// Construct a thread with a LocalApic struct.
    fn new_with_apic(global_id: GlobalThreadId, apic: u32, node_id: Option<NodeId>) -> Thread {
        let hwid = HwId::Apic(apic);

        // We use `get_topology_from_x2apic_id` which should still work (in most
        // cases) if it holds that all lower x2apic ids are identical to apic
        // ids below 256. I can't seem to get to the right AMD topology with the
        // regular xapic cpuid code and unfortunately I can't find much
        // documentation on how to do it for AMD with the 0x8000_0008 leaf.
        // So this will have to do:
        let (thread_id, core_id, package_id) = arch::get_topology(hwid);

        Thread {
            id: global_id,
            hwid,
            thread_id,
            core_id,
            package_id,
            node_id,
        }
    }

    /// Construct a thread with a LocalX2Apic struct.
    fn new_with_x2apic(global_id: GlobalThreadId, apic: u32, node_id: Option<NodeId>) -> Thread {
        let hwid = HwId::X2Apic(apic);
        let (thread_id, core_id, package_id) = arch::get_topology(hwid);

        Thread {
            id: global_id,
            hwid,
            thread_id,
            core_id,
            package_id,
            node_id,
        }
    }

    fn new_with_mpid(global_id: GlobalThreadId, mpid: u32, node_id: Option<NodeId>) -> Thread {
        let hwid = HwId::Mpid(mpid);
        let (thread_id, core_id, package_id) = arch::get_topology(hwid);

        Thread {
            id: global_id,
            hwid,
            thread_id,
            core_id,
            package_id,
            node_id,
        }
    }

    /// APIC ID (unique in the system).
    pub fn hw_id(&self) -> u32 {
        self.hwid.id()
    }

    /// All neighboring threads (on the same core)
    pub fn siblings(&'static self) -> impl Iterator<Item = &'static Thread> {
        MACHINE_TOPOLOGY
            .threads()
            // Find all threads of our parent core
            .filter(move |t| t.package_id == self.package_id && t.core_id == self.core_id)
            // Exclude self
            .filter(move |t| t != &self)
    }

    /// Return the `Core` this thread belongs to.
    pub fn core(&'static self) -> &'static Core {
        MACHINE_TOPOLOGY
            .cores()
            .find(move |core| core.package_id == self.package_id && core.id == self.core_id)
            .unwrap()
    }

    /// Return the `Package` this thread belongs to.
    pub fn package(&'static self) -> &'static Package {
        MACHINE_TOPOLOGY
            .packages()
            .find(move |package| package.id == self.package_id)
            .unwrap()
    }

    /// Return the NUMA node this thread belongs to.
    pub fn node(&'static self) -> Option<&'static Node> {
        self.node_id
            .and_then(|nid| MACHINE_TOPOLOGY.nodes().find(move |node| node.id == nid))
    }
}

/// Represents a core in the system.
///
/// # Note
/// The order is important here, when Ord is derived on structs, it will produce a
/// lexicographic ordering based on the top-to-bottom declaration order of the struct's members.
#[derive(Eq, PartialEq, Debug, Ord, PartialOrd)]
pub struct Core {
    /// NUMA node
    pub node_id: Option<NodeId>,
    /// Package ID
    pub package_id: PackageId,
    /// Core ID (which is relative within the package).
    pub id: CoreId,
}

impl Core {
    fn new(node_id: Option<NodeId>, package_id: PackageId, id: CoreId) -> Core {
        Core {
            node_id,
            package_id,
            id,
        }
    }

    /// All neighboring cores (on the same core)
    pub fn siblings(&'static self) -> impl Iterator<Item = &'static Core> {
        MACHINE_TOPOLOGY
            .cores()
            // Find all cores on parent package
            .filter(move |c| c.package_id == self.package_id)
            // Exclude self
            .filter(move |c| c != &self)
    }

    /// All threads of the core.
    pub fn threads(&'static self) -> impl Iterator<Item = &'static Thread> {
        MACHINE_TOPOLOGY
            .threads()
            .filter(move |t| t.package_id == self.package_id && t.core_id == self.id)
    }

    /// Return the `Package` this core belongs to.
    pub fn package(&'static self) -> &'static Package {
        MACHINE_TOPOLOGY
            .packages()
            .find(move |package| package.id == self.package_id)
            .unwrap()
    }

    /// Return the NUMA node this core belongs to.
    pub fn node(&'static self) -> Option<&'static Node> {
        self.node_id
            .and_then(|nid| MACHINE_TOPOLOGY.nodes().find(move |node| node.id == nid))
    }
}

/// Represents a package/socket in the system.
#[derive(Eq, PartialEq, Debug, Ord, PartialOrd)]
pub struct Package {
    /// Package ID
    pub id: PackageId,
    /// NUMA node
    pub node_id: Option<NodeId>,
}

impl Package {
    fn new(id: PackageId, node_id: Option<NodeId>) -> Package {
        Package { id, node_id }
    }

    /// All packages of the machine.
    pub fn siblings(&'static self) -> impl Iterator<Item = &'static Package> {
        MACHINE_TOPOLOGY
            .packages()
            // Exclude self
            .filter(move |p| p != &self)
    }

    /// All threads of the package.
    pub fn threads(&'static self) -> impl Iterator<Item = &'static Thread> {
        MACHINE_TOPOLOGY
            .threads()
            .filter(move |t| t.package_id == self.id)
    }

    /// All cores of the package.
    pub fn cores(&'static self) -> impl Iterator<Item = &'static Core> {
        MACHINE_TOPOLOGY
            .cores()
            .filter(move |c| c.package_id == self.id)
    }

    /// Return the NUMA node this core belongs to.
    pub fn node(&'static self) -> Option<&'static Node> {
        self.node_id
            .and_then(|nid| MACHINE_TOPOLOGY.nodes().find(move |node| node.id == nid))
    }
}

/// Represents a NUMA node in the system.
#[derive(Eq, PartialEq, Debug, Ord, PartialOrd)]
pub struct Node {
    pub id: NodeId,
}

impl Node {
    /// Construct a node
    fn new(id: NodeId) -> Node {
        Node { id }
    }

    /// All NUMA nodes of the system.
    pub fn siblings(&self) -> impl Iterator<Item = &'static Node> {
        MACHINE_TOPOLOGY.nodes()
    }

    pub fn memory(&'static self) -> impl Iterator<Item = &'static MemoryAffinity> {
        MACHINE_TOPOLOGY
            .memory_affinity
            .iter()
            .filter(move |ma| ma.proximity_domain as NodeId == self.id)
    }

    /// All threads of the NUMA node.
    pub fn threads(&'static self) -> impl Iterator<Item = &'static Thread> {
        MACHINE_TOPOLOGY
            .threads()
            .filter(move |t| t.node_id == Some(self.id))
    }

    /// All cores of the NUMA node.
    pub fn cores(&'static self) -> impl Iterator<Item = &'static Core> {
        MACHINE_TOPOLOGY
            .cores()
            .filter(move |c| c.node_id == Some(self.id))
    }

    /// All packages of the NUMA node.
    pub fn packages(&'static self) -> impl Iterator<Item = &'static Package> {
        MACHINE_TOPOLOGY
            .packages()
            .filter(move |p| p.node_id == Some(self.id))
    }
}

#[cfg(not(target_os = "none"))]
lazy_static! {
    /// A struct that contains all information about current machine we're
    /// running on.
    ///
    /// This code is not running on baremetal, so we don't use ACPI but rely on
    /// procfs to parse some data.
    pub static ref MACHINE_TOPOLOGY: MachineInfo = {
        let ioapics = Vec::new();
        let memory_affinity = Vec::new();
        let max_proximity_info = MaximumSystemCharacteristics::default();
        let prox_domain_info = Vec::new();
        let pmem_descriptors = Vec::new();

        // Make Thread objects out of APIC MADT entries:
        let mut threads: Vec<Thread> = Vec::new();

        let cpuinfo = procfs::CpuInfo::new().expect("can't have cpuinfo");
        for res in cpuinfo.cpus {
            let t = Thread {
                id: res["processor"].parse::<usize>().unwrap(),
                node_id: None, // TODO: complete me
                package_id: 0, // TODO: complete me
                core_id: res["core id"].parse::<usize>().unwrap(),
                thread_id: 0, // TODO: complete me
                /// TODO: this field is probably not correct for armv8
                hwid: HwId::Apic(res["apicid"].parse::<u32>().unwrap()),
            };
            threads.push(t);
        }

        // Next, we can construct the cores, packages, and nodes from threads
        let mut cores: Vec<Core> = threads.iter().map(|t| Core::new(t.node_id, t.package_id, t.core_id)).collect();
        cores.sort_unstable();
        cores.dedup();

        // Gather all packages
        let mut packages: Vec<Package> = threads.iter().map(|t| Package::new(t.package_id, t.node_id)).collect();
        packages.sort_unstable();
        packages.dedup();

        // Gather all nodes
        let mut nodes: Vec<Node> = threads
            .iter()
            .filter(|t| t.node_id.is_some())
            .map(|t| Node::new(t.node_id.unwrap_or(0)))
            .collect::<Vec<Node>>();
        nodes.sort_unstable();
        nodes.dedup();

        MachineInfo::new(
            threads,
            cores,
            packages,
            nodes,
            ioapics,
            memory_affinity,
            pmem_descriptors,
            max_proximity_info,
            prox_domain_info
        )
    };
}

/// Contains a condensed and filtered version of all data queried from ACPI and CPUID.
#[derive(Debug)]
pub struct MachineInfo {
    /// All hardware threads in the system, indexed by GlobalThreadId.
    pub threads: Vec<Thread>,
    cores: Vec<Core>,
    packages: Vec<Package>,
    nodes: Vec<Node>,
    memory_affinity: Vec<MemoryAffinity>,
    pmem_descriptors: Vec<MemoryDescriptor>,
    io_apics: Vec<IoApic>,
    max_proximity_info: MaximumSystemCharacteristics,
    proximity_domains: Vec<MaximumProximityDomainInfo>,
}

impl MachineInfo {
    /// Create a MachineInfo struct from ACPI information.
    fn new(
        threads: Vec<Thread>,
        cores: Vec<Core>,
        packages: Vec<Package>,
        nodes: Vec<Node>,
        io_apics: Vec<IoApic>,
        memory_affinity: Vec<MemoryAffinity>,
        pmem_descriptors: Vec<MemoryDescriptor>,
        max_proximity_info: MaximumSystemCharacteristics,
        proximity_domains: Vec<MaximumProximityDomainInfo>,
    ) -> MachineInfo {
        MachineInfo {
            threads,
            cores,
            packages,
            nodes,
            memory_affinity,
            pmem_descriptors,
            io_apics,
            max_proximity_info,
            proximity_domains,
        }
    }

    /// Returns the current thread we're running on.
    ///
    /// # Notes
    /// Uses cpuid to determine the current APIC ID,
    /// then uses the id to find the corresponding thread.
    ///
    /// This is not an incredibly fast function since cpuid will clobber
    /// your registers unnecessarily. Ideally, call this once then cache.
    ///
    /// You also need to ensure that execution is not migrated to
    /// another core during execution of `current_thread`.
    pub fn current_thread(&'static self) -> &'static Thread {
        let apic_id = arch::determine_hw_id_with_cpuid();

        self.threads()
            .find(move |t| t.hw_id() == apic_id.id())
            .unwrap()
    }

    /// Return the amount of threads in the system.
    pub fn num_threads(&self) -> usize {
        self.threads.len()
    }

    /// Return iterator over all threads in the system.
    pub fn threads(&'static self) -> impl Iterator<Item = &Thread> {
        self.threads.iter()
    }

    /// Return the amount of cores in the system.
    pub fn num_cores(&self) -> usize {
        self.cores.len()
    }

    /// Return iterator over all cores in the system.
    pub fn cores(&'static self) -> impl Iterator<Item = &Core> {
        self.cores.iter()
    }

    /// Return the amount of packages in the system.
    pub fn num_packages(&self) -> usize {
        self.packages.len()
    }

    /// Return iterator over all packages in the system.
    pub fn packages(&'static self) -> impl Iterator<Item = &Package> {
        self.packages.iter()
    }

    /// Return iterator over all NUMA nodes in the system.
    pub fn nodes(&'static self) -> impl Iterator<Item = &Node> {
        self.nodes.iter()
    }

    /// Return the amount of NUMA nodes in the system.
    pub fn num_nodes(&self) -> usize {
        self.nodes.len()
    }

    /// Return an iterator over all I/O APICs in the system.
    pub fn io_apics(&'static self) -> impl Iterator<Item = &IoApic> {
        self.io_apics.iter()
    }

    pub fn persistent_memory(&'static self) -> impl Iterator<Item = &'static MemoryDescriptor> {
        MACHINE_TOPOLOGY.pmem_descriptors.iter()
    }
}

#[cfg(test)]
mod test {

    #[test]
    fn local_topo() {
        env_logger::try_init();
        log::info!("{:?}", super::MACHINE_TOPOLOGY.num_nodes());
    }
}
