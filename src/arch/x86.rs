//! Interface to query information about the underlying hardware.
//! X86-specific information.

use alloc::vec::Vec;
use core::convert::TryInto;

extern crate x86;

use crate::{CoreId, HwId, PackageId, ThreadId};

#[cfg(target_os = "none")]
use crate::{
    process_madt, process_msct, process_nfit, process_srat, Core, GlobalThreadId, MachineInfo,
    Node, Package, Thread,
};

#[cfg(target_os = "none")]
use lazy_static::lazy_static;

/// Silly helper trait for computing power of two
trait PowersOf2 {
    fn log2(self) -> u8;
}

impl PowersOf2 for u8 {
    fn log2(self) -> u8 {
        7 - self.leading_zeros() as u8
    }
}

fn cpuid_bits_needed(count: u8) -> u8 {
    let mut mask: u8 = 0x80;
    let mut cnt: u8 = 8;

    while (cnt > 0) && ((mask & count) != mask) {
        mask >>= 1;
        cnt -= 1;
    }

    cnt
}

fn get_processor_limits() -> (u8, u8) {
    let cpuid = x86::cpuid::CpuId::new();

    // This is for AMD processors:
    if let Some(info) = cpuid.get_processor_capacity_feature_info() {
        // This is how I think it's supposed to work, but doesn't quite work,
        // that's why we use the x2apic code path to determine topology:
        let max_logical_processor_ids = info.num_phys_threads();
        let smt_max_cores_for_package = info.maximum_logical_processors();

        return (
            max_logical_processor_ids.try_into().unwrap(),
            smt_max_cores_for_package.try_into().unwrap(),
        );
    }
    // This is for Intel processors:
    else if let Some(cparams) = cpuid.get_cache_parameters() {
        let max_logical_processor_ids = cpuid
            .get_feature_info()
            .map_or_else(|| 1, |finfo| finfo.max_logical_processor_ids());

        let mut smt_max_cores_for_package: u8 = 0;
        for (ecx, cache) in cparams.enumerate() {
            if ecx == 0 {
                smt_max_cores_for_package = cache.max_cores_for_package() as u8;
            }
        }

        return (
            max_logical_processor_ids as u8,
            smt_max_cores_for_package as u8,
        );
    }

    unreachable!("Example doesn't support this CPU")
}

/// Given APIC ID, figure out package, core and thread ID.
pub fn get_topology_from_apic_id(xapic_id: u8) -> (ThreadId, CoreId, PackageId) {
    let (max_logical_processor_ids, smt_max_cores_for_package) = get_processor_limits();

    let smt_mask_width: u8 = cpuid_bits_needed(
        (max_logical_processor_ids.next_power_of_two() / smt_max_cores_for_package) - 1,
    );
    let smt_select_mask: u8 = !(u8::max_value() << smt_mask_width);
    let core_mask_width: u8 = cpuid_bits_needed(smt_max_cores_for_package - 1);
    let core_only_select_mask =
        (!(u8::max_value() << (core_mask_width + smt_mask_width))) ^ smt_select_mask;
    let pkg_select_mask = u8::max_value() << (core_mask_width + smt_mask_width);

    let smt_id = xapic_id & smt_select_mask;
    let core_id = (xapic_id & core_only_select_mask) >> smt_mask_width;
    let pkg_id = (xapic_id & pkg_select_mask) >> (core_mask_width + smt_mask_width);

    (smt_id.into(), core_id.into(), pkg_id.into())
}

/// Given x2APIC ID, figure out package, core and thread ID.
pub fn get_topology_from_x2apic_id(x2apic_id: u32) -> (ThreadId, CoreId, PackageId) {
    use x86::cpuid::{ExtendedTopologyLevel, TopologyType};
    let cpuid = x86::cpuid::CpuId::new();
    let mut smt_x2apic_shift: u32 = 0;
    let mut core_x2apic_shift: u32 = 0;

    cpuid.get_extended_topology_info().map_or_else(
        || (/* No topology information available */),
        |topoiter| {
            let topology: Vec<ExtendedTopologyLevel> = topoiter.collect();
            for topolevel in topology.iter() {
                match topolevel.level_type() {
                    TopologyType::SMT => {
                        smt_x2apic_shift = topolevel.shift_right_for_next_apic_id();
                    }
                    TopologyType::Core => {
                        core_x2apic_shift = topolevel.shift_right_for_next_apic_id();
                    }
                    _ => panic!("Topology category not supported."),
                };
            }
        },
    );

    let smt_select_mask = !(u32::max_value() << smt_x2apic_shift);
    let core_select_mask = (!((u32::max_value()) << core_x2apic_shift)) ^ smt_select_mask;
    let pkg_select_mask = u32::max_value() << core_x2apic_shift;

    let smt_id = x2apic_id & smt_select_mask;
    let core_id = (x2apic_id & core_select_mask) >> smt_x2apic_shift;
    let pkg_id = (x2apic_id & pkg_select_mask) >> core_x2apic_shift;

    (
        smt_id.try_into().unwrap(),
        core_id.try_into().unwrap(),
        pkg_id.try_into().unwrap(),
    )
}

pub fn determine_hw_id_with_cpuid() -> HwId {
    let cpuid = x86::cpuid::CpuId::new();
    let xapic_id: Option<u8> = cpuid
        .get_feature_info()
        .as_ref()
        .map(|finfo| finfo.initial_local_apic_id());

    let x2apic_id: Option<u32> = cpuid
        .get_extended_topology_info()
        .and_then(|mut topiter| topiter.next().as_ref().map(|t| t.x2apic_id()));

    match (x2apic_id, xapic_id) {
        (None, None) => {
            unreachable!("Can't determine APIC ID, bad. (Maybe try fallback on APIC_BASE_MSR")
        }
        (Some(x2id), None) => HwId::X2Apic(x2id),
        (None, Some(xid)) => HwId::Apic(xid as u32),
        (Some(x2id), Some(xid)) => {
            // 10.12.8.1 Consistency of APIC IDs and CPUID: "Initial APIC ID (CPUID.01H:EBX[31:24]) is always equal to CPUID.0BH:EDX[7:0]."
            debug_assert!(
                (x2id & 0xff) == xid.into(),
                "xAPIC ID is first byte of X2APIC ID"
            );
            if (xid as u32) == x2id {
                HwId::Apic(xid as u32)
            } else {
                HwId::X2Apic(x2id)
            }
        }
    }
}

pub fn get_topology(hwid: HwId) -> (ThreadId, CoreId, PackageId) {
    match hwid {
        HwId::Apic(apic) => get_topology_from_x2apic_id(apic),
        HwId::X2Apic(x2apic) => get_topology_from_x2apic_id(x2apic),
        _ => panic!("Unsupported hardware ID"),
    }
}

#[cfg(target_os = "none")]
lazy_static! {
    /// A struct that contains all information about current machine we're
    /// running on (discovered from ACPI Tables and cpuid).
    ///
    /// Should have some of the following:
    /// - Cores, NUMA nodes, Memory regions
    /// - Interrupt routing (I/O APICs, overrides) (TODO)
    /// - PCIe root complexes (TODO)
    ///
    /// # Note
    /// Not `no_global_oom_handling` safe, low priority as this allocates early
    /// during init and is static after (no hotplug support atm).
    pub static ref MACHINE_TOPOLOGY: MachineInfo = {
        use log::debug;

        // Let's get all the APIC information and transform it into a MachineInfo struct
        let (mut local_apics, mut local_x2apics, ioapics) = process_madt();
        let (mut core_affinity, mut x2apic_affinity, memory_affinity) = process_srat();
        let (max_proximity_info, prox_domain_info) = process_msct();
        let pmem_descriptors = process_nfit(x86::bits64::paging::BASE_PAGE_SIZE);

        local_apics.sort_unstable_by(|a, b| a.apic_id.cmp(&b.apic_id));
        local_x2apics.sort_unstable_by(|a, b| a.apic_id.cmp(&b.apic_id));

        // These to are sorted in decending order since we pop from the stack:
        core_affinity.sort_unstable_by(|a, b| b.apic_id.cmp(&a.apic_id));
        x2apic_affinity.sort_unstable_by(|a, b| b.x2apic_id.cmp(&a.x2apic_id));

        assert!(local_apics.len() == core_affinity.len() || core_affinity.is_empty(),
            "Either we have matching entries for core in affinity table or no affinity information at all.");

        // Make Thread objects out of APIC MADT entries:
        let mut global_thread_id: GlobalThreadId = 0;
        let mut threads = Vec::with_capacity(local_apics.len() + local_x2apics.len());

        // Add all local APIC entries
        for local_apic in local_apics {

            // Try to figure out which proximity domain (NUMA node) a thread belongs to:
            let mut proximity_domain = None;
            if !core_affinity.is_empty() {
                let affinity_entry = core_affinity.pop();
                if affinity_entry.as_ref().unwrap().apic_id == local_apic.apic_id {
                    proximity_domain = affinity_entry.as_ref().map(|a| a.proximity_domain as usize);
                }
                else {
                    core_affinity.push(affinity_entry.unwrap());
                }
            }

            // Cores with IDs < 255 appear as local apic entries, cores above
            // 255 appear as x2apic entries. However, for SRAT entries (to
            // figure out NUMA affinity), some machines will put all entries as
            // X2APIC affinities :S. So we have to check the x2apic_affinity too
            if proximity_domain.is_none() && !x2apic_affinity.is_empty() {
                let affinity_entry = x2apic_affinity.pop();
                let x2apic_id: u8 = affinity_entry.as_ref().unwrap().x2apic_id.try_into().unwrap();
                if x2apic_id == local_apic.apic_id {
                    proximity_domain = affinity_entry.as_ref().map(|a| a.proximity_domain as usize);
                }
                else {
                    x2apic_affinity.push(affinity_entry.unwrap());
                }
            }

            let t = Thread::new_with_apic(global_thread_id, local_apic.apic_id as u32, proximity_domain);
            debug!("Found {:?}", t);
            threads.push(t);
            global_thread_id += 1;
        }

        // Add all x2APIC entries
        for local_x2apic in local_x2apics {
            let affinity = x2apic_affinity.pop();
            if let Some(affinity_entry) = affinity.as_ref() {
                assert_eq!(affinity_entry.x2apic_id, local_x2apic.apic_id, "The x2apic_affinity and local_x2apic are not in the same order?");
            }
            let t = Thread::new_with_x2apic(global_thread_id, local_x2apic.apic_id, affinity.map(|a| a.proximity_domain as usize));
            debug!("Found {:?}", t);
            threads.push(t);
            global_thread_id += 1;
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

impl core::convert::From<HwId> for x86::apic::ApicId {
    fn from(hw_id: HwId) -> x86::apic::ApicId {
        match hw_id {
            HwId::Apic(id) => x86::apic::ApicId::XApic(id.try_into().unwrap()),
            HwId::X2Apic(id) => x86::apic::ApicId::X2Apic(id),
            _ => panic!("Unsupported hardware id"),
        }
    }
}
