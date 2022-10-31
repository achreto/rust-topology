//! Interface to query information about the underlying hardware.
//! AArch64-specific information.

use alloc::vec::Vec;
use alloc::vec;

use crate::{CoreId, Core, HwId, MaximumSystemCharacteristics, PackageId, Package, ThreadId, Thread};

#[cfg(target_os = "none")]
use crate::MachineInfo;

#[cfg(target_os = "none")]
use lazy_static::lazy_static;

pub fn determine_hw_id_with_cpuid() -> HwId {
    panic!("NYI");
}

pub fn get_topology(hwid: HwId) -> (ThreadId, CoreId, PackageId) {
    panic!("NYI");
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

        MachineInfo::new(
            vec![
                Thread {
                    id: 0, node_id: None, package_id: 0, core_id: 0, thread_id: 0, hwid: HwId::Mpid(0)
                }
            ],
            vec![
                Core::new(Some(0), 0, 0)
            ],
            vec![
                Package::new(0, None)
            ],
            vec![],
            vec![],
            vec![],
            vec![],
            MaximumSystemCharacteristics::default(),
            vec![]
        )
    };
}
