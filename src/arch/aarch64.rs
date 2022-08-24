//! Interface to query information about the underlying hardware.
//! AArch64-specific information.

use crate::{CoreId, HwId, PackageId, ThreadId};

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
        panic!("NYI");
    };
}
