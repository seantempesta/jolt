/* static-code-share-helper.c: how much executable memory this process holds
   privately, against what it shares copy-on-write (macOS; see
   static-code-share.ss). Walks the task's regions with mach_vm_region and sums
   the resident pages of every executable region by its share mode. */
#include <mach/mach.h>
#include <mach/mach_vm.h>

static long resident_exec_kb(int want_private) {
  mach_vm_address_t a = 0;
  long kb = 0;
  for (;;) {
    mach_vm_size_t s;
    vm_region_extended_info_data_t e;
    mach_msg_type_number_t c = VM_REGION_EXTENDED_INFO_COUNT;
    mach_port_t o;
    if (mach_vm_region(mach_task_self(), &a, &s, VM_REGION_EXTENDED_INFO, (vm_region_info_t)&e, &c, &o) != KERN_SUCCESS)
      break;
    if (e.protection & VM_PROT_EXECUTE) {
      int priv = e.share_mode == SM_PRIVATE || e.share_mode == SM_PRIVATE_ALIASED;
      if (priv == want_private) kb += (long)e.pages_resident * (long)(vm_page_size / 1024);
    }
    a += s;
  }
  return kb;
}

long static_code_share_private_exec_kb(void) { return resident_exec_kb(1); }
long static_code_share_shared_exec_kb(void) { return resident_exec_kb(0); }
