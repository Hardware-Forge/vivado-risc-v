
import os
import re
import datetime

workspace_dir = "workspace"
base_dir = "/home/user/vivado-risc-v"

print(f"{'Config':<50} | {'Metric':<10} | {'Value':<10} | {'Source File':<80} | {'Timestamp'}")
print("-" * 180)

def verify_file(config_name, file_type, file_path, pattern_str_list):
    if not os.path.exists(file_path):
        print(f"{config_name:<50} | {file_type:<10} | {'MISSING':<10} | {file_path:<80} | -")
        return None

    timestamp = datetime.datetime.fromtimestamp(os.path.getmtime(file_path)).strftime('%Y-%m-%d %H:%M:%S')
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    found_val = "N/A"
    for pattern_str in pattern_str_list:
        match = re.search(pattern_str, content)
        if match:
            found_val = match.group(1)
            break
            
    # Clean up value (remove whitespace)
    if found_val != "N/A":
        found_val = found_val.strip()

    print(f"{config_name:<50} | {file_type:<10} | {found_val:<10} | {os.path.basename(file_path):<80} | {timestamp}")
    return found_val

configs = sorted([d for d in os.listdir(workspace_dir) if os.path.isdir(os.path.join(workspace_dir, d))])

for config in configs:
    # Paths
    impl_dir = os.path.join(workspace_dir, config, "vivado-genesys2-riscv", "genesys2-riscv.runs", "impl_1")
    util_rpt = os.path.join(impl_dir, "riscv_wrapper_utilization_placed.rpt")
    power_rpt = os.path.join(impl_dir, "riscv_wrapper_power_routed.rpt")
    log_file = os.path.join(impl_dir, "runme.log")

    # 1. Check Utilization (Standard)
    # Pattern: | Slice LUTs | 1234 | ...
    luts = verify_file(config, "LUTs", util_rpt, [r'\|\s*Slice LUTs\s*\|\s*(\d+)\s*\|'])
    regs = verify_file(config, "Regs", util_rpt, [r'\|\s*Slice Registers\s*\|\s*(\d+)\s*\|'])

    # 2. Check Power (Standard)
    power = verify_file(config, "Power", power_rpt, [r'\|\s*Total\s*\|\s*([\d\.]+)\s*\|'])

    # 3. Check Log (Failure fallback or double check)
    # Only strictly necessary if util report failed, but good to see what exists
    if luts is None or luts == "N/A":
        # Check log for failure estimates
        # Luts: 219190 (combined)
        verify_file(config, "Log-LUTs", log_file, [r'Luts:\s*(\d+)\s*\(combined\)'])
        verify_file(config, "Log-Regs", log_file, [r'Flip flops:\s*(\d+),'])

    print("-" * 180)
