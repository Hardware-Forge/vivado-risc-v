
import os
import re
from datetime import datetime

def parse_utilization(file_path):
    lut_pattern = re.compile(r'\|\s*Slice LUTs\s*\|\s*(\d+)\s*\|')
    reg_pattern = re.compile(r'\|\s*Slice Registers\s*\|\s*(\d+)\s*\|')
    luts = "N/A"
    regs = "N/A"
    try:
        with open(file_path, 'r') as f:
            content = f.read()
            lut_match = lut_pattern.search(content)
            reg_match = reg_pattern.search(content)
            if lut_match: luts = lut_match.group(1)
            if reg_match: regs = reg_match.group(1)
    except Exception as e:
        pass
    return luts, regs

def parse_power(file_path):
    power_pattern = re.compile(r'\|\s*Total\s*\|\s*([\d\.]+)\s*\|')
    power = "N/A"
    try:
        with open(file_path, 'r') as f:
            content = f.read()
            match = power_pattern.search(content)
            if match: power = match.group(1)
    except Exception as e:
        pass
    return power

def parse_failure_log(log_path):
    # Try to find utilization table in log even if failed
    # Matches: "Luts: 219190 (combined) 243836 (total), available capacity: 203800"
    lut_fail_pattern = re.compile(r'Luts:\s*(\d+)\s*\(combined\)')
    # Matches: "Flip flops: 124576, available capacity: 407600"
    reg_fail_pattern = re.compile(r'Flip flops:\s*(\d+),')
    
    luts = "N/A"
    regs = "N/A"
    try:
        with open(log_path, 'r') as f:
            content = f.read()
            lut_match = lut_fail_pattern.search(content)
            reg_match = reg_fail_pattern.search(content)
            if lut_match: luts = lut_match.group(1)
            if reg_match: regs = reg_match.group(1)
    except Exception:
        pass
    return luts, regs

workspace_dir = "workspace"
data = {}

# 1. Collect Data
for config in sorted(os.listdir(workspace_dir)):
    config_path = os.path.join(workspace_dir, config)
    if os.path.isdir(config_path):
        util_rpt = os.path.join(config_path, "vivado-genesys2-riscv", "genesys2-riscv.runs", "impl_1", "riscv_wrapper_utilization_placed.rpt")
        power_rpt = os.path.join(config_path, "vivado-genesys2-riscv", "genesys2-riscv.runs", "impl_1", "riscv_wrapper_power_routed.rpt")
        log_file = os.path.join(config_path, "vivado-genesys2-riscv", "genesys2-riscv.runs", "impl_1", "runme.log")
        
        luts = 0
        regs = 0
        power = 0.0
        status = "MISSING/FAILED"
        is_estimated = False
        
        # Try reading standard report
        if os.path.exists(util_rpt):
            l, r = parse_utilization(util_rpt)
            if l != "N/A": 
                luts = int(l)
                regs = int(r) if r != "N/A" else 0
                status = "OK"
        
        # If report missing or invalid, try log file
        if status != "OK" and os.path.exists(log_file):
            l, r = parse_failure_log(log_file)
            if l != "N/A":
                luts = int(l)
                regs = int(r) if r != "N/A" else 0
                status = "ESTIMATED"
                is_estimated = True

        if os.path.exists(power_rpt):
            p = parse_power(power_rpt)
            if p != "N/A": power = float(p)
            
        data[config] = {
            "luts": luts,
            "regs": regs,
            "power": power,
            "status": status,
            "is_estimated": is_estimated
        }

# 2. Identify Baseline
baseline_name = "rocket64x1_backup"
if baseline_name not in data or data[baseline_name]["status"] == "MISSING/FAILED":
    candidates = [k for k in data if "Without" in k and data[k]["status"] == "OK"]
    if candidates:
        baseline_name = candidates[0]
    else:
        ok_configs = [k for k in data if data[k]["status"] == "OK"]
        if ok_configs:
            baseline_name = sorted(ok_configs, key=lambda x: data[x]["luts"])[0]

print(f"# Comparative Analysis Report")
print(f"**Baseline Configuration**: `{baseline_name}`\n")

if baseline_name in data:
    base_luts = data[baseline_name]["luts"]
    base_regs = data[baseline_name]["regs"]
    base_power = data[baseline_name]["power"]
else:
    base_luts = 0
    base_regs = 0
    base_power = 0

# 3. Generate Table
print(f"| Configuration | Slice LUTs | Slice Registers | Total Power (W) |")
print(f"| :--- | :--- | :--- | :--- |")

for config in sorted(data.keys()):
    entry = data[config]
    if entry["status"] == "MISSING/FAILED":
        print(f"| `{config}` | FAILED | FAILED | FAILED |")
        continue

    luts = entry["luts"]
    regs = entry["regs"]
    power = entry["power"]
    est_mark = "*" if entry["is_estimated"] else ""
    
    # LUTs
    lut_str = str(luts)
    if base_luts > 0:
        diff = luts - base_luts
        pct = (diff / base_luts) * 100
        lut_str = f"{luts} ({pct:+.2f}%)"
    lut_str += est_mark
        
    # Registers
    reg_str = str(regs)
    if base_regs > 0:
        diff = regs - base_regs
        pct = (diff / base_regs) * 100
        reg_str = f"{regs} ({pct:+.2f}%)"
    reg_str += est_mark

    # Power
    power_str = f"{power:.3f}"
    if base_power > 0 and power > 0:
        diff = power - base_power
        pct = (diff / base_power) * 100
        power_str = f"{power:.3f} ({pct:+.2f}%)"
    elif power == 0:
        power_str = "N/A"

    print(f"| `{config}` | {lut_str} | {reg_str} | {power_str} |")

print("\n*\*Values estimated from failure logs due to congestion.*")
