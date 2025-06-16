# Monitor a process (PID) with psutil and emit a Markdown table.
import sys
import time
import psutil
import pandas as pd

SAMPLE_INTERVAL_SECS = 1

if len(sys.argv) != 2 or not sys.argv[1].isdigit():
    sys.exit(f"Usage: {sys.argv[0]} <PID>")

pid = int(sys.argv[1])
try:
    proc = psutil.Process(pid)
except psutil.NoSuchProcess:
    sys.exit(f"Error: process {pid} not found.")

records = []
start = time.time()
# ignore first result as per docs recommendation
# https://psutil.readthedocs.io/en/latest/#psutil.cpu_percent
proc.cpu_percent(None)

while True:
    try:
        if not proc.is_running():
            break
        time.sleep(SAMPLE_INTERVAL_SECS)

        elapsed = time.time() - start
        cpu = proc.cpu_percent(None)
        mem_pct = proc.memory_percent()
        meminfo = proc.memory_info()
        bytes_in_MB = 1024**2
        rss_mb = meminfo.rss / bytes_in_MB
        vms_mb = meminfo.vms / bytes_in_MB

        records.append(
            [
                f"{elapsed:.3f}",
                f"{cpu:.3f}",
                f"{mem_pct:.3f}",
                f"{rss_mb:.3f}",
                f"{vms_mb:.3f}",
            ]
        )

    except psutil.NoSuchProcess:
        break

cols = ["Elapsed seconds", "CPU (%)", "MEM (%)", "Real (MB)", "Virtual (MB)"]
df = pd.DataFrame(records, columns=cols, dtype=str)
df.to_markdown(sys.stdout, index=False, tablefmt="github")
