# Monitor a process pid with psutil and emits a CSV.
import argparse
import csv
import sys
import time
import psutil

KEY = "Elapsed seconds"
BASE_METRICS = ["CPU (%)", "Real (MB)"]

parser = argparse.ArgumentParser()
parser.add_argument("pid", type=int)
parser.add_argument("--subseconds", action="store_true")
args = parser.parse_args()

sample_interval_secs = 0.1 if args.subseconds else 1
pid = args.pid
try:
    proc = psutil.Process(pid)
except psutil.NoSuchProcess:
    sys.exit(f"Error: process {pid} not found.")

print(f"Starting monitoring of {pid} pid", file=sys.stderr)

records = []
start = time.monotonic()
# ignore first result as per docs recommendation
# https://psutil.readthedocs.io/en/latest/#psutil.cpu_percent
proc.cpu_percent(None)

while True:
    try:
        if not proc.is_running():
            break
        time.sleep(sample_interval_secs)

        elapsed = time.monotonic() - start
        elapsed_secs = round(elapsed, 1) if args.subseconds else int(elapsed)
        cpu = proc.cpu_percent(None)
        meminfo = proc.memory_info()
        bytes_in_MB = 1024**2
        rss_mb = meminfo.rss / bytes_in_MB

        records.append(
            [
                str(elapsed_secs),
                f"{cpu:.3f}",
                f"{rss_mb:.3f}",
            ]
        )

    except psutil.NoSuchProcess:
        break

end = time.monotonic()
total_time = end - start
print(f"Finished {pid} pid monitoring in {total_time:.3f}", file=sys.stderr)

cols = [KEY] + BASE_METRICS
writer = csv.writer(sys.stdout, lineterminator="\n")
writer.writerow(cols)
writer.writerows(records)
