import os
import sys
import pandas as pd

KEY = "Elapsed seconds"

merged = None

paths = [p.strip() for p in sys.stdin.read().split() if p.strip()]

for csv_path in paths:
    # br is branch (variable shortened to pass linter)
    br = os.path.splitext(os.path.basename(csv_path))[0]

    df = pd.read_csv(csv_path)

    if KEY not in df.columns:
        sys.exit(f"{csv_path} is missing the {KEY} column")

    # add branch marker to every metric column
    df = df.rename(columns={c: f"{c} [{br}]" for c in df.columns if c != KEY})

    # outer join so missing rows appear
    merged = df if merged is None else merged.merge(df, on=KEY, how="outer")

# replace nan with empty string
merged = merged.fillna("")
merged.to_markdown(sys.stdout, index=False, tablefmt="github")
