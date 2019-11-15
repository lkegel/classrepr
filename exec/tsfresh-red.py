import sys
import json
import tsfresh
import pandas as pd

def red(dataset, n_jobs):
    extracted_features = tsfresh.extract_features(dataset, column_id="id", column_sort="time",
    disable_progressbar = False, show_warnings = False, n_jobs = n_jobs)
    return extracted_features

if __name__ == "__main__":
    dataset = pd.read_json(sys.stdin)
    dataset["time"] = pd.to_datetime(dataset["time"], format="%Y-%m-%d %H:%M:%S", utc = True)
    #dataset = dataset.set_index(["id", "time"], verify_integrity = True)
    print(dataset.head(), flush = True)
    print(dataset.tail(), flush = True)
    # print(dataset.index)
    n_jobs = int(sys.argv[2])
    if n_jobs == 1:
      n_jobs = 0
    print("Start Extraction", flush = True)
    extracted_features = red(dataset, n_jobs)
    extracted_features.to_csv(sys.argv[1], na_rep = 'NA')
