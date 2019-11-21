import sys
import json
import tsfresh
import pandas as pd

def select(extracted_features, y, n_jobs):
    features_filtered = tsfresh.select_features(extracted_features, y, n_jobs = n_jobs,
    ml_task = 'classification')

    return features_filtered

if __name__ == "__main__":
    dataset = pd.read_csv(sys.argv[1])
    extracted_features = dataset.loc[:, dataset.columns != 'y']
    y = dataset['y']
    n_jobs = int(sys.argv[3])
    if n_jobs == 1:
      n_jobs = 0
    features_filtered = select(extracted_features, y, n_jobs)
    features_filtered.to_csv(sys.argv[2], na_rep = 'NA')
