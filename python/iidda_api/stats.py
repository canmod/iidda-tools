import json
import os
from iidda_api import read_config
from appdirs import *
from pathlib import Path
from json.decoder import JSONDecodeError
from collections import defaultdict

# generate path
def stats_path():
    directory_path = user_data_dir("iidda-api","")
    if not os.path.isdir(directory_path):
        os.makedirs(directory_path)

    path = directory_path + '/stats.json'
    return path

def write_stats(endpoint, datasets=None, repo=read_config('repository')):
    path = Path(stats_path())
    path.touch(exist_ok=True)
    with open(stats_path(), "r") as file:
        try:
            stats = json.load(file)
        except JSONDecodeError:
            stats = dict()

    def nested_dd():
        return defaultdict(nested_dd)


    def convert_to_default(d):
        res = nested_dd()
        for key, value in d.items():
            if isinstance(value, dict):
                res[key] = convert_to_default(value)
            else:
                res[key] = value
        return res

    stats = convert_to_default(stats)
    
    # Incrementing total API calls
    try:
        stats[repo]["total api calls"] += 1
    except:
        stats[repo]["total api calls"] = 1

    # Incrementing total datasets accessed
    if datasets != None:
        try:
            stats[repo]["total datasets accessed"] += len(datasets)
        except:
            stats[repo]["total datasets accessed"] = len(datasets)

    # Incrementing endpoint
    try:
        stats[repo]["endpoints"][endpoint] += 1
    except:
        stats[repo]["endpoints"][endpoint] = 1
    
    # Incrementing dataset 
    if datasets != None:
        for dataset in datasets:
            try:
                stats[repo]["datasets"][dataset] += 1
            except:
                stats[repo]["datasets"][dataset] = 1


    with open(stats_path(), 'w+') as file:
        json.dump(stats, file, indent=4)
    


