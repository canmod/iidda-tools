from github import Github
import requests
import os
import configparser
from iidda_api import generate_config
import json

def get_dataset_list(download_path, all_metadata=False):
    # Read config file and obtain access token
    config_obj = configparser.ConfigParser()
    config_obj.read(generate_config.path)
    config_github = config_obj["github_info"]

    ACCESS_TOKEN = config_github["access_token"]

    github = Github(ACCESS_TOKEN)
    repo = github.get_repo('canmod/iidda-test-assets')

    # Retrieve list of releases
    releases = list(repo.get_releases())

    # Create list of unique dataset titles
    dataset_title_list = map(lambda release: release.title, releases)
        
    dataset_title_list = dict.fromkeys(dataset_title_list)

    # Retrieve latest version of each dataset
    
    headers = {
        'Authorization': 'token ' + ACCESS_TOKEN,
        'Accept': 'application/octet-stream'
    }
    
    for title in dataset_title_list:
        # Search for latest version
        title_version_list = filter(lambda release: release.title == title, releases)
        title_version_list = sorted(title_version_list, key=lambda release: int(release.body[8:]))
        latest_version = title_version_list[len(title_version_list) - 1]
        
        # get metadata
        latest_version_metadata = latest_version.get_assets()
        latest_version_metadata = list(filter(lambda release: release.name == title + '.json', latest_version_metadata))
        
        if latest_version_metadata != []:
            response = requests.get(latest_version_metadata[0].url, stream=True, headers=headers)
            if response.ok:
                meta_data = json.loads(response.text)
                if all_metadata == False:
                    dataset_title_list[title] = {'identifier': meta_data['identifier']}
                elif all_metadata == True:
                    dataset_title_list[title] = meta_data
            else:
                print("Download failed: {}\n{}".format(response.status_code, response.text))
        else:
            dataset_title_list[title] = 'No metadata.'
            
    path = download_path + "/" + 'data set list' + "/" + 'dataset_list.json'

    # make directory if it doesn't exist
    os.makedirs(os.path.dirname(path), exist_ok=True)
    
    with open(path, "w") as file:
        file.write(json.dumps(dataset_title_list, indent=4))
            
        
