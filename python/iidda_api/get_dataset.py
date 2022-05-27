from github import Github
import requests
import os
import configparser
from iidda_api import read_config

def get_dataset(dataset_name, download_path, version="latest", metadata=False):
    # Get access token
    ACCESS_TOKEN = read_config('access_token')
    github = Github(ACCESS_TOKEN)
    repo = github.get_repo(read_config('repository'))

    # filter through and sort all releases of this name ascending by version
    release_list = list(
        filter(lambda release: release.title == dataset_name, repo.get_releases()))
    release_list = sorted(
        release_list, key=lambda release: int(release.body[8:])
    )
    
    # check if dataset is contained in repo
    if not release_list:
        print("This dataset does not exist in the releases")
        return
    
    if version == "latest":
        version = len(release_list)
    
    if version > len(release_list):
        print("The supplied version is greater than the latest version. Downloading the latest version...")
        version = len(release_list)

    release = release_list[version - 1]

    headers = {
        'Authorization': 'token ' + ACCESS_TOKEN,
        'Accept': 'application/octet-stream'
    }

    for asset in release.get_assets():
        if asset.name.endswith(".csv") or (asset.name.endswith(".json") and metadata):
            response = requests.get(asset.url, stream=True, headers=headers)
            if response.ok:
                path = download_path + "/" + release.title + "/" + asset.name

                # make directory if it doesn't exist
                os.makedirs(os.path.dirname(path), exist_ok=True)
                with open(path, "wb") as file:
                    file.write(response.content)
            else:
                print("Download failed: {}\n{}".format(response.status_code, response.text))
