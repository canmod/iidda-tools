from github import Github
import requests
import os
from fastapi.responses import StreamingResponse
import configparser
from iidda_api import read_config, get_pipeline_dependencies
from io import BytesIO, StringIO
import zipfile
import json
from fastapi.responses import PlainTextResponse

def get_dataset(dataset_name, version):
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
        return f"{dataset_name} does not exist in the releases"
    
    if version == "latest":
        version = len(release_list)
    
    if int(version) > len(release_list):
        return f"The supplied version of {dataset_name} is greater than the latest version of {len(release_list)}"

    release = release_list[int(version) - 1]

    headers = {
        'Authorization': 'token ' + ACCESS_TOKEN,
        'Accept': 'application/octet-stream'
    }

    for asset in release.get_assets():
        if asset.name == dataset_name + '.csv':
            response = requests.get(asset.url, stream=True, headers=headers)
            if response.ok:
                return BytesIO(response.content)
            else:
                return "Download failed: {}\n{}".format(response.status_code, response.text)

