from github import Github
import requests
import os
from fastapi.responses import StreamingResponse
import configparser
from iidda_api import read_config, get_pipeline_dependencies
from io import BytesIO
import zipfile
import json
from fastapi.responses import PlainTextResponse

def get_dataset(dataset_name, version, response_type):
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
        return "This dataset does not exist in the releases"
    
    if version == "latest":
        version = len(release_list)
    
    if int(version) > len(release_list):
        return f"The supplied version is greater than the latest version. The latest version is {len(release_list)}"

    release = release_list[int(version) - 1]

    headers = {
        'Authorization': 'token ' + ACCESS_TOKEN,
        'Accept': 'application/octet-stream'
    }

    if response_type == "github_url":
        return {"github_url": release.html_url}
 
    elif response_type == "raw_csv":
        for asset in release.get_assets():
            if asset.name == dataset_name + '.csv':
                response = requests.get(asset.url, stream=True, headers=headers)
                if response.ok:
                    return PlainTextResponse(response.content, media_type="text/plain")
                else:
                    return "Download failed: {}\n{}".format(response.status_code, response.text)

    elif response_type == "metadata":
        for asset in release.get_assets():
            if asset.name == dataset_name + '.json':
                response = requests.get(asset.url, stream=True, headers=headers)
                if response.ok:
                    return json.loads(response.content)
                else:
                    return "Download failed: {}\n{}".format(response.status_code, response.text)

    elif response_type == "csv_dialect" or response_type == "data_dictionary":
        for asset in release.get_assets():
            if asset.name == dataset_name + "_" + response_type + '.json':
                response = requests.get(asset.url, stream=True, headers=headers)
                if response.ok:
                    return json.loads(response.content)
                else:
                    return "Download failed: {}\n{}".format(response.status_code, response.text)
                        
