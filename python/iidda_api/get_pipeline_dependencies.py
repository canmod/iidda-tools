from github import Github
import requests
import os
import configparser
from iidda_api import read_config
import aiohttp
import asyncio

def convert_to_raw(url):
    return url.replace("blob", "raw")

def get_pipeline_dependencies(dataset_name, version="latest"):
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
        return False

    if version == "latest":
        version = len(release_list)
    
    if int(version) > len(release_list):
        print("The supplied version is greater than the latest version. Downloading the latest version...")
        version = len(release_list)

    release = release_list[int(version) - 1]

    headers = {
        'Authorization': 'token ' + ACCESS_TOKEN,
        'Accept': 'application/octet-stream'
    }

    dependency_links = dict()
    for asset in release.get_assets():
        if asset.name == dataset_name + ".json":
            response = requests.get(asset.url, stream=True, headers=headers)
            if response.ok:
                dataset_metadata = response.json()
                for relatedIdentifier in dataset_metadata['relatedIdentifiers']:
                    if relatedIdentifier['relatedIdentifierType'] == "URL" and relatedIdentifier['relationType'] == "IsSourceOf":
                        if isinstance(relatedIdentifier['relatedIdentifier'], list):
                            for link in relatedIdentifier['relatedIdentifier']:
                                file_name = os.path.basename(link[19:])
                                dependency_links[file_name] = convert_to_raw(link)
                        else:
                            file_name = os.path.basename(relatedIdentifier['relatedIdentifier'][19:])
                            dependency_links[file_name] = convert_to_raw(relatedIdentifier['relatedIdentifier'])
            else:
                print("Failure in getting assets from GitHub {}\n{}".format(response.status_code, response.text))
    
    return dependency_links
