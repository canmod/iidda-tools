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
from aiohttp_client_cache import CachedSession, FileBackend
from appdirs import *

async def get_dataset(dataset_name, version):
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

    # make cache directory
    cache_path = user_cache_dir("iidda-api-cache","")
    if not os.path.isdir(cache_path):
        os.makedirs(cache_path)
    # Cache configurations
    assets_cache = FileBackend(
        cache_name = cache_path  + "/assets"
    )

    for asset in release.get_assets():
        if asset.name == dataset_name + '.csv':
            async with CachedSession(cache=assets_cache, headers=headers) as session:
                async with session.get(asset.url) as response:
                    if response.ok:
                        file_content = await response.read()
                        return BytesIO(file_content)
                    else:
                        return "Download failed: {}\n{}".format(response.status_code, response.text)

