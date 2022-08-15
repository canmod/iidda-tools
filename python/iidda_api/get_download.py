from github import Github
import requests
import os
from fastapi.responses import StreamingResponse
import configparser
from iidda_api import read_config, get_pipeline_dependencies, get_release_list
from io import BytesIO
import zipfile
import json
import aiohttp
import asyncio
from fastapi.responses import PlainTextResponse
from aiohttp_client_cache import CachedSession, FileBackend
from appdirs import *


async def get_download(dataset_name, version, resource=None):
    # Get access token
    ACCESS_TOKEN = read_config('access_token')
    # make cache directory
    cache_path = user_cache_dir("iidda-api-cache", "")
    if not os.path.isdir(cache_path):
        os.makedirs(cache_path)
    # Cache configurations
    release_list_cache = FileBackend(
        cache_name=cache_path + "/release_list"
    )

    releases = asyncio.run(get_release_list(
        ACCESS_TOKEN, release_list_cache, clear_cache=False))

    # filter through and sort all releases of this name ascending by version
    release_list = filter(
        lambda release: release['name'] == dataset_name, releases)
    release_list = sorted(
        release_list, key=lambda release: int(release['body'][8:]))

    # check if dataset is contained in repo
    if not release_list:
        return f"{dataset_name} does not exist in the releases"

    if version == "latest":
        # version_tag is for creating file names later on
        version_tag = ""
        version = len(release_list)
    else:
        version_tag = f"v{version}-"

    if int(version) > len(release_list):
        return f"The supplied version of {dataset_name} is greater than the latest version of {len(release_list)}"

    release = release_list[int(version) - 1]

    headers = {
        'Authorization': 'token ' + ACCESS_TOKEN,
        'Accept': 'application/octet-stream'
    }

    # make cache directory
    cache_path = user_cache_dir("iidda-api-cache", "")
    if not os.path.isdir(cache_path):
        os.makedirs(cache_path)
    # Cache configurations
    assets_cache = FileBackend(
        cache_name=cache_path + "/assets"
    )

    async def main():
        async with CachedSession(cache=assets_cache, headers=headers) as session:
            tasks = []
            if "pipeline_dependencies" in resource:
                task = asyncio.ensure_future(get_pipeline_dependencies(
                    dataset_name, version=version, version_tag=version_tag))
                tasks.append(task)
            for asset in release['assets']:
                if (asset['name'].endswith(".csv") and "csv" in resource) or (asset['name'].endswith(".json") and "metadata" in resource):
                    task = asyncio.ensure_future(download_asset(
                        asset['url'], asset['name'], session))
                    tasks.append(task)

            files = await asyncio.gather(*tasks)
            return files

    async def download_asset(url, asset_name, session):
        file_name = version_tag + dataset_name + "/" + version_tag + asset_name
        async with session.get(url) as response:
            if response.ok:
                file_content = await response.read()
                return (file_name, file_content)
            else:
                return "Download failed: {}\n{}".format(response.status_code, response.text)

    return asyncio.run(main())
