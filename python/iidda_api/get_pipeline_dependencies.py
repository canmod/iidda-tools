from github import Github
import requests
import os
import configparser
from iidda_api import read_config
import aiohttp
import asyncio
from io import BytesIO
from iidda_api import get_release_list
from fastapi.responses import StreamingResponse
import zipfile
from aiohttp_client_cache import CachedSession, FileBackend
from appdirs import *


def convert_to_raw(url):
    return url.replace("github.com", "raw.githubusercontent.com").replace("/blob/", "/")


async def get_pipeline_dependencies(dataset_name, version="latest", version_tag=""):
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

    for asset in release['assets']:
        if asset['name'] == dataset_name + ".json":
            response = requests.get(asset['url'], stream=True, headers=headers)
            if response.ok:
                dataset_metadata = response.json()

                async def main():
                    async with aiohttp.ClientSession(headers={'Authorization': 'token ' + ACCESS_TOKEN, 'Accept': 'application/vnd.github.v3.raw'}) as session:
                        tasks = []
                        for relatedIdentifier in dataset_metadata['relatedIdentifiers']:
                            if relatedIdentifier['relatedIdentifierType'] == "URL":
                                if isinstance(relatedIdentifier['relatedIdentifier'], list):
                                    for link in relatedIdentifier['relatedIdentifier']:
                                        url = convert_to_raw(link)
                                        task = asyncio.ensure_future(
                                            download_dependencies(url, session))
                                        tasks.append(task)
                                else:
                                    url = convert_to_raw(
                                        relatedIdentifier['relatedIdentifier'])
                                    task = asyncio.ensure_future(
                                        download_dependencies(url, session))
                                    tasks.append(task)

                        files = await asyncio.gather(*tasks)
                        return files

                async def download_dependencies(url, session):
                    if url == "on mcmaster math server (file to large for github)":
                        file_name = file_name = version_tag + dataset_name + "/" + \
                            version_tag + dataset_name + "_dependencies/" + url + ".txt"
                        return (file_name, "on mcmaster math server (file to large for github)")
                    else:
                        file_name = version_tag + dataset_name + "/" + version_tag + \
                            dataset_name + "_dependencies/" + \
                            os.path.basename(url[34:])
                    async with session.get(url) as response:
                        file_content = await response.read()
                        return (file_name, file_content)

                return asyncio.run(main())
            else:
                return "Failure in getting assets from GitHub {}\n{}".format(response.status_code, response.text)
