import requests
import os
from iidda_api import read_config
import aiohttp
import asyncio
from iidda_api import get_release_list
from aiohttp_client_cache import FileBackend
from appdirs import *


def convert_to_raw(url):
    '''Converts github.com url to raw.githubusercontent.com url

    Args:
        url (str): link with base url "github.com" to a file stored on github

    Returns:
        str: equivalent url with "raw.githubusercontent.com" base url
    '''
    return url.replace("github.com", "raw.githubusercontent.com").replace("/blob/", "/")


async def get_pipeline_dependencies(dataset_name, version="latest", version_tag=""):
    '''Downloads all pipeline_dependencies of a dataset

    Args:
        dataset_name (str): name of the dataset
        version (str, int, optional): version of the dataset
        version_tag (str, optional): version prefix of dataset (e.g. "v9-" indicates version 9 of a particular dataset)

    Returns:
        list: list of tuples. Each tuple contains a file's name and content
    '''
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
