import os
from iidda_api import read_config
import json
import asyncio
from aiohttp_client_cache import CachedSession, FileBackend
from appdirs import *
import re


async def get_release_list(access_token, cache_config, clear_cache):
    '''Gets list of all releases in a github respository

    Args:
        access_token (str): github access token
        cache_config (aiohttp_client_cache Backend Object): aiohttp_client_cache Backend Object (e.g. FileBackend object)
        clear_cache (bool): clears all cache stored in the cache backend

    Returns:
        list: list of all releases found in a github repository
    '''
    async with CachedSession(cache=cache_config) as session:
        if clear_cache == True or (isinstance(clear_cache, str) and clear_cache.lower() == 'true'):
            await session.cache.clear()
        page = 1
        release_list = []
        while True:
            async with session.get(f"https://api.github.com/repos/{read_config('repository')}/releases?per_page=100&page={page}", headers={"Authorization": "token " + access_token}) as response:
                releases = await response.json()
                if not releases:
                    break
                release_list.extend(list(releases))
                page += 1
        return release_list


def get_dataset_list(clear_cache, response_type="metadata", subset="all"):
    """Returns dictionary containg all (or some) datasets found in the releases and their corresponding metadata, data dictionaries, etc.

    Args:
        clear_cache (bool): clears all cache stored in the cache backend
        response_type (str, optional): Type of information stored in the dictionary. Acceptable values include "columns", "csv_dialect", "data_dictionary", "github_url", "metadata". Defaults to "metadata".
        subset (str or List(str), optional): List of dataset ID strings, or "all" to return all datasets. Defaults to "all".

    Returns:
        dict: Dictionary with keys given by dataset IDs and values containing the response of the requested type (see response_type)
    """
    # Get access token
    ACCESS_TOKEN = read_config('access_token')

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

    release_list_cache = FileBackend(
        cache_name=cache_path + "/release_list"
    )

    releases = asyncio.run(get_release_list(
        ACCESS_TOKEN, release_list_cache, clear_cache))

    if subset == "all":
        dataset_title_list = map(lambda release: release['name'], releases)

        dataset_title_list = list(dict.fromkeys(dataset_title_list))
    else:
        dataset_title_list = subset

    async def main():
        async with CachedSession(cache=assets_cache) as session:
            if clear_cache == True or (isinstance(clear_cache, str) and clear_cache.lower() == 'true'):
                await session.cache.clear()
            tasks = []
            for title in dataset_title_list:
                r = re.compile('^v([0-9]+)-(.*)')
                if r.match(title):
                    version = r.search(title).group(1)
                    title = r.search(title).group(2)
                else:
                    version = "latest"
                task = asyncio.ensure_future(
                    get_dataset_data(session, title, version))
                tasks.append(task)

            dataset_metadata = await asyncio.gather(*tasks)

            result_file = dict(zip(dataset_title_list, dataset_metadata))

            return result_file

    async def get_dataset_data(session, title, version):
        # Search for latest version
        title_version_list = filter(
            lambda release: release['name'] == title, releases)
        title_version_list = sorted(
            title_version_list, key=lambda release: int(release['body'][8:]))
        if len(title_version_list) == 0:
            return "This dataset does not exist."
        if version == "latest":
            version = len(title_version_list)
        if int(version) > len(title_version_list):
            return f"The supplied version of this dataset is greater than the latest version of {len(title_version_list)}"
        version_in_question = title_version_list[int(version) - 1]

        # get metadata
        latest_version_metadata = version_in_question['assets']
        if response_type == "metadata":
            latest_version_metadata = list(
                filter(lambda asset: asset['name'] == title + '.json', latest_version_metadata))
        elif response_type == "github_url":
            return {"github_url": version_in_question["html_url"]}
        else:
            latest_version_metadata = list(filter(
                lambda asset: asset['name'] == title + "_" + response_type + '.json', latest_version_metadata))

        if latest_version_metadata != []:
            metadata_url = latest_version_metadata[0]['url']
            async with session.get(metadata_url, headers=headers) as response:
                metadata = await response.text()
                metadata = json.loads(metadata)
                return metadata
        else:
            return 'No metadata.'

    # If OS is windows then include the below line
    # asyncio.set_event_loop_policy(asyncio.WindowsSelectorEventLoopPolicy())
    return asyncio.run(main())
