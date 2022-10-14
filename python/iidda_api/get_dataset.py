import os
from iidda_api import read_config
from io import BytesIO
import asyncio
from iidda_api import get_release_list
from aiohttp_client_cache import CachedSession, FileBackend
from appdirs import *


async def get_dataset(dataset_name, version):
    # Get access token
    ACCESS_TOKEN = read_config('access_token')

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
        version = len(release_list)

    if int(version) > len(release_list):
        return f"The supplied version of {dataset_name} is greater than the latest version of {len(release_list)}"

    release = release_list[int(version) - 1]

    headers = {
        'Authorization': 'token ' + ACCESS_TOKEN,
        'Accept': 'application/octet-stream'
    }

    for asset in release['assets']:
        if asset['name'] == dataset_name + '.csv':
            async with CachedSession(cache=assets_cache) as session:
                async with session.get(asset['url'], headers=headers) as response:
                    file_content = await response.read()
                    return BytesIO(file_content)
