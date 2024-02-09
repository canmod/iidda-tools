import re
import os
from iidda_api import read_config
from io import BytesIO
import asyncio
from iidda_api import get_release_list
from aiohttp_client_cache import CachedSession, FileBackend
from appdirs import *

def csv_exists(dataset_name, version):

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
    r = re.compile('^v([0-9]+)-(.*)')
    release_list = filter(
        lambda release: release['name'] == dataset_name, releases)
    #release_list = sorted(
    #    release_list, key=lambda release: int(release['body'][8:]))
    release_list = sorted(
        release_list, key=lambda release: int(r.search(release['tag_name']).group(1)))

    # check if dataset is contained in repo
    if not release_list:
        print(f"{dataset_name} does not exist in the releases")
        return False

    if version == "latest":
        version = len(release_list)

    if int(version) > len(release_list):
        print(f"The supplied version of {dataset_name} is greater than the latest version of {len(release_list)}")
        return False

    release = release_list[int(version) - 1]

    for asset in release['assets']:
        if asset['name'] == dataset_name + '.csv':
            return True
    

    print("---------------------------------------------")
    print("Could not find a csv file for existing dataset: " + dataset_name)
    print("---------------------------------------------")
    return False


async def get_dataset(dataset_name, version):
    '''Gets the csv file of a dataset by name

    Args:
        dataset_name (str): name of the dataset
        version (str, int): version of the dataset

    Returns:
        BytesIO Object: contains content of the csv file
    '''

    if (read_config("use_local_csv_files", "local_info") == "true"):
        dataset_file = get_dataset_local_file(dataset_name)
        return dataset_file

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
    r = re.compile('^v([0-9]+)-(.*)')
    release_list = filter(
        lambda release: release['name'] == dataset_name, releases)
    #release_list = sorted(
    #    release_list, key=lambda release: int(release['body'][8:]))
    release_list = sorted(
        release_list, key=lambda release: int(r.search(release['tag_name']).group(1)))

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
                    #print('here we go: ')
                    #print(file_content)
                    file_bytes_io = BytesIO(file_content)
                    #print(file_bytes_io)
                    return file_bytes_io
    



def get_dataset_local_file(dataset_name, suffix = ".csv"):
    '''Gets the file path of a locally stored dataset by name

    Args:
        dataset_name (str): name of the dataset
        suffix (str): type of dataset file. can be ".csv", ".json", "_columns.json", "_data_dictionary.json", "_csv_dialect.json", "_filter_group_vals.json"

    Returns:
        BytesIO Object: contains content of the csv file
    '''
    if dataset_name is None:
        raise NameError("Dataset name is missing")
    data_file = dataset_name + suffix
    data_path = read_config("local_derived_data", "local_info")
    for root, subdir, files in os.walk(data_path):
        for file in files:
            if file == data_file:
                file_to_return = os.path.join(root, file)
                print("FILE_TO_RETURN")
                print(file_to_return)
                if os.path.exists(file_to_return):
                    return file_to_return
                raise FileNotFoundError(file_to_return)
