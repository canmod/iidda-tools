from github import Github
import requests
import os
import configparser
from iidda_api import generate_config
import json
import aiohttp
import asyncio
from aiohttp_client_cache import CachedSession, FileBackend
import requests_cache

def get_dataset_list(download_path, file_name='Dataset List', all_metadata=False):
    # Get access token
    ACCESS_TOKEN = generate_config.read_config()

    headers = {
        'Authorization': 'token ' + ACCESS_TOKEN,
        'Accept': 'application/octet-stream'
    }
    
    # make directory if it doesn't exist
    path = "".join([download_path, "/", file_name])
    if not os.path.isdir(path):
        os.makedirs(path)
    
    # make cache directory
    cache_path = "".join([path, "/", 'cache'])
    if not os.path.isdir(cache_path):
        os.makedirs(cache_path)
    
    # Cache configuration for GitHub Repo
    session = requests_cache.CachedSession(
        cache_path + '/github-repo-cache',
        expire_after=30 #expire after x number of seconds
    )
    
    # Retrieve list of releases
    response = session.get('https://api.github.com/repos/canmod/iidda-test-assets/releases', headers={'Authorization': 'token ' + ACCESS_TOKEN, 'Accept': 'application/vnd.github.v3+json'})
    releases = list(response.json())
    
    dataset_title_list = map(lambda release: release['name'], releases)
        
    dataset_title_list = list(dict.fromkeys(dataset_title_list))

    # Cache configuration for release assets
    cache = FileBackend(
        cache_name = cache_path
    )
    
    async def main():
        async with CachedSession(cache=cache) as session:
            tasks = []
            for title in dataset_title_list:
                task = asyncio.ensure_future(get_dataset_data(session, title))
                tasks.append(task)

            dataset_metadata = await asyncio.gather(*tasks)

            result_file = dict(zip(dataset_title_list,dataset_metadata))

            # Write the dicitonary in JSON file
            file_path = "".join([path, '/' + "_".join(file_name.split()) + '.json'])
            with open(file_path, "w") as file:
                file.write(json.dumps(result_file, indent=4))

    async def get_dataset_data(session,title): 
        # Search for latest version
        title_version_list = filter(lambda release: release['name'] == title, releases)
        title_version_list = sorted(title_version_list, key=lambda release: int(release['body'][8:]))
        latest_version = title_version_list[len(title_version_list) - 1]
        
        # get metadata
        latest_version_metadata = latest_version['assets']
        latest_version_metadata = list(filter(lambda asset: asset['name'] == title + '.json', latest_version_metadata))
        if latest_version_metadata != []:
            metadata_url = latest_version_metadata[0]['url']
            async with session.get(metadata_url,headers=headers) as response:
                metadata = await response.text()
                metadata = json.loads(metadata)
                if all_metadata == False:
                    return {'identifier': metadata['identifier']}
                else:
                    return metadata
        else:
            return 'No metadata.'
            
    # If OS is windows then include the below line
    # asyncio.set_event_loop_policy(asyncio.WindowsSelectorEventLoopPolicy())
    asyncio.run(main())
