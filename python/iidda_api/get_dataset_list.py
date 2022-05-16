from github import Github
import requests
import os
import configparser
from iidda_api import generate_config
import json
import aiohttp
import asyncio

def get_dataset_list(download_path, all_metadata=False):
    # Get access token
    ACCESS_TOKEN = generate_config.read_config()
    github = Github(ACCESS_TOKEN)
    repo = github.get_repo('canmod/iidda-test-assets')

    # Retrieve list of releases
    releases = list(repo.get_releases())

    # Create list of unique dataset titles
    dataset_title_list = map(lambda release: release.title, releases)
        
    dataset_title_list = list(dict.fromkeys(dataset_title_list))

    # Generate dataset dictionary
    
    headers = {
        'Authorization': 'token ' + ACCESS_TOKEN,
        'Accept': 'application/octet-stream'
    }

    async def main():
        async with aiohttp.ClientSession(headers=headers) as session:
            tasks = []
            for title in dataset_title_list:
                task = asyncio.ensure_future(get_dataset_data(session, title))
                tasks.append(task)

            dataset_metadata = await asyncio.gather(*tasks)

            result_file = dict(zip(dataset_title_list,dataset_metadata))

            path = "".join([download_path, "/", 'Dataset List', "/", 'dataset_list.json'])

            # Creating JSON File
    
            # make directory if it doesn't exist
            os.makedirs(os.path.dirname(path), exist_ok=True)
    
            # Write the dicitonary in JSON file
            with open(path, "w") as file:
                file.write(json.dumps(result_file, indent=4))

    async def get_dataset_data(session,title): 
        # Search for latest version
        title_version_list = filter(lambda release: release.title == title, releases)
        title_version_list = sorted(title_version_list, key=lambda release: int(release.body[8:]))
        latest_version = title_version_list[len(title_version_list) - 1]
        
        # get metadata
        latest_version_metadata = latest_version.get_assets()
        latest_version_metadata = list(filter(lambda release: release.name == title + '.json', latest_version_metadata))
    
        if latest_version_metadata != []:
            metadata_url = latest_version_metadata[0].url
            async with session.get(metadata_url) as response:
                metadata = await response.text()
                metadata = json.loads(metadata)
                if all_metadata == False:
                    return {'identifier': metadata['identifier']}
                else:
                    return metadata
        else:
            return 'No metadata.'
            
    asyncio.set_event_loop_policy(asyncio.WindowsSelectorEventLoopPolicy())
    asyncio.run(main())
