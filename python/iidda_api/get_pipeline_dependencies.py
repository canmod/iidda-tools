from github import Github
import requests
import os
import configparser
from iidda_api import read_config
import aiohttp
import asyncio

def convert_to_raw(url):
    return url.replace("github.com", "raw.githubusercontent.com").replace("/blob/", "/")

def get_pipeline_dependencies(dataset_name, download_path, version="latest"):
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
        print("This dataset does not exist in the releases")
        return
    
    if version == "latest":
        version = len(release_list)
    
    if version > len(release_list):
        print("The supplied version is greater than the latest version. Downloading the latest version...")
        version = len(release_list)

    release = release_list[version - 1]

    headers = {
        'Authorization': 'token ' + ACCESS_TOKEN,
        'Accept': 'application/octet-stream'
    }

    for asset in release.get_assets():
        if asset.name == dataset_name + ".json":
            response = requests.get(asset.url, stream=True, headers=headers)
            if response.ok:
                dataset_metadata = response.json()
                async def main():
                    async with aiohttp.ClientSession(headers={'Authorization': 'token ' + ACCESS_TOKEN, 'Accept': 'application/vnd.github.v3.raw'}) as session:
                        tasks = []
                        for relatedIdentifier in dataset_metadata['relatedIdentifiers']:
                            if relatedIdentifier['relatedIdentifierType'] == "URL" and relatedIdentifier['relationType'] == "IsSourceOf":
                                if isinstance(relatedIdentifier['relatedIdentifier'], list):
                                    for link in relatedIdentifier['relatedIdentifier']:
                                        url = convert_to_raw(link)
                                        task = asyncio.ensure_future(download_dependencies(url, session))
                                        tasks.append(task)
                                else:
                                    url = convert_to_raw(relatedIdentifier['relatedIdentifier'])
                                    task = asyncio.ensure_future(download_dependencies(url, session))
                                    tasks.append(task)
                                
                        
                        files = await asyncio.gather(*tasks)
                        for file in files:
                            path = download_path + "/" + release.title + "/" + file[1]
                            # make directory if it doesn't exist
                            os.makedirs(os.path.dirname(path), exist_ok=True)
                            with open(path, "wb") as downloaded_file:
                                downloaded_file.write(file[0])
                            
                async def download_dependencies(url, session):
                    file_name = os.path.basename(url[34:])
                    async with session.get(url) as response:
                        file_content = await response.read()
                        return [file_content,file_name]
                
                asyncio.run(main())
            else:
                print("Failure in getting assets from GitHub {}\n{}".format(response.status_code, response.text))
