from github import Github
import requests
import os
import configparser
from iidda_api import read_config
import aiohttp
import asyncio
from io import BytesIO
from fastapi.responses import StreamingResponse
import zipfile


def convert_to_raw(url):
    return url.replace("github.com", "raw.githubusercontent.com").replace("/blob/", "/")

def get_pipeline_dependencies(dataset_name, version="latest"):
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
                            return files
                                
                    async def download_dependencies(url, session):
                        file_name = dataset_name + "/" + dataset_name + "_dependencies/" + os.path.basename(url[34:])
                        async with session.get(url) as response:
                            file_content = await response.read()
                            return (file_name,file_content)
                    
                    return asyncio.run(main())
                else:
                    return "Failure in getting assets from GitHub {}\n{}".format(response.status_code, response.text)