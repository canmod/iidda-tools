from github import Github
import requests
import os
from fastapi.responses import StreamingResponse
import configparser
from iidda_api import read_config, get_pipeline_dependencies
from io import BytesIO
import zipfile
import json
import aiohttp
import asyncio
from fastapi.responses import PlainTextResponse

def get_download(dataset_name, version, resource=None):
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

    async def main():
        async with aiohttp.ClientSession(headers=headers) as session:
            tasks = []
            if "pipeline_dependencies" in resource:
                task = asyncio.create_task(asyncio.coroutine(get_pipeline_dependencies)(dataset_name, version=version))
                tasks.append(task)
            for asset in release.get_assets():
                if (asset.name.endswith(".csv") and "CSV" in resource) or (asset.name.endswith(".json") and "metadata" in resource):
                    task = asyncio.ensure_future(download_asset(asset.url, asset.name, session))
                    tasks.append(task)
                    
            files = await asyncio.gather(*tasks)
            mem_zip = BytesIO()
            zip_sub_dir = dataset_name
            zip_filename = "%s.zip" % zip_sub_dir
            with zipfile.ZipFile(mem_zip, mode="w",compression=zipfile.ZIP_DEFLATED) as zf:
                for f in files:
                    zf.writestr(f[0], f[1])

            return (zip_filename, mem_zip.getvalue())
                
    async def download_asset(url, asset_name, session):
        file_name = asset_name
        async with session.get(url) as response:
            if response.ok:
                file_content = await response.read()
                return (file_name,file_content)
            else:
                return "Download failed: {}\n{}".format(response.status_code, response.text)
    
    return asyncio.run(main())