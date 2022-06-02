from github import Github
import requests
import os
from fastapi.responses import StreamingResponse
import configparser
from iidda_api import read_config
from io import BytesIO
import zipfile
from fastapi.responses import PlainTextResponse

def get_dataset(dataset_name, version, metadata, response_type):
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

    if response_type == "github_url":
        return {"github_url": release.html_url}
    elif response_type == "dataset_download":
        headers = {
            'Authorization': 'token ' + ACCESS_TOKEN,
            'Accept': 'application/octet-stream'
        }
        files = []
        for asset in release.get_assets():
            if asset.name.endswith(".csv") or (asset.name.endswith(".json") and metadata.lower() == "true"):
                response = requests.get(asset.url, stream=True, headers=headers)
                if response.ok:
                    files.append((asset.name,response.content))
                else:
                    return "Download failed: {}\n{}".format(response.status_code, response.text)
        
        mem_zip = BytesIO()
        zip_sub_dir = dataset_name
        zip_filename = "%s.zip" % zip_sub_dir
        with zipfile.ZipFile(mem_zip, mode="w",compression=zipfile.ZIP_DEFLATED) as zf:
            for f in files:
                zf.writestr(f[0], f[1])

        return StreamingResponse(
            iter([mem_zip.getvalue()]),
            media_type="application/x-zip-compressed",
            headers = { "Content-Disposition":f"attachment;filename=%s" % zip_filename}
        )
    elif response_type == "raw_csv":
        headers = {
            'Authorization': 'token ' + ACCESS_TOKEN,
            'Accept': 'application/octet-stream'
        }
        for asset in release.get_assets():
            if asset.name == dataset_name + '.csv':
                response = requests.get(asset.url, stream=True, headers=headers)
                if response.ok:
                    return PlainTextResponse(response.content)
                else:
                    return "Download failed: {}\n{}".format(response.status_code, response.text)
