from http.client import responses
from fastapi import FastAPI, Request, HTTPException, Depends, FastAPI, Query
from iidda_api import *
from fastapi.responses import FileResponse
import nest_asyncio
from fastapi.openapi.utils import get_openapi
from jq import jq
import re
from typing import Union
import aiohttp
import asyncio
nest_asyncio.apply()

app = FastAPI(title="IIDDA API", swagger_ui_parameters={
              "defaultModelsExpandDepth": -1})


def generate_filters():
    dataset_list = get_dataset_list(all_metadata=True, clear_cache=False)
    data = jq('map_values(select(. != "No metadata.")) | [paths(scalars) as $p | [ ( [ [$p[]] | map(select(. | type != "number")) | .[] | tostring ][1:] | join(" ."))] | .[]] | unique').transform(
        dataset_list)
    for x in range(len(data)):
        data[x] = "." + data[x]
    return data


@app.get("/resource", responses={200: {"content": {"text/plain": {}}}})
async def dataset(
    dataset_ids: Union[list[str], None] = Query(default=None),
    string_matching: str = Query(
        "", description='Both options are case sensitive. Default is "Equals."', enum=["Contains", "Equals"]),
    key: str = Query(
        "", description="Descriptions of the different paths leading to strings:\n * **.contributors .contributorType:** \n * **.contributors .name:** name of the contributor(s) of the dataset\n * **.contributors .nameType:**\n * **.creators .creatorName:** name of the creator(s) of the dataset\n * **creators .nameType:** type of creator (e.g. organizational) \n * **.descriptions .description:** description of the dataset \n * **.descriptions .descriptionType:** type of description (e.g. abstract, methods, etc.) \n* **.descriptions .lang:** language of the description \n * **.formats:** file formats available for download (e.g. csv), \n * **geoLocations .geoLocationPlace:** location(s) in which data was collected \n * **.identifier .identifier:** GitHub URL of the dataset \n * **.identifier .identifierType:** \n * **.language:** language the dataset is available in \n * **.publicationYear:** year of publication of the dataset \n * **.publisher:** publisher of the dataset \n * **.relatedIdentifiers .relatedIdentifier:**  \n * **.relatedIdentifiers .relatedIdentifierType:** type of content inside .relatedIdentifiers .relatedIdentifier (e.g. URL)\n * **.relatedIdentifiers .relationType:** \n * **resourceType .resourceType:** \n * **.resourceType .resourceTypeGeneral:** \n * **.rightsList .lang:** \n * **.rightsList .rights:** \n * **.rightsList .rightsURI:** \n * **.titles .lang:** language of the title \n * **.titles .title:** title of the dataset \n * **.version:** version of the dataset",
        enum=generate_filters()
    ),
    value: str = "",
    jq_query: str = "",
    response_type: str = Query("metadata", enum=sorted(
        ["github_url", "raw_csv", "metadata", "csv_dialect", "data_dictionary"])),
    version: str = "latest"
):
    # Defining list of datasets to download
    if dataset_ids != None:
        dataset_list = dataset_ids
    else:
        data = get_dataset_list(all_metadata=True, clear_cache=False)
        if (key == "" or value == "") and jq_query == "":
            dataset_list = jq("keys").transform(data)
        elif jq_query != "":
            dataset_list = jq(f'{jq_query} | keys').transform(data)
        elif key != "" and value != "":
            if string_matching == "Contains":
                string_matching = f'contains("{value}")'
            elif string_matching == None or string_matching == "Equals":
                string_matching = f'. == "{value}"'

            keys = key.split(" ")
            if len(keys) > 1:
                dataset_list = jq(
                    f'map_values(select(. != "No metadata.") | select({keys[0]} | if type == "array" then select(.[] {keys[1]} | if type == "array" then select(.[] | {string_matching}) else select(. | {string_matching}) end) else select({keys[1]} | {string_matching}) end)) | keys').transform(data)
            else:
                dataset_list = jq(
                    f'map_values(select(. != "No metadata.") | select({keys[0]} != null) | select({keys[0]} | if type == "array" then (.[] | {string_matching}) else {string_matching} end)) | keys').transform(data)
    
    if response_type == "metadata":
        data = get_dataset_list(all_metadata=True, clear_cache=False)
        if len(dataset_list) == 0: 
            return data
        if len(dataset_list) == 1: 
            return get_dataset(dataset_name = dataset_list[0], version = version, response_type = response_type)
        elif len(dataset_list) > 1:
            conditions = " or .key == ".join(f'"{d}"' for d in dataset_list)
            return jq(f'with_entries(select(.key == {conditions}))').transform(data)


@app.get("/download", responses={200: {"content": {"application/x-zip-compressed": {}}}})
async def download(
    dataset_ids: Union[list[str], None] = Query(default=None),
    string_matching: str = Query(
        "", description='Both options are case sensitive. Default is "Equals."', enum=["Contains", "Equals"]),
    key: str = Query(
        "", description="Descriptions of the different paths leading to strings:\n * **.contributors .contributorType:** \n * **.contributors .name:** name of the contributor(s) of the dataset\n * **.contributors .nameType:**\n * **.creators .creatorName:** name of the creator(s) of the dataset\n * **creators .nameType:** type of creator (e.g. organizational) \n * **.descriptions .description:** description of the dataset \n * **.descriptions .descriptionType:** type of description (e.g. abstract, methods, etc.) \n* **.descriptions .lang:** language of the description \n * **.formats:** file formats available for download (e.g. csv), \n * **geoLocations .geoLocationPlace:** location(s) in which data was collected \n * **.identifier .identifier:** GitHub URL of the dataset \n * **.identifier .identifierType:** \n * **.language:** language the dataset is available in \n * **.publicationYear:** year of publication of the dataset \n * **.publisher:** publisher of the dataset \n * **.relatedIdentifiers .relatedIdentifier:**  \n * **.relatedIdentifiers .relatedIdentifierType:** type of content inside .relatedIdentifiers .relatedIdentifier (e.g. URL)\n * **.relatedIdentifiers .relationType:** \n * **resourceType .resourceType:** \n * **.resourceType .resourceTypeGeneral:** \n * **.rightsList .lang:** \n * **.rightsList .rights:** \n * **.rightsList .rightsURI:** \n * **.titles .lang:** language of the title \n * **.titles .title:** title of the dataset \n * **.version:** version of the dataset",
        enum=generate_filters()
    ),
    value: str = "",
    jq_query: str = "",
    resource: Union[list[str], None] = Query(
        default=None, description="Options include: CSV, pipeline_dependencies, metadata"),
    version: str = "latest"
):

    # Defining list of datasets to download
    if resource == None:
        return "Please select at least one resource to download."

    if dataset_ids != None:
        dataset_list = dataset_ids
    else:
        data = get_dataset_list(all_metadata=True, clear_cache=False)
        if (key == "" or value == "") and jq_query == "":
            dataset_list = jq("keys").transform(data)
        elif jq_query != "":
            dataset_list = jq(f'{jq_query} | keys').transform(data)
        elif key != "" and value != "":
            if string_matching == "Contains":
                string_matching = f'contains("{value}")'
            elif string_matching == None or string_matching == "Equals":
                string_matching = f'. == "{value}"'

            keys = key.split(" ")
            if len(keys) > 1:
                dataset_list = jq(
                    f'map_values(select(. != "No metadata.") | select({keys[0]} | if type == "array" then select(.[] {keys[1]} | if type == "array" then select(.[] | {string_matching}) else select(. | {string_matching}) end) else select({keys[1]} | {string_matching}) end)) | keys').transform(data)
            else:
                dataset_list = jq(
                    f'map_values(select(. != "No metadata.") | select({keys[0]} != null) | select({keys[0]} | if type == "array" then (.[] | {string_matching}) else {string_matching} end)) | keys').transform(data)

    async def main():
        tasks = []
        for dataset in dataset_list:
            if "metadata" in resource:
                task = asyncio.create_task(asyncio.coroutine(get_download)(
                    dataset_name=dataset, version=version, resource=resource))
                tasks.append(task)
            else:
                task = asyncio.create_task(asyncio.coroutine(get_download)(
                    dataset_name=dataset, version=version, resource=resource))
                tasks.append(task)

        files = await asyncio.gather(*tasks)
        mem_zip = BytesIO()
        zip_sub_dir = "-".join(dataset_list)
        zip_filename = "%s.zip" % zip_sub_dir
        with zipfile.ZipFile(mem_zip, mode="w", compression=zipfile.ZIP_DEFLATED) as zf:
            for f in files:
                zf.writestr(f[0], f[1])

        return StreamingResponse(
            iter([mem_zip.getvalue()]),
            media_type="application/x-zip-compressed",
            headers={"Content-Disposition": f"attachment;filename=%s" %
                     zip_filename}
        )

    return asyncio.run(main())


# ‘/githubwebhook’ specifies which link will it work on
@app.post('/githubwebhook', include_in_schema=False)
async def webhook(req: Request):
    get_dataset_list(all_metadata="False", clear_cache=True)
    return "Cache cleared."


def custom_openapi():
    if app.openapi_schema:
        return app.openapi_schema
    openapi_schema = get_openapi(
        title="IIDDA API",
        version="1.0.0",
        description="Open toolchain for processing infectious disease datasets available through IIDDA and other repositories",
        routes=app.routes,
    )
    openapi_schema["info"]["x-logo"] = {
        "url": "https://brand.mcmaster.ca/app/uploads/2019/04/mcm-bw-rev.png"
    }
    app.openapi_schema = openapi_schema
    return app.openapi_schema


app.openapi = custom_openapi
