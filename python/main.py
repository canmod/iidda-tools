import nest_asyncio
nest_asyncio.apply()
from http.client import responses
from fastapi import FastAPI, Request, HTTPException, Depends, FastAPI, Query, Header
from iidda_api import *
from fastapi.responses import FileResponse, PlainTextResponse
from fastapi.openapi.utils import get_openapi
from jq import jq
import re
import aiohttp
import asyncio
from typing import List
import pandas as pd
from io import StringIO
import time
import hashlib
import hmac
import http
# from fastapi_cprofile.profiler import CProfileMiddleware

app = FastAPI(title="IIDDA API", swagger_ui_parameters={
              "defaultModelsExpandDepth": -1, "syntaxHighlight": False})
# app.add_middleware(CProfileMiddleware, enable=True, print_each_request = True, strip_dirs = False, sort_by='tottime')


def generate_filters():
    dataset_list = get_dataset_list(clear_cache=False)
    data = jq('map_values(select(. != "No metadata.")) | [paths(scalars) as $p | [ ( [ [$p[]] | map(select(. | type != "number")) | .[] | tostring ][1:] | join(" ."))] | .[]] | unique').transform(
        dataset_list)
    for x in range(len(data)):
        data[x] = "." + data[x]
    return data


def get_resource_types():
    dataset_list = get_dataset_list(clear_cache=False)
    data = jq('[.[] | select(. != "No metadata.") | .resourceType .resourceType] | unique').transform(
        dataset_list)
    return data


def generate_hash_signature(
    secret: bytes,
    payload: bytes,
    digest_method=hashlib.sha1,
):
    return hmac.new(secret, payload, digest_method).hexdigest()


@app.middleware("http")
async def add_process_time_header(request: Request, call_next):
    start_time = time.time()
    response = await call_next(request)
    process_time = time.time() - start_time
    response.headers["X-Process-Time"] = str(process_time)
    return response

@app.get("/metadata")
async def metadata(
    dataset_ids: List[str] = Query(default=None),
    string_matching: str = Query(
        "", description='Both options are case sensitive. Default is "Equals."', enum=["Contains", "Equals"]),
    key: str = Query(
        "", description="Descriptions of the different paths leading to strings:\n * **.contributors .contributorType:** \n * **.contributors .name:** name of the contributor(s) of the dataset\n * **.contributors .nameType:**\n * **.creators .creatorName:** name of the creator(s) of the dataset\n * **creators .nameType:** type of creator (e.g. organizational) \n * **.descriptions .description:** description of the dataset \n * **.descriptions .descriptionType:** type of description (e.g. abstract, methods, etc.) \n* **.descriptions .lang:** language of the description \n * **.formats:** file formats available for download (e.g. csv), \n * **geoLocations .geoLocationPlace:** location(s) in which data was collected \n * **.identifier .identifier:** GitHub URL of the dataset \n * **.identifier .identifierType:** \n * **.language:** language the dataset is available in \n * **.publicationYear:** year of publication of the dataset \n * **.publisher:** publisher of the dataset \n * **.relatedIdentifiers .relatedIdentifier:**  \n * **.relatedIdentifiers .relatedIdentifierType:** type of content inside .relatedIdentifiers .relatedIdentifier (e.g. URL)\n * **.relatedIdentifiers .relationType:** \n * **resourceType .resourceType:** \n * **.resourceType .resourceTypeGeneral:** \n * **.rightsList .lang:** \n * **.rightsList .rights:** \n * **.rightsList .rightsURI:** \n * **.titles .lang:** language of the title \n * **.titles .title:** title of the dataset \n * **.version:** version of the dataset",
        enum=generate_filters()
    ),
    value: str = "",
    jq_query: str = "",
    response_type=Query("metadata", enum=sorted(
        ["github_url", "metadata", "csv_dialect", "data_dictionary", "columns"]))
):
    # Defining list of datasets to download
    data = get_dataset_list(clear_cache=False)
    if dataset_ids != None:
        dataset_list = dataset_ids
    else:
        if (key == "" or value == "") and jq_query == "":
            return get_dataset_list(clear_cache=False, response_type=response_type)
        elif jq_query != "" and (key == "" and value == "" and dataset_ids == None):
            return jq(f'{jq_query}').transform(get_dataset_list(clear_cache=False, response_type=response_type))
        elif key != "" and value != "":
            if string_matching == "Contains":
                string_matching = f'contains("{value}")'
            elif string_matching == None or string_matching == "Equals":
                string_matching = f'. == "{value}"'

            keys = key.split(" ")
            if len(keys) > 1:
                dataset_list = jq(
                    f'map_values(select(. != "No metadata.") | select({keys[0]} | if type == "array" then select(.[] {keys[1]} | if type == "array" then del(.. | nulls) | select(.[] | {string_matching}) else select(. != null) | select(. | {string_matching}) end) else select({keys[1]} != null) | select({keys[1]} | {string_matching}) end)) | keys').transform(data)
            else:
                dataset_list = jq(
                    f'map_values(select(. != "No metadata.") | select({keys[0]} != null) | select({keys[0]} | if type == "array" then (.[] | {string_matching}) else {string_matching} end)) | keys').transform(data)

    # Ensure list has no duplicates
    dataset_list = list(set(dataset_list))

    if jq_query != "":
        return jq(f'{jq_query}').transform(get_dataset_list(clear_cache=False, response_type=response_type, subset=dataset_list))
    else:
        return get_dataset_list(clear_cache=False, response_type=response_type, subset=dataset_list)


@app.get("/raw_csv", responses={200: {"content": {"text/plain": {}}}}, response_class=StreamingResponse)
async def raw_csv(
    dataset_ids: List[str] = Query(default=None),
    string_matching: str = Query(
        "", description='Both options are case sensitive. Default is "Equals."', enum=["Contains", "Equals"]),
    key: str = Query(
        "", description="Descriptions of the different paths leading to strings:\n * **.contributors .contributorType:** \n * **.contributors .name:** name of the contributor(s) of the dataset\n * **.contributors .nameType:**\n * **.creators .creatorName:** name of the creator(s) of the dataset\n * **creators .nameType:** type of creator (e.g. organizational) \n * **.descriptions .description:** description of the dataset \n * **.descriptions .descriptionType:** type of description (e.g. abstract, methods, etc.) \n* **.descriptions .lang:** language of the description \n * **.formats:** file formats available for download (e.g. csv), \n * **geoLocations .geoLocationPlace:** location(s) in which data was collected \n * **.identifier .identifier:** GitHub URL of the dataset \n * **.identifier .identifierType:** \n * **.language:** language the dataset is available in \n * **.publicationYear:** year of publication of the dataset \n * **.publisher:** publisher of the dataset \n * **.relatedIdentifiers .relatedIdentifier:**  \n * **.relatedIdentifiers .relatedIdentifierType:** type of content inside .relatedIdentifiers .relatedIdentifier (e.g. URL)\n * **.relatedIdentifiers .relationType:** \n * **resourceType .resourceType:** \n * **.resourceType .resourceTypeGeneral:** \n * **.rightsList .lang:** \n * **.rightsList .rights:** \n * **.rightsList .rightsURI:** \n * **.titles .lang:** language of the title \n * **.titles .title:** title of the dataset \n * **.version:** version of the dataset",
        enum=generate_filters()
    ),
    value: str = "",
    jq_query: str = "",
):
    # Defining list of datasets to download
    data = get_dataset_list(clear_cache=False)
    if dataset_ids != None:
        dataset_list = dataset_ids
    else:
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
                    f'map_values(select(. != "No metadata.") | select({keys[0]} | if type == "array" then select(.[] {keys[1]} | if type == "array" then del(.. | nulls) | select(.[] | {string_matching}) else select(. != null) | select(. | {string_matching}) end) else select({keys[1]} != null) | select({keys[1]} | {string_matching}) end)) | keys').transform(data)
            else:
                dataset_list = jq(
                    f'map_values(select(. != "No metadata.") | select({keys[0]} != null) | select({keys[0]} | if type == "array" then (.[] | {string_matching}) else {string_matching} end)) | keys').transform(data)

    # Ensure list has no duplicates
    dataset_list = list(set(dataset_list))

    # Handling responses
    conditions = " or .key == ".join(f'"{d}"' for d in dataset_list)
    data_types = jq(
        f'[with_entries(select(.key == {conditions})) | .[] .resourceType .resourceType] | unique').transform(data)
    if len(data_types) > 1:
        raise HTTPException(
            status_code=400, detail="In order to use the 'raw_csv' response type, all datasets in question must be of the same resource type.")

    async def main():
        tasks = []
        for dataset in dataset_list:
            r = re.compile('^v([0-9]+)-(.*)')
            if r.match(dataset):
                version = r.search(dataset).group(1)
                dataset = r.search(dataset).group(2)
            else:
                version = "latest"
            task = asyncio.ensure_future(get_dataset(
                dataset_name=dataset, version=version))
            tasks.append(task)

        csv_list = await asyncio.gather(*tasks)

        # Error handling
        version_regex = re.compile(
            'The supplied version of (.*) is greater than the latest version of ([0-9]+)')
        exists_regex = re.compile(f'(.*) does not exist in the releases')
        error_list = []
        for file in csv_list:
            if isinstance(file, str):
                if exists_regex.match(file):
                    error_list.append(exists_regex.search(file).group(0))
                elif version_regex.match(file):
                    error_list.append(version_regex.search(file).group(0))

        if len(error_list) != 0:
            raise HTTPException(status_code=400, detail=error_list)
        else:
            merged_csv = pd.concat(
                map(pd.read_csv, csv_list), ignore_index=True)
            write_stats(endpoint="/raw_csv", datasets=dataset_list)
            return StreamingResponse(iter([merged_csv.to_csv(index=False)]), media_type="text/plain")

    return asyncio.run(main())


@app.get("/download", responses={200: {"content": {"application/x-zip-compressed": {}}}}, response_class=StreamingResponse)
async def download(
    dataset_ids: List[str] = Query(default=None),
    string_matching: str = Query(
        "", description='Both options are case sensitive. Default is "Equals."', enum=["Contains", "Equals"]),
    key: str = Query(
        "", description="Descriptions of the different paths leading to strings:\n * **.contributors .contributorType:** \n * **.contributors .name:** name of the contributor(s) of the dataset\n * **.contributors .nameType:**\n * **.creators .creatorName:** name of the creator(s) of the dataset\n * **creators .nameType:** type of creator (e.g. organizational) \n * **.descriptions .description:** description of the dataset \n * **.descriptions .descriptionType:** type of description (e.g. abstract, methods, etc.) \n* **.descriptions .lang:** language of the description \n * **.formats:** file formats available for download (e.g. csv), \n * **geoLocations .geoLocationPlace:** location(s) in which data was collected \n * **.identifier .identifier:** GitHub URL of the dataset \n * **.identifier .identifierType:** \n * **.language:** language the dataset is available in \n * **.publicationYear:** year of publication of the dataset \n * **.publisher:** publisher of the dataset \n * **.relatedIdentifiers .relatedIdentifier:**  \n * **.relatedIdentifiers .relatedIdentifierType:** type of content inside .relatedIdentifiers .relatedIdentifier (e.g. URL)\n * **.relatedIdentifiers .relationType:** \n * **resourceType .resourceType:** \n * **.resourceType .resourceTypeGeneral:** \n * **.rightsList .lang:** \n * **.rightsList .rights:** \n * **.rightsList .rightsURI:** \n * **.titles .lang:** language of the title \n * **.titles .title:** title of the dataset \n * **.version:** version of the dataset",
        enum=generate_filters()
    ),
    value: str = "",
    jq_query: str = "",
    resource: List[str] = Query(
        default=None, description="Options include: csv, pipeline_dependencies, metadata. Due to large file sizes, including 'pipeline_dependencies' will significantly increase download time.")
):
    # making sure resource types are valid
    if resource == None:
        raise HTTPException(
            status_code=400, detail="No resource was selected for download.")

    bad_resources = []
    for i in range(len(resource)):
        resource[i] = resource[i].lower()
        if resource[i] not in ["csv", "pipeline_dependencies", "metadata"]:
            bad_resources.append(resource[i])
        else:
            continue

    if len(bad_resources) == 1:
        raise HTTPException(
            status_code=400, detail=f"{bad_resources[0]} is not a valid resource. Only 'csv', 'pipeline_dependencies', and 'metadata' are valid.")
    elif len(bad_resources) > 1:
        raise HTTPException(
            status_code=400, detail=f"{', '.join(bad_resources[:-1])} and {bad_resources[-1]} are not valid resources. Only 'csv', 'pipeline_dependencies', and 'metadata' are valid.")

    # Defining list of datasets to download
    if dataset_ids != None:
        dataset_list = dataset_ids
    else:
        data = get_dataset_list(clear_cache=False)
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
                    f'map_values(select(. != "No metadata.") | select({keys[0]} | if type == "array" then select(.[] {keys[1]} | if type == "array" then del(.. | nulls) | select(.[] | {string_matching}) else select(. != null) | select(. | {string_matching}) end) else select({keys[1]} != null) | select({keys[1]} | {string_matching}) end)) | keys').transform(data)
            else:
                dataset_list = jq(
                    f'map_values(select(. != "No metadata.") | select({keys[0]} != null) | select({keys[0]} | if type == "array" then (.[] | {string_matching}) else {string_matching} end)) | keys').transform(data)

    # Ensure list has no duplicates
    dataset_list = list(set(dataset_list))

    # Handling responses
    async def main():
        tasks = []
        for dataset in dataset_list:
            r = re.compile('^v([0-9]+)-(.*)')
            if r.match(dataset):
                version = r.search(dataset).group(1)
                dataset = r.search(dataset).group(2)
            else:
                version = "latest"
            if "metadata" in resource:
                task = asyncio.ensure_future(get_download(
                    dataset_name=dataset, version=version, resource=resource))
                tasks.append(task)
            else:
                task = asyncio.ensure_future(get_download(
                    dataset_name=dataset, version=version, resource=resource))
                tasks.append(task)

        files = await asyncio.gather(*tasks)

        # Error handling
        version_regex = re.compile(
            'The supplied version of (.*) is greater than the latest version of ([0-9]+)')
        exists_regex = re.compile(f'(.*) does not exist in the releases')
        error_list = []
        for file in files:
            if isinstance(file, str):
                if exists_regex.match(file):
                    error_list.append(exists_regex.search(file).group(0))
                elif version_regex.match(file):
                    error_list.append(version_regex.search(file).group(0))

        if len(error_list) != 0:
            raise HTTPException(status_code=400, detail=error_list)
        else:
            files = sum(files, [])
            mem_zip = BytesIO()
            zip_sub_dir = "-".join(dataset_list)
            zip_filename = "%s.zip" % zip_sub_dir
            with zipfile.ZipFile(mem_zip, mode="w", compression=zipfile.ZIP_DEFLATED) as zf:
                for f in files:
                    if isinstance(f, list):
                        for item in f:
                            zf.writestr(item[0], item[1])
                    else:
                        zf.writestr(f[0], f[1])
            write_stats(endpoint="/download", datasets=dataset_list)
            return StreamingResponse(
                iter([mem_zip.getvalue()]),
                media_type="application/x-zip-compressed",
                headers={"Content-Disposition": f"attachment;filename=%s" %
                         zip_filename}
            )

    return asyncio.run(main())


@app.get("/filter", responses={200: {"content": {"application/json": {}}}})
async def filter(
    resource_type: str = Query(
        enum=get_resource_types()),
    location: List[str] = Query(default=None),
    disease: List[str] = Query(default=None),
    disease_family: List[str] = Query(default=None),
    disease_subclass: List[str] = Query(default=None),
    icd_7: List[str] = Query(default=None),
    icd_9: List[str] = Query(default=None),
    period_start_date: List[str] = Query(default=None),
    period_end_date: List[str] = Query(default=None)
):
    if resource_type not in get_resource_types():
        raise HTTPException(
            status_code=400, detail=f"'{resource_type}' is not a valid resource_type. Available values are {get_resource_types()}")

    filter_arguments = locals()
    filter_arguments = jq(
        'del(.resource_type) | map_values(select(. != null))').transform(filter_arguments)
    if filter_arguments == {}:
        raise HTTPException(
            status_code=400, detail="Please provide at least one column filter.")
    filter_list = list()
    pandas_query = list()
    for key in filter_arguments:

        # containment_filter -- components of the jq query for identifying required datasets by looking in the column summaries
        # pandas_containment_filter -- components of the pandas query for filtering the datasets identified
        if key == "period_start_date" or key == "period_end_date":
            if len(filter_arguments[key]) != 2:
                raise HTTPException(status_code=400, detail="Date query parameters must have only 2 inputs (a minimum and maximum date.)")
            if filter_arguments[key][1] < filter_arguments[key][0]:
                raise HTTPException(status_code=400, detail="The input should be in the form ['min','max']. The first date input is larger than the second.")
            containment_filter = f'select((.[0] <= "{filter_arguments[key][1]}") and (.[1] >= "{filter_arguments[key][0]}"))'
            pandas_containment_filter = f"{key} >= '{filter_arguments[key][0]}' and {key} <= '{filter_arguments[key][1]}'"
        else:
            containment_filter = " or ".join(
                map(lambda value: f'contains(["{value}"])', filter_arguments[key]))
            pandas_containment_filter = " | ".join(
                map(lambda value: f'({key} == "{value}")', filter_arguments[key]))
        filter = f'(select(.{key} != null) | .{key} | {containment_filter})'.replace(
            "'", '"')
        query = f'({pandas_containment_filter})'
        pandas_query.append(query)
        filter_list.append(filter)
    pandas_query = ' and '.join(pandas_query)
    filter_string = ' and '.join(filter_list)

    # Initial dataset_list
    dataset_list = get_dataset_list(clear_cache=False)
    
    # Filter to include only the datasets of the correct resource type
    dataset_list = jq(
        f'map_values(select(. != "No metadata.") | select(.resourceType .resourceType == "{resource_type}")) | . |=  keys').transform(dataset_list)

    # Obtain column summaries for all datasets of the correct resource type
    dataset_list = get_dataset_list(
        clear_cache=False, response_type="columns", subset=dataset_list)

    # Apply filter to the column summaries to get the list of required datasets
    dataset_list = jq(
        f'map_values(select(. != {{}}) | select({filter_string})) | keys').transform(dataset_list)
    
    if len(dataset_list) == 0:
        return "No datasets match the provided criteria."

    async def main():
        tasks = []
        for dataset in dataset_list:
            r = re.compile('^v([0-9]+)-(.*)')
            task = asyncio.ensure_future(get_dataset(
                dataset_name=dataset, version="latest"))
            tasks.append(task)

        csv_list = await asyncio.gather(*tasks)

        merged_csv = pd.concat(
            map(pd.read_csv, csv_list), ignore_index=True)
        missing_cols = list()
        for key in filter_arguments:
            if key not in merged_csv.columns:
                missing_cols.append(key)
        if len(missing_cols) > 0:
            raise HTTPException(
                status_code=400, detail=f"These columns do not exist in these dataset(s): {missing_cols}")
            merged_csv = merged_csv.query(pandas_query)
        write_stats(endpoint="/filter", datasets=dataset_list)
        return StreamingResponse(iter([merged_csv.to_csv(index=False)]), media_type="text/plain")

    return asyncio.run(main())

# ‘/githubwebhook’ specifies which link will it work on


@app.post('/githubwebhook', status_code=http.HTTPStatus.ACCEPTED, include_in_schema=False)
async def webhook(req: Request, x_hub_signature: str = Header(None)):
    payload = await req.body()
    secret = read_config("webhook_secret").encode("utf-8")
    signature = generate_hash_signature(secret, payload)
    if x_hub_signature != f"sha1={signature}":
        raise HTTPException(status_code=401, detail="Authentication error.")
    else:
        get_dataset_list(clear_cache=True)
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
