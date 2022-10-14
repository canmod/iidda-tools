from heapq import merge
import http
import hmac
import hashlib
import time
from io import StringIO
import pandas as pd
from typing import List
import asyncio
import aiohttp
import re
from jq import jq
from fastapi.openapi.utils import get_openapi
from fastapi.responses import FileResponse, PlainTextResponse
from iidda_api import *
from fastapi import FastAPI, Request, HTTPException, Depends, FastAPI, Query, Header
from http.client import responses
import nest_asyncio
nest_asyncio.apply()
# from fastapi_cprofile.profiler import CProfileMiddleware

app = FastAPI(title="IIDDA API", swagger_ui_parameters={
              "defaultModelsExpandDepth": -1, "syntaxHighlight": False})
# app.add_middleware(CProfileMiddleware, enable=True, print_each_request = True, strip_dirs = False, sort_by='tottime')

global_data_dictionary = requests.get(
    'https://raw.githubusercontent.com/canmod/iidda/main/global-metadata/data-dictionary.json').json()
global_data_dictionary = dict((item['name'], item)
                              for item in global_data_dictionary)


def generate_filters():
    dataset_list = get_dataset_list(clear_cache=False)
    data = jq('map_values(select(. != "No metadata.")) | [paths(scalars) as $p | [ ( [ [$p[]] | map(select(. | type != "number")) | .[] | tostring ][1:] | join(" ."))] | .[]] | unique').transform(
        dataset_list)
    for x in range(len(data)):
        data[x] = "." + data[x]
    return data


def get_resource_types():
    dataset_list = get_dataset_list(clear_cache=False)
    data = jq('[.[] | select(. != "No metadata.") | .types .resourceType] | unique').transform(
        dataset_list)
    return data


def generate_hash_signature(
    secret: bytes,
    payload: bytes,
    digest_method=hashlib.sha1,
):
    return hmac.new(secret, payload, digest_method).hexdigest()


def dataset_list_search(
    dataset_ids, key, metadata_search, jq_query, string_comparison, response_type="metadata"
): 
    if dataset_ids is not None:
        return dataset_ids
    else:
        data = get_dataset_list(clear_cache=False, response_type=response_type)
        if (key is None or metadata_search is None) and jq_query is None:
            raise HTTPException(
                status_code=400, detail="There are three ways to filter datasets; they cannot be used in conjunction. 1. Provide values for metadata_search, key, and string_comparison, 2. Explicitly provide dataset_ids, 3. Provide a value for jq_query.")
        elif jq_query is not None:
            return jq(f'{jq_query} | keys').transform(data)
        elif key is not None and metadata_search is not None:
            if string_comparison == "Contains":
                string_comparison = f'contains("{metadata_search}")'
            elif string_comparison == None or string_comparison == "Equals":
                string_comparison = f'. == "{metadata_search}"'

            keys = key.split(" ")
            if len(keys) > 1:
                return jq(
                    f'map_values(select(. != "No metadata.") | select({keys[0]} | if type == "array" then select(.[] {keys[1]} | if type == "array" then del(.. | nulls) | select(.[] | {string_comparison}) else select(. != null) | select(. | {string_comparison}) end) else select({keys[1]} != null) | select({keys[1]} | {string_comparison}) end)) | keys').transform(data)
            else:
                return jq(
                    f'map_values(select(. != "No metadata.") | select({keys[0]} != null) | select({keys[0]} | if type == "array" then (.[] | {string_comparison}) else {string_comparison} end)) | keys').transform(data)


@app.middleware("http")
async def add_process_time_header(request: Request, call_next):
    start_time = time.time()
    response = await call_next(request)
    ACCESS_TOKEN = read_config('access_token')
    rate_limit = requests.get("https://api.github.com/rate_limit", headers={
                              "Authorization": "token " + ACCESS_TOKEN}).headers['x-ratelimit-remaining']
    response.headers["X-github-rate-limit-remaining"] = rate_limit
    process_time = time.time() - start_time
    response.headers["X-Process-Time"] = str(process_time)
    return response


@app.get("/metadata")
async def metadata(
    metadata_search: str = Query(None),
    key: str = Query(
        None, description="Key in metadata to search for metadata_search value. Descriptions of the different paths leading to strings:\n * **.contributors .contributorType:** \n * **.contributors .name:** name of the contributor(s) of the dataset\n * **.contributors .nameType:**\n * **.creators .creatorName:** name of the creator(s) of the dataset\n * **creators .nameType:** type of creator (e.g. organizational) \n * **.descriptions .description:** description of the dataset \n * **.descriptions .descriptionType:** type of description (e.g. abstract, methods, etc.) \n* **.descriptions .lang:** language of the description \n * **.formats:** file formats available for download (e.g. csv), \n * **geoLocations .geoLocationPlace:** location(s) in which data was collected \n * **.identifier .identifier:** GitHub URL of the dataset \n * **.identifier .identifierType:** \n * **.language:** language the dataset is available in \n * **.publicationYear:** year of publication of the dataset \n * **.publisher:** publisher of the dataset \n * **.relatedIdentifiers .relatedIdentifier:**  \n * **.relatedIdentifiers .relatedIdentifierType:** type of content inside .relatedIdentifiers .relatedIdentifier (e.g. URL)\n * **.relatedIdentifiers .relationType:** \n * **resourceType .resourceType:** \n * **.resourceType .resourceTypeGeneral:** \n * **.rightsList .lang:** \n * **.rightsList .rights:** \n * **.rightsList .rightsURI:** \n * **.titles .lang:** language of the title \n * **.titles .title:** title of the dataset \n * **.version:** version of the dataset",
        enum=generate_filters()
    ),
    string_comparison: str = Query(
        "Equals", description='Method to compare string in metadata_search and key. Both options are case sensitive."', enum=["Contains", "Equals"]),
    dataset_ids: List[str] = Query(
        default=None, description="Return metadata for the datasets specified."),
    jq_query: str = Query(None, description="JSON filter written in jq notation. Filter is applied to the metadata of the selected response_type. Cannot be used in conjuction with dataset_ids, key, and metadata_search parameters. Docs: https://stedolan.github.io/jq/"),
    response_type=Query("metadata", enum=sorted(
        ["github_url", "metadata", "csv_dialect", "data_dictionary", "columns"]))
):
    """
    Get metadata for datasets. There are 3 ways to filter datasets; they cannot be used in conjuction:
    - ***metadata_search, key, and string_comparison***
    - ***dataset_ids*** 
    - ***jq_query***
    """
    if jq_query is not None:
        try:
            return jq(f'{jq_query}').transform(get_dataset_list(clear_cache=False, response_type=response_type))
        except:
            raise HTTPException(
                status_code=400, detail="Something went wrong. Make sure the jq_query value returns data in the correct format.")
    elif (key is None or metadata_search is None) and jq_query is None and dataset_ids is None:
        return get_dataset_list(clear_cache=False, response_type=response_type)

    dataset_list = dataset_list_search(
        dataset_ids, key, metadata_search, None, string_comparison)

    return get_dataset_list(clear_cache=False, response_type=response_type, subset=dataset_list)


@app.get("/data_dictionary")
async def data_dictionary():
    ACCESS_TOKEN = read_config('access_token')
    dictionary = requests.get(
        'https://raw.githubusercontent.com/canmod/iidda/main/global-metadata/data-dictionary.json', headers={"Authorization": "token " + ACCESS_TOKEN}).json()
    return dictionary


@app.get("/raw_csv", responses={200: {"content": {"text/plain": {}}}}, response_class=StreamingResponse)
async def raw_csv(
    metadata_search: str = Query(None),
    key: str = Query(
        None, description="Key in metadata to search for metadata_search value. Descriptions of the different paths leading to strings:\n * **.contributors .contributorType:** \n * **.contributors .name:** name of the contributor(s) of the dataset\n * **.contributors .nameType:**\n * **.creators .creatorName:** name of the creator(s) of the dataset\n * **creators .nameType:** type of creator (e.g. organizational) \n * **.descriptions .description:** description of the dataset \n * **.descriptions .descriptionType:** type of description (e.g. abstract, methods, etc.) \n* **.descriptions .lang:** language of the description \n * **.formats:** file formats available for download (e.g. csv), \n * **geoLocations .geoLocationPlace:** location(s) in which data was collected \n * **.identifier .identifier:** GitHub URL of the dataset \n * **.identifier .identifierType:** \n * **.language:** language the dataset is available in \n * **.publicationYear:** year of publication of the dataset \n * **.publisher:** publisher of the dataset \n * **.relatedIdentifiers .relatedIdentifier:**  \n * **.relatedIdentifiers .relatedIdentifierType:** type of content inside .relatedIdentifiers .relatedIdentifier (e.g. URL)\n * **.relatedIdentifiers .relationType:** \n * **resourceType .resourceType:** \n * **.resourceType .resourceTypeGeneral:** \n * **.rightsList .lang:** \n * **.rightsList .rights:** \n * **.rightsList .rightsURI:** \n * **.titles .lang:** language of the title \n * **.titles .title:** title of the dataset \n * **.version:** version of the dataset",
        enum=generate_filters()
    ),
    string_comparison: str = Query(
        None, description='Both options are case sensitive. Default is "Equals."', enum=["Contains", "Equals"]),
    dataset_ids: List[str] = Query(default=None),
    jq_query: str = Query(None, description="JSON filter written in jq notation. Filter is applied to the data found at '/metadata?response_type=metadata'; your filter must return a JSON object of the same format. Cannot be used in conjuction with dataset_ids, key, and metadata_search parameters. Docs: https://stedolan.github.io/jq/"),
):
    """
    Get raw csvs for datasets. There are 3 ways to filter datasets; they cannot be used in conjuction:
    - ***metadata_search, key, and string_comparison***
    - ***dataset_ids*** 
    - ***jq_query***
    """
    # Defining list of datasets to download
    data = get_dataset_list(clear_cache=False)
    dataset_list = dataset_list_search(
        dataset_ids, key, metadata_search, jq_query, string_comparison)

    # Ensure list has no duplicates
    dataset_list = list(set(dataset_list))

    if len(dataset_list) == 0:
        raise HTTPException(
            status_code=400, detail="No datasets found.")

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
                map(lambda x: pd.read_csv(x, dtype=str), csv_list), ignore_index=True)
            write_stats(endpoint="/raw_csv", datasets=dataset_list)
            return StreamingResponse(iter([merged_csv.to_csv(index=False)]), media_type="text/plain")

    return asyncio.run(main())


@app.get("/download", responses={200: {"content": {"application/x-zip-compressed": {}}}}, response_class=StreamingResponse)
async def download(
    resource: List[str] = Query(
        description="Options include: csv, pipeline_dependencies, metadata. Due to large file sizes, including 'pipeline_dependencies' will significantly increase download time."),
    metadata_search: str = Query(None),
    string_comparison: str = Query(
        None, description='Both options are case sensitive. Default is "Equals."', enum=["Contains", "Equals"]),
    key: str = Query(
        None, description="Key in metadata to search for metadata_search value. Descriptions of the different paths leading to strings:\n * **.contributors .contributorType:** \n * **.contributors .name:** name of the contributor(s) of the dataset\n * **.contributors .nameType:**\n * **.creators .creatorName:** name of the creator(s) of the dataset\n * **creators .nameType:** type of creator (e.g. organizational) \n * **.descriptions .description:** description of the dataset \n * **.descriptions .descriptionType:** type of description (e.g. abstract, methods, etc.) \n* **.descriptions .lang:** language of the description \n * **.formats:** file formats available for download (e.g. csv), \n * **geoLocations .geoLocationPlace:** location(s) in which data was collected \n * **.identifier .identifier:** GitHub URL of the dataset \n * **.identifier .identifierType:** \n * **.language:** language the dataset is available in \n * **.publicationYear:** year of publication of the dataset \n * **.publisher:** publisher of the dataset \n * **.relatedIdentifiers .relatedIdentifier:**  \n * **.relatedIdentifiers .relatedIdentifierType:** type of content inside .relatedIdentifiers .relatedIdentifier (e.g. URL)\n * **.relatedIdentifiers .relationType:** \n * **resourceType .resourceType:** \n * **.resourceType .resourceTypeGeneral:** \n * **.rightsList .lang:** \n * **.rightsList .rights:** \n * **.rightsList .rightsURI:** \n * **.titles .lang:** language of the title \n * **.titles .title:** title of the dataset \n * **.version:** version of the dataset",
        enum=generate_filters()
    ),
    dataset_ids: List[str] = Query(default=None),
    jq_query: str = Query(None, description="JSON filter written in jq notation. Filter is applied to the data found at '/metadata?response_type=metadata'; your filter must return a JSON object of the same format. Cannot be used in conjuction with dataset_ids, key, and metadata_search parameters. Docs: https://stedolan.github.io/jq/"),
):
    """
    Download specific dataset files (pipeline dependencies, csv, and/or metadata). There are 3 ways to filter datasets; they cannot be used in conjuction:
    - ***metadata_search, key, and string_comparison***
    - ***dataset_ids*** 
    - ***jq_query***
    """
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
    data = get_dataset_list(clear_cache=False)
    dataset_list = dataset_list_search(
        dataset_ids, key, metadata_search, jq_query, string_comparison)

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


@app.get("/filter", responses={200: {"content": {"application/json": {}, "text/plain": {}}}})
async def filter(
    resource_type: str = Query(
        enum=get_resource_types()),
    response_type: str = Query("csv", enum=["csv", "dataset list"]),
    location: List[str] = Query(
        default=None, description=global_data_dictionary['location']['description']),
    iso_3166: List[str] = Query(
        default=None, description=global_data_dictionary['iso_3166']['description']),
    iso_3166_2: List[str] = Query(
        default=None, description=global_data_dictionary['iso_3166_2']['description']),
    period_start_date: str = Query(
        default=None, description=f"{global_data_dictionary['period_start_date']['description']} Must be in the form \<start date\>/\<end date\>."),
    period_end_date: str = Query(
        default=None, description=f"{global_data_dictionary['period_end_date']['description']} Must be in the form \<start date\>/\<end date\>."),
    disease_family: List[str] = Query(
        default=None, description=global_data_dictionary['disease_family']['description']),
    disease: List[str] = Query(
        default=None, description=global_data_dictionary['disease']['description']),
    icd_9: List[str] = Query(
        default=None, description=global_data_dictionary['icd_9']['description']),
    icd_7: List[str] = Query(
        default=None, description=global_data_dictionary['icd_7']['description']),
    disease_subclass: List[str] = Query(
        default=None, description=global_data_dictionary['disease_subclass']['description']),
    icd_9_subclass: List[str] = Query(
        default=None, description=global_data_dictionary['icd_9_subclass']['description']),
    icd_7_subclass: List[str] = Query(
        default=None, description=global_data_dictionary['icd_7_subclass']['description']),
    lower_age: List[str] = Query(
        default=None, description=f"{global_data_dictionary['lower_age']['description']} The first item must either be a number interval of the form \<min\>-\<max\> or 'none' (meaning no filter is applied to the case numbers). Additional items are meant to be any 'unavailable values' like 'Not available', 'Not reportable', or 'null'."),
    upper_age: List[str] = Query(
        default=None, description=f"{global_data_dictionary['upper_age']['description']} The first item must either be a number interval of the form \<min\>-\<max\> or 'none' (meaning no filter is applied to the case numbers). Additional items are meant to be any 'unavailable values' like 'Not available', 'Not reportable', or 'null'."),
    sex: List[str] = Query(default=None),
    cases_this_period: List[str] = Query(
        default=None, description=f"{global_data_dictionary['cases_this_period']['description']} The first item must either be a number interval of the form \<min\>-\<max\> or 'none' (meaning no filter is applied to the case numbers). Additional items are meant to be any 'unavailable values' like 'Not available', 'Not reportable', or 'null'."),
    cases_prev_period: List[str] = Query(
        default=None, description=f"{global_data_dictionary['cases_prev_period']['description']} The first item must either be a number interval of the form \<min\>-\<max\> or 'none' (meaning no filter is applied to the case numbers). Additional items are meant to be any 'unavailable values' like 'Not available', 'Not reportable', or 'null'."),
    cases_cum_report_year: List[str] = Query(
        default=None, description=f"{global_data_dictionary['cases_cum_report_year']['description']} The first item must either be a number interval of the form \<min\>-\<max\> or 'none' (meaning no filter is applied to the case numbers). Additional items are meant to be any 'unavailable values' like 'Not available', 'Not reportable', or 'null'."),
    cases_cum_prev_year: List[str] = Query(
        default=None, description=global_data_dictionary['cases_cum_prev_year']['description']),
    cases_median_prev_5_years: List[str] = Query(
        default=None, description=f"{global_data_dictionary['cases_median_prev_5_years']['description']} The first item must either be a number interval of the form \<min\>-\<max\> or 'none' (meaning no filter is applied to the case numbers). Additional items are meant to be any 'unavailable values' like 'Not available', 'Not reportable', or 'null'."),
    cases_cum_median_prev_5_years: List[str] = Query(
        default=None, description=f"{global_data_dictionary['cases_cum_median_prev_5_years']['description']} The first item must either be a number interval of the form \<min\>-\<max\> or 'none' (meaning no filter is applied to the case numbers). Additional items are meant to be any 'unavailable values' like 'Not available', 'Not reportable', or 'null'."),
    population: List[str] = Query(
        default=None, description=f"{global_data_dictionary['population']['description']} The first item must either be a number interval of the form \<min\>-\<max\> or 'none' (meaning no filter is applied to the case numbers). Additional items are meant to be any 'unavailable values' like 'Not available', 'Not reportable', or 'null'."),
    cause: List[str] = Query(
        default=None, description=global_data_dictionary['cause']['description']),
    location_scale: List[str] = Query(
    default=None, description=global_data_dictionary['location_scale']['description']),
    time_scale: List[str] = Query(
    default=None, description=global_data_dictionary['time_scale']['description']),
):
    """
    Get a csv containing data satisfying filters provided by the user.
    """
    if resource_type not in get_resource_types():
        raise HTTPException(
            status_code=400, detail=f"'{resource_type}' is not a valid resource_type. Available values are {get_resource_types()}")

    # filter_arguments is a dictionary containing the arguments input into the function
    filter_arguments = locals()

    # Delete the resource_type argument from filter_arguments
    filter_arguments = jq(
        'del(.resource_type, .response_type) | map_values(select(. != null))').transform(filter_arguments)

    # Check if no column filters were applied
    if filter_arguments == {}:
        raise HTTPException(
            status_code=400, detail="Please provide at least one column filter.")

    # Define list of column filters in both JQ and panda query syntax
    filter_list = list()
    pandas_query = list()

    # Create a list of any column filters that apply to a 'num_missing' column as they must be specially treated
    num_missing_columns = list()

    # Fetch data_dictionary from github
    data_dictionary = requests.get(
        'https://raw.githubusercontent.com/canmod/iidda/main/global-metadata/data-dictionary.json').json()

    # loop over the filter_arguments to generate a filter
    for key in filter_arguments:
        if jq(f'.[] | select(.name == "{key}")').transform(data_dictionary)["type"] == "date":
            date_range = filter_arguments[key].split("/")
            if len(date_range) != 2:
                raise HTTPException(
                    status_code=400, detail="This query parameter should have an argument of the form <start date>/<end date> with dates in ISO 8601 format")
            if date_range[1] < date_range[0]:
                raise HTTPException(
                    status_code=400, detail="The input should be in the form <start date>/<end date> with dates in ISO 8601 format. The first date is larger than the second.")
            # containment_filter is a jq filter that will be used to filter the columns metadata
            containment_filter = f'select((.[0] <= "{date_range[1]}") and (.[1] >= "{date_range[0]}"))'

            # pandas_containment_filter is a pandas filter that will be used to filter the dataframe
            pandas_containment_filter = f"{key} >= '{date_range[0]}' and {key} <= '{date_range[1]}'"
        elif jq(f'.[] | select(.name == "{key}")').transform(data_dictionary)["format"] == "num_missing":
            # Create name of a new temporary column (this column will contain the contents of the original column but converted to numbers or NaN allowing for proper filtering)
            temporary_column_name = key + "_num_missing"
            num_missing_columns.append((key, temporary_column_name))

            if filter_arguments[key][0].lower() == "none":
                number_range = ["-infinite", "infinite"]
                pandas_containment_filter = None
            else:
                number_range = filter_arguments[key][0].split("-")
                if len(number_range) != 2:
                    raise HTTPException(
                        status_code=400, detail="The input should be in the form {min}-{max}.")
                if int(number_range[1]) < int(number_range[0]):
                    raise HTTPException(
                        status_code=400, detail="The input should be in the form {min}-{max}. The first number is larger than the second.")

                # All filters on numbers will be applied to the temporary column while all filters on strings (i.e. the "unavailable values") will be applied to the original column
                pandas_containment_filter = f"({temporary_column_name} >= {number_range[0]} and {temporary_column_name} <= {number_range[1]})"

            containment_filter = f'select((.range[0] <= {number_range[1]}) and (.range[1] >= {number_range[0]}))'

            if len(filter_arguments[key]) > 1:
                # list of all filters applied to num_missing column
                containment_filter_list = [containment_filter]
                if pandas_containment_filter is None:
                    pandas_containment_filter_list = []
                else:
                    pandas_containment_filter_list = [
                        pandas_containment_filter]
                # Iterate over all unavailable values input by user
                for i in range(1, len(filter_arguments[key])):
                    if filter_arguments[key][i].lower() == 'null' or filter_arguments[key][i] == None:
                        unavailable_value_filter = f'(.unavailable_values | if type=="array" then any(.[]; . == null) else (. == null) end)'
                        pandas_unavailable_value_filter = f'({key}.isnull())'
                    else:
                        unavailable_value_filter = f'(select(.unavailable_values != null) | .unavailable_values | if type=="array" then any(.[]; . == "{filter_arguments[key][i]}") else (. == "{filter_arguments[key][i]}") end)'
                        pandas_unavailable_value_filter = f'({key} == "{filter_arguments[key][i]}")'
                    containment_filter_list.append(unavailable_value_filter)
                    pandas_containment_filter_list.append(
                        pandas_unavailable_value_filter)

                containment_filter = " or ".join(containment_filter_list)
                pandas_containment_filter = " or ".join(
                    pandas_containment_filter_list)
        else:
            containment_filter = " or ".join(
                map(lambda value: f'contains(["{value}"])', filter_arguments[key]))
            pandas_containment_filter = " | ".join(
                map(lambda value: f'({key} == "{value}")', filter_arguments[key]))

        filter = f'(select(.{key} != null) | .{key} | {containment_filter})'.replace(
            "'", '"')

        if pandas_containment_filter is not None:
            pandas_containment_filter = f'({pandas_containment_filter})'
            pandas_query.append(pandas_containment_filter)
        filter_list.append(filter)
    # combine all the individual pandas queries into a single string
    pandas_query = ' and '.join(pandas_query)
    # combine all the individual jq filters into a single string
    filter_string = ' and '.join(filter_list)

    # Get list of datasets of the specific resource type
    dataset_list = get_dataset_list(clear_cache=False)

    # Filter to include only the datasets of the correct resource type
    dataset_list = jq(
        f'map_values(select(. != "No metadata.") | select(.types .resourceType == "{resource_type}")) | . |=  keys').transform(dataset_list)

    # Get columns metadata for all the datasets of the specific resource type
    dataset_list = get_dataset_list(
        clear_cache=False, response_type="columns", subset=dataset_list)

    # Apply filter_string to dataset_list
    dataset_list = jq(
        f'map_values(select(. != {{}}) | select({filter_string})) | keys').transform(dataset_list)
    # Check if no datasets satisfy the filter
    if len(dataset_list) == 0:
        return "No datasets match the provided criteria."

    if response_type == "dataset list":
        return dataset_list

    async def main():
        tasks = []
        for dataset in dataset_list:
            r = re.compile('^v([0-9]+)-(.*)')
            task = asyncio.ensure_future(get_dataset(
                dataset_name=dataset, version="latest"))
            tasks.append(task)

        csv_list = await asyncio.gather(*tasks)

        merged_csv = pd.concat(
            map(lambda x: pd.read_csv(x, dtype=str), csv_list), ignore_index=True)

        # Create temporary columns for any num_missing columns that is being filtered
        if len(num_missing_columns) != 0:
            for column in num_missing_columns:
                merged_csv[column[1]] = pd.to_numeric(
                    merged_csv[column[0]], errors='coerce')

        missing_cols = list()
        for key in filter_arguments:
            if key not in merged_csv.columns:
                missing_cols.append(key)
        if len(missing_cols) > 0:
            raise HTTPException(
                status_code=400, detail=f"These columns do not exist in these dataset(s): {missing_cols}")
        if pandas_query != "":
            merged_csv = merged_csv.query(pandas_query)
        if len(num_missing_columns) != 0:
            merged_csv = merged_csv.drop(
                list(map(lambda x: x[1], num_missing_columns)), axis=1)

        all_columns_list = list(map(lambda x: x['name'], data_dictionary))

        cols = merged_csv.columns.tolist()
        cols = sorted(cols, key=all_columns_list.index)
        merged_csv = merged_csv[cols]

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
        title="API for the International Infectious Disease Data Archive (IIDDA)",
        version="1.0.0",
        description="API for searching, combining, filtering, and downloading infectious disease datasets available through IIDDA",
        routes=app.routes,
    )
    openapi_schema["info"]["x-logo"] = {
        "url": "https://brand.mcmaster.ca/app/uploads/2019/04/mcm-bw-rev.png"
    }
    app.openapi_schema = openapi_schema
    return app.openapi_schema


app.openapi = custom_openapi
