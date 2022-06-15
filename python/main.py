from http.client import responses
from fastapi import FastAPI, Request, HTTPException, Depends, FastAPI, Query
from iidda_api import *
from fastapi.responses import FileResponse
import nest_asyncio
from fastapi.openapi.utils import get_openapi
from jq import jq
import re
nest_asyncio.apply()

app = FastAPI(title="IIDDA API", swagger_ui_parameters={"defaultModelsExpandDepth": -1})

def generate_filters():
    dataset_list = get_dataset_list(all_metadata=True,clear_cache=False)
    data = jq('map_values(select(. != "No metadata.")) | [paths(scalars) as $p | [ ( [ [$p[]] | map(select(. | type != "number")) | .[] | tostring ][1:] | join(" ."))] | .[]] | unique').transform(dataset_list)
    for x in range(len(data)):
        data[x] = "." + data[x]
    return data

@app.get("/dataset_metadata")
async def dataset_metadata(
        all_metadata: bool = False, 
        key: str = Query(
            "", description = "Descriptions of the different paths leading to strings:\n * **.contributors .contributorType:** \n * **.contributors .name:** name of the contributor(s) of the dataset\n * **.contributors .nameType:**\n * **.creators .creatorName:** name of the creator(s) of the dataset\n * **creators .nameType:** type of creator (e.g. organizational) \n * **.descriptions .description:** description of the dataset \n * **.descriptions .descriptionType:** type of description (e.g. abstract, methods, etc.) \n* **.descriptions .lang:** language of the description \n * **.formats:** file formats available for download (e.g. csv), \n * **geoLocations .geoLocationPlace:** location(s) in which data was collected \n * **.identifier .identifier:** GitHub URL of the dataset \n * **.identifier .identifierType:** \n * **.language:** language the dataset is available in \n * **.publicationYear:** year of publication of the dataset \n * **.publisher:** publisher of the dataset \n * **.relatedIdentifiers .relatedIdentifier:**  \n * **.relatedIdentifiers .relatedIdentifierType:** type of content inside .relatedIdentifiers .relatedIdentifier (e.g. URL)\n * **.relatedIdentifiers .relationType:** \n * **resourceType .resourceType:** \n * **.resourceType .resourceTypeGeneral:** \n * **.rightsList .lang:** \n * **.rightsList .rights:** \n * **.rightsList .rightsURI:** \n * **.titles .lang:** language of the title \n * **.titles .title:** title of the dataset \n * **.version:** version of the dataset", 
            enum=generate_filters()
            ),
        value: str ="",
        jq_query: str = ""
    ):
    if (key == "" or value == "") and jq_query == "":
        return get_dataset_list(all_metadata,clear_cache=False)
    elif jq_query != "":
        data = get_dataset_list(all_metadata=True,clear_cache=False)
        return jq(jq_query).transform(data, multiple_output=True)
    elif key != "" and value != "":
        keys = key.split(" ")
        print(keys)
        data = get_dataset_list(all_metadata=True,clear_cache=False)
        if len(keys) > 1:
            if all_metadata == False:
                return jq(f'map_values(select(. != "No metadata.") | select({keys[0]} | if type == "array" then select(.[] {keys[1]} | if type == "array" then select(.[] | contains("{value}")) else select(. | contains("{value}")) end) else select({keys[1]} | contains("{value}")) end) .identifier)').transform(data)
            else:
                return jq(f'map_values(select(. != "No metadata.") | select({keys[0]} | if type == "array" then select(.[] {keys[1]} | if type == "array" then select(.[] | contains("{value}")) else select(. | contains("{value}")) end) else select({keys[1]} | contains("{value}")) end))').transform(data)
        else:
            if all_metadata == False:
                return jq(f'map_values(select(. != "No metadata.") | select({keys[0]} != null) | select({keys[0]} | if type == "array" then (.[] | contains("{value}")) else contains("{value}") end) .identifier)').transform(data)
            else:
                return jq(f'map_values(select(. != "No metadata.") | select({keys[0]} != null) | select({keys[0]} | if type == "array" then (.[] | contains("{value}")) else contains("{value}") end))').transform(data)

@app.get(
    "/dataset/{dataset_name}", 
    responses={
        200: {
            "content": {"text/plain": {}, "application/x-zip-compressed": {}}
        }
    }
)
async def dataset(dataset_name: str,response_type: str = Query("dataset_download", enum=sorted(["dataset_download", "pipeline_dependencies", "github_url", "raw_csv", "metadata", "csv_dialect", "data_dictionary"])), version: str = "latest", metadata: bool =False):
    if response_type == "pipeline_dependencies":
        return get_pipeline_dependencies(dataset_name,version)
    else:
        return get_dataset(dataset_name,version,metadata,response_type)


@app.post('/githubwebhook', include_in_schema=False)  # ‘/githubwebhook’ specifies which link will it work on 
async def webhook(req: Request):
    get_dataset_list(all_metadata="False",clear_cache=True)
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