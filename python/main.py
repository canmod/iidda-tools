from fastapi import FastAPI, Request, HTTPException
from iidda_api import *
from fastapi.responses import FileResponse
import nest_asyncio
from fastapi.openapi.utils import get_openapi
nest_asyncio.apply()

app = FastAPI()

@app.get("/datasets")
async def datasets(all_metadata=False):
    return get_dataset_list(all_metadata,clear_cache=False)

@app.get("/datasets/{dataset_name}")
async def dataset_name(dataset_name,version="latest"):
    if not get_pipeline_dependencies(dataset_name,version):
        raise HTTPException(status_code=404, detail="Item not found")
    else:
        return get_pipeline_dependencies(dataset_name,version)

@app.post('/payload', include_in_schema=False)  # ‘/hooktest’ specifies which link will it work on 
async def webhook(req: Request):
    get_dataset_list(all_metadata="False",clear_cache=True)
    return "Cache cleared."

def custom_openapi():
    if app.openapi_schema:
        return app.openapi_schema
    openapi_schema = get_openapi(
        title="IIDDA API",
        version="1.0.0",
        description="Description.",
        routes=app.routes,
    )
    openapi_schema["info"]["x-logo"] = {
        "url": "https://brand.mcmaster.ca/app/uploads/2019/04/mcm-bw-rev.png"
    }
    app.openapi_schema = openapi_schema
    return app.openapi_schema


app.openapi = custom_openapi