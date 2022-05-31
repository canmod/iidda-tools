from fastapi import FastAPI, Request, HTTPException
from iidda_api import *
from fastapi.responses import FileResponse
import nest_asyncio
nest_asyncio.apply()

app = FastAPI()

@app.get("/datasets")
async def datasets(all_metadata=False,clear_cache=False):
    return get_dataset_list(all_metadata,clear_cache)

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