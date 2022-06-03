# iidda_api package

## Installation

* Navigate to the `python` directory
* Execute `pip install -r requirements.txt`
* Execute `pip install .`
* To generate a config file, import the `generate_config` function from `iidda_api` and pass a Github access token and repository (in the form `{user}/{repository}`) into the function (parameters in the form `(token, repository)`). This will generate a `config.ini` file on your computer.
* You can update the configuration simply by re-generating the configuration file.
* Execute `uvicorn main:app`. Open [http://127.0.0.1:8000/docs](http://127.0.0.1:8000/docs) or [http://127.0.0.1:8000/redoc](http://127.0.0.1:8000/redoc) in a browser.

**Note:**
If the `uvloop` package is installed on your computer, you may get the error: `ValueError: Can't patch loop of type <class 'uvloop.Loop’>`. Simply uninstall the `uvloop` package to fix this error.

## Usage

* The `/datasets/{dataset_name}` endpoint has the following options for the `response_type` parameter:
    * `dataset_download`: downloads a zip file containing only the `.csv` file if `metadata == false` and all the associated `.json` metadata files if `metadata == true`.
    * `pipeline_dependencies`: downloads all the original source files for the datasets. 
    * `github_url`: returns a JSON object containing the GitHub URL to the release.
    * `raw_csv`: returns the raw CSV file which can then be used in functions like `read.csv()` in R. Note that this will not work in the Swagger UI but only as a URL (for example, [http://127.0.0.1:8000/datasets/cdi_ca_1957_wk_prov_dbs?response_type=raw_csv](http://127.0.0.1:8000/datasets/cdi_ca_1957_wk_prov_dbs?response_type=raw_csv))
    * `metadata`: returns the content inside the `{dataset_name}.json` asset.
    * `csv_dialect`: returns the content inside the `{dataset_name}_csv_dialect.json` asset.
    * `data_dictionary`: returns the content inside the `{dataset_name}_data_dictionary.json` asset.
