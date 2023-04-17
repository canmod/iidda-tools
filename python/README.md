# IIDDA Python Packages

## Development Environment Installation and Setup :computer:

**For Windows:**
As of now, the development environment cannot be setup regularly on Windows.
Instead, the Windows Subsystem for Linux (WSL) could be used. Open command
prompt as administrator, then run `wsl --install` and restart your machine.
After, download Ubuntu for WSL from https://ubuntu.com/wsl. You should now be
able to open the Ubuntu terminal and follow the commands for Mac & Linux users.

**For Mac & Linux:**
* Make sure that Python 3.8 or higher is installed and being used (see `python --version`)
* Navigate to the `python` directory in `iidda-tools` repository (i.e. navigate to `iidda-tools/python`)
* Optional: establish a Python virtual environment for this repo -- for example
  * (one-time) make a virtual environment with something like this `/usr/local/anaconda3/bin/python -m venv venv` (here I'm pointing explicitly to the python that I want)
  * If another version of python was installed and you cannot create a virtual environment with that version, you may have to install the corresponding Python venv (`sudo apt-get install python3.X-dev python3.X-venv` where X represents the Python version).
  * (every-time) then activate the virtual environment with this `source venv/bin/activate`
* (whenever package dependencies change) Execute `pip install -r requirements.txt`
* (whenever `iidda_api` package changes) Execute `pip install .` to install the `iidda_api` package (and potentially others)
* (whenever your GitHub personal access tokens expire) Start a Python process and do the following:
```
from iidda_api import generate_config
generate_config(token="TOKEN", repository="canmod/iidda-test-assets")
```
* Generate your personal access token [here](https://github.com/settings/tokens).
  * Note: You may need to enable `repo` in the Scopes when generating the token
* To find the location of your config file do the following
```
from iidda_api import config_path
config_path()
```
* You can update the configuration simply by re-generating the configuration file, or you can edit this config file directly instead of re-generating it
  * Confirm that you included your personal access token in the config file. (Replace "TOKEN" with your access token when running `generate_config` in Python, or edit the config file to have `access_token = (The personal access token you generated)`)
* Confirm that you are an admin of `iidda-test-assets` repository (if not, it may lead to the app not returning anything when ran)

**Note:**
If the `uvloop` package is installed on your computer, you may get the error:
`ValueError: Can't patch loop of type <class 'uvloop.Loop’>`. Simply uninstall
the `uvloop` package to fix this error.

## Usage

The API can be used in a few ways.

### Using the Deployed Version

We are working to get https://davidearn.mcmaster.ca/iidda/ back up as a deployed
version of the API in this repository. It is not currently up 😢.

### Using the Development Version

* Follow the instructions above to set up a development environment
* From a terminal in the `python` directory, execute `uvicorn main:app`
* If all goes well, open [http://127.0.0.1:8000/docs](http://127.0.0.1:8000/docs) or [http://127.0.0.1:8000/redoc](http://127.0.0.1:8000/redoc) in a browser, which will reveal interactive documentation

### Using the R Bindings

The [iidda.api](../R/iidda.api) package can be used to interact with the API.
Currently these bindings depend on having a running development version of the
API (see instructions above).

### Using a Shiny App

The [iidda.shiny](../R/iidda.shiny) app is another way to interact with the API.
As with the R bindings, this app depends on having a running development version
of the API.


## Troubleshooting

### Clearing the Cache

Making many requests to the GitHub API can be a performance bottleneck. This occurs for example when a filter requires data from many individual tables to be combined. In such a case, the GitHub API must be called for every table. To enhance API performance, we cache the results that come back from the GitHub API. In the deployed version of the IIDDA API we use a GitHub webhook to automatically clear the cache if there has been an update to the archive. This approach is not available for a local development deployment and so if the archive is updated the cache must be manually cleared using these Python commands.

```{python}
from iidda_api import get_dataset_list
get_dataset_list(clear_cache=True, subset=[])
```


### Pulling Dataset CSV Files Locally

Sometimes it is convenient for developers to have the API pull from a local 
repository of files as opposed a GitHub release. Working in this mode makes
it easier to make experimental changes to data files. To set up this local
mode for the API one should set the `local_derived_data` variable in the 
`config.ini` file under the `[local_info]` section. This variable should give
a path to a directory within which one can recursively find directories
named after the datasets and containing the dataset assets (e.g. the dataset
csv file itself, the json metadata). One also needs to set 
`use_local_csv_files = true` under the `[local_info]` section, and can use 
this variable to turn local pulling on and off.
