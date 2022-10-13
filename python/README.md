# IIDDA Python Packages

## Development Environment Installation and Setup :computer:

* Make sure that Python 3.8 or higher is installed and being used (see `python --version`)
* Navigate to the `python` directory
* Optional: establish a Python virtual environment for this repo -- for example
  * (one-time) make a virtual environment with something like this `/usr/local/anaconda3/bin/python -m venv venv` (here I'm pointing explicitly to the python that I want)
  * (every-time) then activate the virtual environment with this `source venv/bin/activate`
* (whenever package dependencies change) Execute `pip install -r requirements.txt`
* (whenever `iidda_api` package changes) Execute `pip install .` to install the `iidda_api` package (and potentially others)
* (whenever your GitHub personal access tokens expire) Start a Python process and do the following:
```
from iidda_api import generate_config
generate_config(token="TOKEN", repository="canmod/iidda-test-assets")
```
  * You can get your personal access token [here](https://github.com/settings/tokens)
  * In the future we likely will modify the repository containing the data and so the second argument may also need changing (in the form `{user}/{repository}`)
  * To find the location of your config file do the following
```
from iidda_api import config_path
config_path()
```
  * You can update the configuration simply by re-generating the configuration file, or you can edit this config file directly instead of re-generating it

**Note:**
If the `uvloop` package is installed on your computer, you may get the error: `ValueError: Can't patch loop of type <class 'uvloop.Loop’>`. Simply uninstall the `uvloop` package to fix this error.

## Usage

* From a terminal in the `python` directory, execute `uvicorn main:app`
* If all goes well, open [http://127.0.0.1:8000/docs](http://127.0.0.1:8000/docs) or [http://127.0.0.1:8000/redoc](http://127.0.0.1:8000/redoc) in a browser, which will reveal interactive documentation
