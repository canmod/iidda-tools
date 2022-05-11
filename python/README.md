# iidda_api package

## Installation

## One-Time

* Navigate to the `python/iidda_api` directory
* Make a copy of `config.py` called `config_site.py`
* Obtain a GitHub access token and add it to `config_site.py`

`config_site.py` is on the `.gitignore` list so do not check it in.

## After a Change

* Navigate to the `python` directory
* Execute `pip install .`

## New Installation steps
* Navigate to the `python` directory
* Execute `pip install .`
* To generate a config file, import the `generate_config` module from `iidda_api` and pass a Github access token into the `generate_config(token)` function found in this module. This will generate a `config.ini` file in the `iidda_api` package folder.
* You can update the access token simply by re-generating the configuration file.