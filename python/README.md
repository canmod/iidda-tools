# iidda_api package

## Installation

* Navigate to the `python` directory
* Execute `pip install .`
* To generate a config file, import the `generate_config` module from `iidda_api` and pass a Github access token and repository (in the form `{user}/{repository}` into the `generate_config(token, repository)` function found in this module. This will generate a `config.ini` file on your computer.
* You can update the access token simply by re-generating the configuration file.
