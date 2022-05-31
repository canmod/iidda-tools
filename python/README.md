# iidda_api package

## Installation

* Navigate to the `python` directory
* Execute `pip install -r requirements.txt`
* Execute `pip install .`
* To generate a config file, import the `generate_config` function from `iidda_api` and pass a Github access token and repository (in the form `{user}/{repository}`) into the function (parameters in the form `(token, repository)`). This will generate a `config.ini` file on your computer.
* You can update the configuration simply by re-generating the configuration file.
* Execute `uvicorn main:app`. Open `http://127.0.0.1:8000/docs` in a browser.
