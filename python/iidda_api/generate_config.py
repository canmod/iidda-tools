import configparser
import os
from appdirs import *

# generate path


def config_path():
    '''
    Returns:
        str: The path of the config.ini file
    '''
    directory_path = user_config_dir("iidda-api", "")
    if not os.path.isdir(directory_path):
        os.makedirs(directory_path)

    path = directory_path + '/config.ini'
    return path


def generate_config(token, repository="canmod/iidda-test-assets", webhook_secret=""):
    '''Writes values into a config.ini file

    Args:
        token: github access token
        repository: github repository name (default is "canmod/iidda-test-assets")
        webhook_secret: github webhook secret
    '''
    # defining structure of the file
    config = configparser.ConfigParser()
    config.add_section('github_info')
    config.set('github_info', 'access_token', token)
    # repository must be in the form {author}/{repository}
    config.set('github_info', 'repository', repository)
    config.set('github_info', 'webhook_secret', webhook_secret)
    config.set('local_info', 'use_local_csv_files', 'false')
    config.set('local_info', 'local_derived_data', '')

    print("generating config file at: " + config_path())

    # write the file
    with open(config_path(), 'w') as configfile:
        config.write(configfile)


def read_config(key, info_type = "github_info"):
    '''Read values inside the config.ini file

    Args:
        key: key found in the github_info section of the config.ini file (e.g. access_token)
        info_type: type of info in the config file (github_info or local_info)
    Returns:
        str: returns the corresponding value of the key
    '''
    config_obj = configparser.ConfigParser()
    config_obj.read(config_path())
    if info_type in config_obj.keys():
        config_github = config_obj[info_type]
    else:
        return None
    
    if key in config_github.keys():
        value = config_github[key]

    return value
