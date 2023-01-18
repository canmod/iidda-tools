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

    print("generating config file at: " + config_path())

    # write the file
    with open(config_path(), 'w') as configfile:
        config.write(configfile)


def read_config(key):
    '''Read values inside the config.ini file

    Args:
        key: key found in the github_info section of the config.ini file (e.g. access_token)
    Returns:
        str: returns the corresponding value of the key
    '''
    config_obj = configparser.ConfigParser()
    config_obj.read(config_path())
    config_github = config_obj["github_info"]

    return config_github[key]
