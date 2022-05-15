import configparser
import os
from appdirs import *

# generate path
def config_path():
    directory_path = user_config_dir("iidda-api","")
    if not os.path.isdir(directory_path):
        os.makedirs(directory_path)

    path = directory_path + '/config.ini'
    return path

def generate_config(token):
    # defining structure of the file
    config = configparser.ConfigParser()
    config.add_section('github_info')
    config.set('github_info', 'access_token', token)

    print("generating config file at: " + config_path())

    # write the file
    with open(config_path(), 'w') as configfile:
        config.write(configfile)
        
def read_config():
    config_obj = configparser.ConfigParser()
    config_obj.read(config_path())
    config_github = config_obj["github_info"]

    ACCESS_TOKEN = config_github["access_token"]
    
    return ACCESS_TOKEN
    


