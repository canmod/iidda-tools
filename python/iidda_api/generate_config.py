import configparser
import os
from appdirs import *

# generate path
directory_path = user_config_dir("iidda-api","")
if not os.path.isdir(directory_path):
    os.makedirs(directory_path)

path = directory_path + '/config.ini'

def generate_config(token):
    # defining structure of the file
    config = configparser.ConfigParser()
    config.add_section('github_info')
    config.set('github_info', 'access_token', token)

    print("generating config file at: " + path)

    # write the file
    with open(path, 'w') as configfile:
        config.write(configfile)


