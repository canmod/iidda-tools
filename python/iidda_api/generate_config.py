import configparser
import os
from appdirs import *

# generate path
os.mkdir(user_config_dir("iidda-api"))
path = user_config_dir("iidda-api") + '/config.ini'

def generate_config(token):
    # defining structure of the file
    config = configparser.ConfigParser()
    config.add_section('github_info')
    config.set('github_info', 'access_token', token)

    print("generating config file at: " + path)

    # write the file
    with open(path, 'w') as configfile:
        config.write(configfile)


