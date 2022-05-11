import configparser
import os

# detect directory of current file
path = os.path.dirname(os.path.abspath(__file__)) + '/config.ini'

def generate_config(token):
    # defining structure of the file
    config = configparser.ConfigParser()
    config.add_section('github_info')
    config.set('github_info', 'access_token', token)

    # write the file
    with open(path, 'w') as configfile:
        config.write(configfile)

