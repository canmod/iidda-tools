import os
from setuptools import setup

#with open('requirements.txt') as f:
#    requirements = f.read().splitlines()

setup(
    name='iidda_api',
    version='0.0.1',
    author='Steve Walker',
    author_email='swalk@mcmaster.ca',
    license='GPL-3',
    packages=['iidda_api'],
    include_package_data=True,
    description='API for the IIDDA package',
    long_description='API for the IIDDA package',
    keywords='api data epidemiology',
    #install_requires=requirements,
    zip_safe=False
)
