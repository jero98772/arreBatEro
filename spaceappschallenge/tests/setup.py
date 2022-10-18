#!/usr/bin/env python 
# -*- coding: utf-8 -*-"
"""
"""
from setuptools import setup, find_packages
setup(
	name='',
	version='',
	license='GPLv3',
	author_email='jero98772@protonmail.com',
	author='jero98772',
	description='free source minimal multilingual blog maker and manager for different blog entries and multiple blog entries , with web interface. in this blog can use images but we looking to keep it minimalism.',
	url='',
	packages=find_packages(),
    install_requires=['Flask','deep-translator'],
    include_package_data=True,
	)
genTokenFile("data/token.txt")