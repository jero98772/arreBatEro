#!/usr/bin/env python 
# -*- coding: utf-8 -*-"
"""
MyManual - 2023 - por [jero98772,jhonmesa]
MyManual - 2023 - by [jero98772,jhonmesa]
"""
from setuptools import setup, find_packages
setup(
	name='gas',
	version='2.0.0 beta',
	license='GPLv3',
	author_email='jero98772@protonmail.com',
	author='[jero98772,jhonmesa]',
	description='',
	url='',
	packages=find_packages(),
    install_requires=['Flask','pycrypto'],
    include_package_data=True,
	)
