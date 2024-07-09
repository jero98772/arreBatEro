import os
import sys
from setuptools import setup, Extension
import pybind11

# Define the name of the module and sources
module_name = "ntree"
sources = ['ntree.cpp']

# Create an extension module
ext_modules = [
    Extension(
        module_name,
        sources,
        include_dirs=[pybind11.get_include()],
        language='c++',
    ),
]

# Setup function
def setup_package():
    setup(
        name=module_name,
        version='1.0',
        author='Your Name',
        description='N-Tree implementation for taxonomic tree',
        ext_modules=ext_modules,
        install_requires=['pybind11>=2.8'],
        setup_requires=['pybind11>=2.8'],
        python_requires='>=3.6',
    )

# Run setup
if __name__ == '__main__':
    setup_package()
