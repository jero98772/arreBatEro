import os
import sys

sys.path.insert(0, os.path.abspath('../src'))

extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.intersphinx',
    'sphinx_rust_domain',
]

project = 'YourProjectName'
author = 'YourName'
version = '0.1'
release = '0.1.0'
language = 'en'

html_theme = 'alabaster'

intersphinx_mapping = {
    'python': ('https://docs.python.org/3', None),
}