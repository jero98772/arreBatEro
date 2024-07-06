from setuptools import setup, Extension
from pybind11.setup_helpers import Pybind11Extension, build_ext

ext_modules = [
    Pybind11Extension(
        'pagerank',
        ['pagerank.cpp', 'bindings.cpp'],
    ),
]

setup(
    name='pagerank',
    ext_modules=ext_modules,
    cmdclass={'build_ext': build_ext},
)
