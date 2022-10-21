from os import path

from setuptools import setup
import sysconfig

HERE = path.abspath(path.dirname(__file__))

with open(path.join(HERE, "README.md"), encoding="utf-8") as fp:
    README = fp.read()

setup(
    name='kivra_api_errors',
    version='1.0.3',
    description='Python interface for Kivra API errors',
    py_modules = ['kivra_api_errors'],
    data_files = [('/kivra_api_errors_data', ['api-errors.json'])],
    python_requires="~=3.9",
    long_description=README,
    package_data={'': ['LICENSE']},
)
