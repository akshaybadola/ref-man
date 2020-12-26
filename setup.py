#!/usr/bin/env python

from setuptools import setup, find_packages

from ref_man import __version__

description = """ref-man server for network requests.

    Network requests and xml parsing can be annoying in emacs, so ref-man uses
    a separate python process for efficient (and sometimes parallel) fetching
    of network requests."""

setup(
    name="ref-man-server",
    version=__version__,
    description="Ref Man Python Server",
    long_description=description,
    url="https://github.com/akshaybadola/ref-man",
    author="Akshay Badola",
    license="GPL",
    classifiers=[
        "Development Status :: 2 - Pre-Alpha",
        "Intended Audience :: Science/Research",
        ("License :: OSI Approved :: "
         "GNU General Public License v3 or later (GPLv3+)"),
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
        "Natural Language :: English",
        "Topic :: Text Editors :: Emacs",
    ],
    packages=find_packages(),
    include_package_data=True,
    install_requires=["flask==1.1.2", "requests==2.24.0", "beautifulsoup4==4.9.1"],
    entry_points={
        'console_scripts': [
            'ref-man = ref_man:main',
        ],
    },
)
