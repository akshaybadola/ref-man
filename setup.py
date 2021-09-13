#!/usr/bin/env python3

from setuptools import setup

from ref_man import __version__

description = """ref-man server for network requests.

    Network requests and xml parsing can be annoying in emacs, so ref-man uses
    a separate python process for efficient (and sometimes parallel) fetching
    of network requests."""

setup(
    name="ref-man",
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
        'Programming Language :: Python :: 3.9',
        "Natural Language :: English",
        "Topic :: Education",
        "Topic :: Scientific/Engineering",
        "Topic :: Text Editors :: Emacs",
    ],
    packages=["ref_man"],
    include_package_data=True,
    python_requires=">=3.6.9",
    install_requires=["flask==1.1.2",
                      "requests>=2.24.0",
                      "beautifulsoup4>=4.9.1",
                      "psutil>=5.8.0",
                      "common_pyutil>=0.7.2"],
    entry_points={
        'console_scripts': [
            'ref-man = ref_man:__main__.main',
        ],
    },
)
