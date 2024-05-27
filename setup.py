"""Stack-based language for concisely building tweakable apps"""

from setuptools import setup, find_namespace_packages
from setup_helpers import __version__

with open("README.md", "r") as fh:
    long_description = fh.read()

setup(
    name="forthic",
    version=__version__,
    author="Rino Jose",
    author_email="rjose@linkedin.com",
    description="A stack-based language for concisely building tweakable apps",
    long_description=long_description,
    long_description_content_type="text/markdown",
    license="BSD 2-CLAUSE LICENSE",
    keywords="forth language",
    url="https://forthic.readthedocs.io",
    download_url="https://github.com/linkedin/forthic",
    packages=find_namespace_packages(
        where=".", exclude=["test*", "docs", "forthic-js", "apps"]
    ),
    package_data={
        "forthic": ["py.typed"],
    },
    # readthedocs builds fail unless zip_safe is False.
    zip_safe=False,
    install_requires=[
        "urllib3==1.26.10",
        "pytz",
        "cryptography",
        "python-dateutil",
        "requests-oauthlib",
        "Flask",
        "Jinja2",
        "markdown",
        "trino",
        "pandas",
    ],
    project_urls={
        "Documentation": "https://forthic.readthedocs.io",
        "Source": "https://github.com/linkedin/forthic",
        "Tracker": "https://github.com/linkedin/forthic/issues",
    },
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: BSD 2-CLAUSE",
        "Operating System :: OS Independent",
        "Intended Audience :: Developers",
        "Development Status :: 5 - Production/Stable",
    ],
    python_requires=">=3.6",
)
