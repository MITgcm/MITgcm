import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="MITgcmutils",
    version="0.1.2",
    author="MITgcm Developers and Contributors",
    author_email="mitgcm-support@mitgcm.org",
    description="Python utilities for MITgcm",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="http://mitgcm.org/",
    packages=setuptools.find_packages(),
    scripts=["scripts/gluemncbig"],
    classifiers=[
        "Programming Language :: Python",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    install_requires=["numpy"],
    extras_require={
        "plot":  ["matplotlib"],
    }
)
