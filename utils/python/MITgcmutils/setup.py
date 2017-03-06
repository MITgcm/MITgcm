from setuptools import setup

setup(name='MITgcmutils',
      version='0.1.0',
      description='Python utilities for MITgcm',
      maintainer='Oliver Jahn',
      maintainer_email='jahn@mit.edu',
      url='http://mitgcm.org/',
      packages=['MITgcmutils', 'MITgcmutils.cs'],
      scripts=['scripts/gluemncbig'],
      install_requires=['numpy'],
      extras_require={
        'plot':  ['matplotlib'],
      }
)
