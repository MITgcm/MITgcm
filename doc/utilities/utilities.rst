Utilities
*********

.. _MITgcmutils:

MITgcmutils
===========

This Python package includes a number of helpful functions and scripts for
dealing with MITgcm output.  You can install it from the model repository (in
directory :filelink:`utils/python/MITgcmutils`) or from the Python Package
Index:

::

    pip install --user MITgcmutils

The following functions are exposed at the package level:

- from module mds: :meth:`~MITgcmutils.mds.rdmds` and
  :meth:`~MITgcmutils.mds.wrmds`
- from module mnc: :meth:`~MITgcmutils.mnc.rdmnc` and
  :meth:`~MITgcmutils.mnc.mnc_files`
- from module ptracers: :meth:`~MITgcmutils.ptracers.iolabel` and:
  :meth:`~MITgcmutils.ptracers.iolabel2num`
- from module diagnostics: :meth:`~MITgcmutils.diagnostics.readstats`

The package also includes a standalone script for joining tiled mnc files:
gluemncbig_.

For more functions, see the individual modules:

mds
---

.. automodule:: MITgcmutils.mds
    :members:

mnc
---

.. automodule:: MITgcmutils.mnc
    :members:

diagnostics
-----------

.. automodule:: MITgcmutils.diagnostics
    :members:

ptracers
--------

.. automodule:: MITgcmutils.ptracers
    :members:

density
-------

.. automodule:: MITgcmutils.density
    :members:

miscellaneous utilities
-----------------------

.. automodule:: MITgcmutils.utils
    :members:

conversion
----------

.. automodule:: MITgcmutils.conversion
    :members:

cs
--

.. automodule:: MITgcmutils.cs
    :members:

llc
---

.. automodule:: MITgcmutils.llc
    :members:

examples
--------

.. automodule:: MITgcmutils.examples
    :members:

.. _gluemncbig:

gluemncbig
----------

This command line script is part of MITgcmutils and provides a convenient
method for stitching together NetCDF files into a single file covering the
model domain. Be careful though - the resulting files can get very large.

.. program-output:: ../utils/python/MITgcmutils/scripts/gluemncbig --help
