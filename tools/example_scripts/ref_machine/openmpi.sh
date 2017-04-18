#! /bin/bash

if [ -z "${PATH}" ] ; then
   PATH="/usr/lib64/openmpi/bin";
else
   PATH="/usr/lib64/openmpi/bin:${PATH}";
fi
   export PATH
   MPI_INC_DIR="/usr/include/openmpi-x86_64"
   export MPI_INC_DIR

#-- in practice, only 1 share-libs location is needed
#  if [ -z "${LD_LIBRARY_PATH}" ] ; then
      LD_LIBRARY_PATH="/usr/lib64/openmpi/lib"
#  else
#     LD_LIBRARY_PATH="/usr/lib64/openmpi/lib:${LD_LIBRARY_PATH}"
#  fi
   export LD_LIBRARY_PATH

  if [ -z "${LIBRARY_PATH}" ] ; then
    LIBRARY_PATH="/usr/lib64/openmpi/lib"
  else
    LIBRARY_PATH="/usr/lib64/openmpi/lib:${LIBRARY_PATH}"
  fi
  export LIBRARY_PATH

if [ -z "${MANPATH}" ] ; then
   MANPATH="/usr/share/man/openmpi-x86_64":$(manpath)
else
   MANPATH="/usr/share/man/openmpi-x86_64:${MANPATH}"
fi
export MANPATH
