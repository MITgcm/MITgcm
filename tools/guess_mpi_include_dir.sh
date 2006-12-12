#!/bin/sh
mpicommand=`which mpif90  >/dev/null 2>&1`
status=$?
if [ "$status" == "0" ]; then
 mpicommand=`which mpif90`
 mpibin=`echo $mpicommand  | sed s'z\(.*\)/[^/]*$z\1'z`
 mpiroot=`echo $mpibin     | sed s'z\(.*\)/[^/]*$z\1'z`
 mpi_idir=$mpiroot"/include"
else
 mpi_idir=""
fi
echo $mpi_idir
