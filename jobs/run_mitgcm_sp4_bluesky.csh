#! /usr/bin/csh
#
setenv MP_RESD yes
setenv MP_RMPOOL 1
setenv MP_CPU_USE multiple
setenv MP_SHARED_MEMORY yes
setenv MP_NODES 1
setenv MP_PROCS 4
#
cd ../exe
poe ./mitgcmuv
cd ../jobs

exit
