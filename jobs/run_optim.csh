#!/bin/csh
# script for running line search routine
# * set line search parameters
# * link control and gradient vector
# * generate runtime parameter files
# * run line search
# heimbach@mit.edu 27-May-2003

# set parameters for namelist ECCO_OPTIM
set optimcycle = 0
set numiter    = 1
set nfunc      = 3
set fmin       = 300.
set iprint     = 10
set nupdate    = 4

# set parameters for namelist ECCO_PARMS
set expId = MIT_CE_000

# set parameters for namelist CTRL_NML and CTRL_PACKNAMES
set ctrlname = ecco_ctrl
set costname = ecco_cost

# set system parameters
set mitgcmdir = /cluster/scratch/month01/heimbach/ecco-branch/exe
set optimdir = /cluster/scratch/month01/heimbach/ecco-branch/optim/exe

# IDEALLY, NO MORE EDITING BEYOND THIS LINE.
# ---------------------------------------------------------------------

# do some stuff
#
if ( $optimcycle < 10 ) then
  set optsuffix = 000${optimcycle}
else if ( $optimcycle < 100 ) then
  set optsuffix = 00${optimcycle}
else if ( $optimcycle < 1000 ) then
  set optsuffix = 0${optimcycle}
else
  set optsuffix = ${optimcycle}
endif

# go to wrkdir and proceed:
#
cd $optimdir
rm -f data.*
if ( $optimcycle == 0 ) then
  rm -f OPWARM? PH_?.dat *.opt0001 fort.94
endif

cat >! data.optim << EOF
# ********************************
# Off-line optimization parameters
# ********************************
 &ECCO_OPTIM
 optimcycle = $optimcycle,
 numiter    = $numiter,
 nfunc      = $nfunc,
 fmin       = $fmin,
 iprint     = $iprint,
 nupdate    = $nupdate,
 &
EOF

cat >! data.ecco << EOF
# ***************
# ECCO parameters
# ***************
 &ECCO_PARMS
 expId = '${expId}',
 &
EOF

cat >! data.ctrl << EOF
# *********************
# ECCO controlvariables
# *********************
 &CTRL_NML
 &
# *********************
# names for ctrl_pack/unpack
# *********************
 &CTRL_PACKNAMES
 ctrlname = '${ctrlname}',
 costname = '${costname}',
 &
EOF

ln -s ${mitgcmdir}/${ctrlname}_${expId}.opt${optsuffix} .
ln -s ${mitgcmdir}/${costname}_${expId}.opt${optsuffix} .

./optim.x >&! output_${optsuffix}.txt

exit

