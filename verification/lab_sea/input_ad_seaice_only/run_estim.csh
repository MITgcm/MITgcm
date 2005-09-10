#!/bin/csh
#
# initial settings:
#
set iter   = 0
set fcfg   = 321429.308
set cutoff = 10
#
# run it:
#-----------------------------------------
#
if ( ${iter} == 0 ) then
  \rm ecco_c* costf* run.out*
  \rm optim/ecco_c* optim/op_* optim/OPWARM*
endif
#
#---------
loopstart:
#---------
#
if ( ${iter} < 10 ) then
  set whichiter = 000${iter}
else
  set whichiter = 00${iter}
endif
#
echo 'entering iter ' ${whichiter}
#
if ( ${iter} > 0 ) then
  ln -s optim/ecco_ctrl_MIT_CE_000.opt${whichiter} .
endif
#
cat >! data.optim <<EOF
 &OPTIM
 optimcycle=${iter},
 numiter=1,
 nfunc=30,
 fmin=${fcfg},
 iprint=10,
 nupdate=4,
 &
EOF
#
./mitgcmuv_ad >! output.txt.${whichiter}
#
cd optim/
if ( ${iter} == 0 ) then
  ln -s ../ecco_ctrl_MIT_CE_000.opt${whichiter} .
endif
ln -s ../ecco_cost_MIT_CE_000.opt${whichiter} .
#
./optim.x >! op_${whichiter}
cd ../
#
@ iter = ${iter} + 1
#
if (  ${iter} <= ${cutoff} ) goto loopstart
#


