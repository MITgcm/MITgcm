# run on 6 cpu's for 2 hours
# lsc.windf stands for large scale computing
# ac.windf stands for analysis cluster
#
#$ -pe lsc.windf 60 -l h_cpu=8:00:00
#
# if using with DIVA, FTMPDIR must not be deleted,
# or costfinal, divided.ctrl, snapshot* need to be
# saved and re-copied
#if (-e $FTMPDIR ) then
#  rm -f $FTMPDIR/*
#endif
#
cd $FTMPDIR
pwd
#
\cp -f ~/ecco/ecco-branch/exe/mitgcmuv .
\cp -f ~/ecco/ecco-branch/verification/global1x1_tot/input/* .
dmget /archive/psh/data/UV_1x1/*
\cp -f /archive/psh/data/UV_1x1/* .
#
mpirun -np 60 mitgcmuv
#
mv STD* /archive/psh/ecco/ecco-branch/res/.
mv cost* /archive/psh/ecco/ecco-branch/res/.
mv ecco_* /archive/psh/ecco/ecco-branch/res/.
mv ?.00000* /archive/psh/ecco/ecco-branch/res/.
mv adxx_* /archive/psh/ecco/ecco-branch/res/.
mv xx_* /archive/psh/ecco/ecco-branch/res/.
mv pickup* /archive/psh/ecco/ecco-branch/res/.
mv snapshot* /archive/psh/ecco/ecco-branch/res/.
mv divided* /archive/psh/ecco/ecco-branch/res/.
mv tapelev3_1_* /archive/psh/ecco/ecco-branch/res/.
mv tapelev2_2_* /archive/psh/ecco/ecco-branch/res/.

