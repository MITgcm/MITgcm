#!/bin/csh
# invoking mpirun on ITRDA Linux cluster
#
# o -q queue
#   available on itrda are four (=2hours), long (=168hours)
#PBS -q long
#
# o -N Job name
#PBS -N global2x2
#
# -l resource lists
#PBS -l nodes=6:ppn=2
#
# o STDERR/OUT
#PBS -e stderr
#PBS -o stdout
#
# o export all my environment var's
#PBS -V
printenv
#
# get PBS node info
#
echo $PBS_NODEFILE
cat  $PBS_NODEFILE
#
cat $PBS_NODEFILE >! my_proc_list
set ncpus = ( `wc -l my_proc_list | awk '{print $1}'` )
echo 'ncpus = ' $ncpus
set listvar = `cat my_proc_list`
#
cd /u1/heimbach/ecco/ecco-branch/exe
cp ~heimbach/ecco/ecco-branch/exe/mitgcmuv .
#
echo $ncpus >! my_list
set dualvar = 1
foreach lv ($listvar)
  if ( $dualvar == 1 ) then
    echo $lv 2 >> my_list
    set dualvar = 2
  else
    echo $lv 4 >> my_list
    set dualvar = 1
  endif
end
#
/usr/local/mpich/mpich-1.2.1..7b_pgi/bin/mpirun.ch_gm \
    --gm-f ./my_list --gm-v ./mitgcmuv

exit

