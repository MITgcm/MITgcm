#!/bin/csh
# Example PBS script to run a job on the myrinet-3 cluster.
# The lines beginning #PBS set various queuing parameters.
#
# o -N Job Name
#PBS -N global2x2
#
# o -l resource lists that control where job goes
#      here we ask for 3 nodes, each with the attribute "p4".
#PBS -l nodes=6:p4
#
# o Where to write output
#PBS -e stderr
#PBS -o stdout
#
# o Export all my environment variables to the job
#PBS -V
#
#
echo $PBS_NODEFILE
cat  $PBS_NODEFILE
#
cd /s07/heimbach/ecco-branch/exe
rm -f proc_list *.00?.00?.*
cp ~heimbach/ecco/ecco-branch/exe/mitgcmuv .
#
cat $PBS_NODEFILE >! proc_list
set ncpus = ( `wc -l proc_list | awk '{print $1}'` )
echo 'ncpus = ' $ncpus
set listvar = `cat proc_list`
#
foreach lv ($listvar)
echo 'creating /s/local/1/ecco-exe on ' $lv
rsh -n $lv rm -rf /s/local/1/ecco-exe
rsh -n $lv mkdir /s/local/1/ecco-exe
end
#
/usr/local/pkg/mpi/mpi-1.2.4..8a-gm-1.5/g77/bin/mpirun.ch_gm -machinefile proc_list --gm-kill 7 -v -np $ncpus ./mitgcmuv
#
set out=$?
echo 'end with status' $out

