
#!/bin/csh
# invoking mpirun at AERs Linux cluster beehive
#
foreach node (01 02 03 04 05 06 07)
echo node$node
rsh -n beehive$node mkdir /client/scratch/heimbach
rsh -n beehive$node rm -rf /client/scratch/heimbach/exe
rsh -n beehive$node mkdir /client/scratch/heimbach/exe
end
#
# the default dir is globally visible.
# all ctrl related output is written to this dir.
# the mdsioLocalDir is /client/scratch/...
#
rm -rf /cluster/scratch/month01/ecco-exe/
mkdir /cluster/scratch/month01/ecco-exe/
cd /cluster/scratch/month01/ecco-exe/
cp -f /cluster/scratch/month01/heimbach/ecco-branch/verification/global2x2_tot/input/* .
ln -s /cluster/scratch/month01/ponte/UV_2x2/* .
#
# the local scratch dir /client/scratch/...
# is specified in the "data" file, variable mdsioLocalDir
#
set exec = /cluster/scratch/month01/heimbach/ecco-branch/exe/mitgcmuv
/opt/mpich/bin/mpirun.ch_gm -np 6 --gm-kill 7 --gm-v $exec
exit
