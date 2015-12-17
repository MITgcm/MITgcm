#!/bin/bash
#SBATCH -J troadgfortran
#SBATCH -p sched_mit_darwin
#SBATCH --mem-per-cpu 4000 
#SBATCH -n 1
#SBATCH -N 1
NSLOTS=$SLURM_NTASKS
echo '********************************************************************************'
THEDATE=`date`
echo 'Start job '$THEDATE
echo 'NSLOTS = '$NSLOTS
echo '======= NODELIST ==============================================================='
echo $SLURM_NODELIST
echo '======= env ===================================================================='
env
echo '================================================================================'

source /etc/profile.d/modules.sh
module add gcc mvapich2/gcc slurm harvard/centos6/hdf5-1.8.11_gcc-4.4.7 harvard/centos6/netcdf-4.3.0_gcc-4.4.7
module use /home/jahn/software/modulefiles
module add openad/20140116

# to make sort order case-insensitive
export LC_ALL=en_US.iso885915

comp=gfortran
optfile="../tools/build_options/linux_amd64_gfortran"

ulimit -s unlimited

# to make sort order case-insensitive
export LC_ALL=en_US.iso885915

mail="-a jmc@mitgcm.org"
maildf="jahn@mitgcm.org"
mailhost=eofe4

# relative to MITgcm/verification
refdir=../../ref

#defopts="-j 2"
defopts=""

maildiff() {
    summaryref="$1"
    odir="$2"
    subject="$3"
    npassref=`grep -c -e ' pass ' $summaryref`
    ntotref=`grep -c -e ' \(pass\|FAIL\|N/O\) ' $summaryref`
    summary=`pwd`/`ls tr_${odir}_*/summary.txt`
    npass=`grep -c -e ' pass ' $summary`
    ntot=`grep -c -e ' \(pass\|FAIL\|N/O\) ' $summary`
    echo "npass=$npass"
    echo "ntot=$ntot"
    diff -I time: -I 'on :' -I run: $summaryref $summary | grep -v -e 'run:' -e 'on :' > diff.out
    difflen=`cat diff.out | wc -l`
    ssh $mailhost "cat $summary `pwd`/diff.out | mail -s '"$subject$odir" "$npass"/"$ntot" ["$npassref"/"$ntotref"] "$difflen"' $maildf"
}

cpmodel() {
    newgcmDIR="$1"
    if test -e $newgcmDIR ; then
        rm -rf $newgcmDIR
    fi
    cp -a MITgcm $newgcmDIR
}

echo '================================================================================'

gcmDIR="MITgcm_openad_$comp"

cpmodel $gcmDIR

sfx=${comp}-openad
odir="engaging1-openad"

cd $gcmDIR/verification

echo ./testreport -oad -optfile "$optfile" -odir "$odir" $mail
./testreport -oad -optfile "$optfile" -odir "$odir" $mail

maildiff "$refdir/summary_$sfx.txt" "$odir"

cd ../..

THEDATE=`date`
echo '================================================================================'
echo "End $sfx $THEDATE"
echo '================================================================================'

