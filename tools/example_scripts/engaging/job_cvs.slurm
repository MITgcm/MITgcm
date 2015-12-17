#!/bin/bash
#SBATCH -J trcvs
#SBATCH -p sched_mit_darwin
#SBATCH --mem-per-cpu 4000 
#SBATCH -n 1
#SBATCH -N 1
echo '********************************************************************************'
THEDATE=`date`
echo 'Start job '$THEDATE
echo '======= env ===================================================================='
env
echo '================================================================================'

dodarwin1=1
dodarwin2=1

cpmodel() {
    newgcmDIR="$1"
    if test -e $newgcmDIR ; then
        rm -rf $newgcmDIR
    fi
    cp -a MITgcm $newgcmDIR
}

########################################################################
# check MITgcm out of CVS

export CVS_RSH=ssh
cvsroot=:pserver:cvsanon@mitgcm.org:/u/gcmpack

gcmDIR="MITgcm"
if test -e $gcmDIR ; then
    rm -rf $gcmDIR
fi
echo "cvs -d $cvsroot co -P -d $gcmDIR MITgcm > cvs.log 2>&1"
cvs -d $cvsroot co -P -d $gcmDIR MITgcm > cvs.log 2>&1 || exit 1

########################################################################
# copy MITgcm and check darwin pkg and verification out of CVS

if [ $dodarwin1 -ne 0 ]; then

    gcmDIR="MITgcm_darwin1"

    cpmodel $gcmDIR

    cd $gcmDIR
    cat /dev/null > cvs_darwin.log
    cd pkg
    for pkg in darwin radtrans; do
        cvs -d $cvsroot co -P -d $pkg MITgcm_contrib/darwin/pkg/$pkg >> cvs_darwin_$dirsfx.log 2>&1
    done
    cd ../verification
    for exp in darwin_1d_carbon darwin_1d_geider darwin_1d_wavebands darwin_1d_radtrans darwin_1d_twospecies darwin_baltic_76x72x24 darwin_global_128x64x15; do
        cvs -d $cvsroot co -P -d $exp MITgcm_contrib/darwin/verification/$exp >> cvs_darwin_$dirsfx.log 2>&1
    done
    cd ../..

fi

echo '================================================================================'

########################################################################
# copy MITgcm and check darwin2 pkg and verification out of CVS

if [ $dodarwin2 -ne 0 ]; then

    gcmDIR="MITgcm_darwin2"

    cpmodel $gcmDIR

    cd $gcmDIR
    cat /dev/null > cvs_darwin2.log
    cd pkg
    for pkg in darwin monod quota radtrans; do
        cvs -d $cvsroot co -P -d $pkg MITgcm_contrib/darwin2/pkg/$pkg >> cvs_darwin2_$dirsfx.log 2>&1
    done
    cd ../verification
    for exp in monod_1d_carbon monod_1d_geider monod_1d_wavebands monod_1d_radtrans monod_1d_twospecies monod_baltic_76x72x24 monod_global_128x64x15 quota_1d; do
        cvs -d $cvsroot co -P -d $exp MITgcm_contrib/darwin2/verification/$exp >> cvs_darwin2_$dirsfx.log 2>&1
    done
    cd ../..

fi

THEDATE=`date`
echo '================================================================================'
echo 'End job '$THEDATE
echo '********************************************************************************'

