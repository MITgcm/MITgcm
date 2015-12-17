#!/bin/bash
#SBATCH -J trintel
#SBATCH -p sched_mit_darwin
#SBATCH --mem-per-cpu 4000 
#SBATCH -n 6
#SBATCH -N 2
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
module add engaging/intel/2013.1.046

# to make sort order case-insensitive
export LC_ALL=en_US.iso885915

doieee=0
dompi=0
dompidevel=1
dompifast=1
dodarwin=0
dodarwin2=0
dodarwin2mpi=0

dofast=0
dodarwinmpi=0

dirpre=engaging1-ifort
pre=ifort
defopts="-j 7"
#cleanopts="-postclean"
cleanopts=
trmpiopts="-MPI $NSLOTS"
optfile="../tools/build_options/linux_amd64_ifort+impi"
mpioptfile="../tools/build_options/linux_amd64_ifort+impi"
mpicommand="mpirun -env I_MPI_DEBUG 2 -n TR_NPROC ./mitgcmuv"

# NONE will not send email, but keep rs_* directory
#mailrcpt="-a NONE"
mailrcpt="-a jmc@mitgcm.org"
mailfw="$mailrcpt"
mailrs="$mailrcpt"
maildf="jahn@mitgcm.org"
mailhost=eofe4

# relative to MITgcm/verification
refdir=../../ref

cpmodel() {
    newgcmDIR="$1"
    if test -e $newgcmDIR ; then
        rm -rf $newgcmDIR
    fi
    cp -a MITgcm $newgcmDIR
}

cpmodel1() {
    newgcmDIR="$1"
    if test -e $newgcmDIR ; then
        rm -rf $newgcmDIR
    fi
    cp -a MITgcm_darwin1 $newgcmDIR
}

cpmodel2() {
    newgcmDIR="$1"
    if test -e $newgcmDIR ; then
        rm -rf $newgcmDIR
    fi
    cp -a MITgcm_darwin2 $newgcmDIR
}

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

maildiff2p2() {
    summaryref="$1"
    odir="$2"
    subject="$3"
    npassref=`grep -c -e ' pass ' $summaryref`
    ntotref=`grep -c -e ' \(pass\|FAIL\|N/O\) ' $summaryref`
    summary=`pwd`/`ls rs_${odir}_*/summary.txt`
    npass=`grep -c -e ' pass ' $summary`
    ntot=`grep -c -e ' \(pass\|FAIL\|N/O\) ' $summary`
    echo "npass=$npass"
    echo "ntot=$ntot"
    diff -I 'End time:' -I 'on :' -I run: "$summaryref" $summary | grep -v -e 'run:' -e 'on :' > diff.out
    difflen=`cat diff.out | wc -l`
    ssh $mailhost "cat $summary `pwd`/diff.out | mail -s '"$subject$odir" 2+2 "$npass"/"$ntot" ["$npassref"/"$ntotref"] "$difflen"' $maildf"
}

########################################################################
if test $dompi -ne 0; then

    sfx=mpi
    dirsfx=$pre-$sfx
    odir="$dirpre-$sfx"

    echo '--------------------------------------------------------------------------------'
    echo "$sfx: MITgcm_$dirsfx $odir"

    cpmodel MITgcm_$dirsfx

    cd MITgcm_$dirsfx/verification

    echo ./testreport $defopts $trmpiopts -optfile "$mpioptfile" -command "$mpicommand" -odir "$odir" $mailfw
    ./testreport $defopts $trmpiopts -optfile "$mpioptfile" -command "$mpicommand" -odir "$odir" $mailfw

    maildiff "$refdir/summary_$dirsfx.txt" "$odir"

    echo ../tools/do_tst_2+2 -mpi -exe "$mpicommand" -o "$odir" $mailrs
    ../tools/do_tst_2+2 -mpi -exe "$mpicommand" -o "$odir" $mailrs

    maildiff2p2 "$refdir/summary_2+2_$dirsfx.txt" "$odir"

    for d in */build; do make -C $d Clean; done

    cd ../..

    THEDATE=`date`
    echo '--------------------------------------------------------------------------------'
    echo "End $dirsfx $THEDATE"
    echo '--------------------------------------------------------------------------------'

fi

########################################################################
if test $doieee -ne 0; then

    sfx=
    dirsfx=$pre
    odir="$dirpre"

    echo '--------------------------------------------------------------------------------'
    echo "$sfx: MITgcm_$dirsfx $odir"

    cpmodel MITgcm_$dirsfx

    cd MITgcm_$dirsfx/verification

    echo ./testreport $defopts -optfile "$optfile" -odir "$odir" $mailfw
    ./testreport $defopts $cleanopts -optfile "$optfile" -odir "$odir" $mailfw

    maildiff "$refdir/summary_$dirsfx.txt" "$odir"

    cd ../..

    THEDATE=`date`
    echo '--------------------------------------------------------------------------------'
    echo "End $dirsfx $THEDATE"
    echo '--------------------------------------------------------------------------------'

fi

########################################################################
if test $dompidevel -ne 0; then

    sfx=mpi-dvlp
    options="$defopts $trmpiopts -devel"
    dirsfx=$pre-$sfx
    odir="$dirpre-dvlp"

    echo '--------------------------------------------------------------------------------'
    echo "$sfx: MITgcm_$dirsfx $odir"

    cpmodel MITgcm_$dirsfx

    cd MITgcm_$dirsfx/verification

    echo ./testreport $options -optfile "$mpioptfile" -command "$mpicommand" -odir "$odir" $mailfw
    ./testreport $options -optfile "$mpioptfile" -command "$mpicommand" -odir "$odir" $mailfw

    maildiff "$refdir/summary_$dirsfx.txt" "$odir"

    echo ../tools/do_tst_2+2 -mpi -exe "$mpicommand" -o "$odir" $mailrs
    ../tools/do_tst_2+2 -mpi -exe "$mpicommand" -o "$odir" $mailrs

    maildiff2p2 "$refdir/summary_2+2_$dirsfx.txt" "$odir"

    for d in */build; do make -C $d Clean; done

    cd ../..

    THEDATE=`date`
    echo '--------------------------------------------------------------------------------'
    echo "End $dirsfx $THEDATE"
    echo '--------------------------------------------------------------------------------'

fi

########################################################################
if test $dompifast -ne 0; then

    sfx=mpi-fast
    options="$defopts $trmpiopts -fast"
    dirsfx=$pre-$sfx
    odir="$dirpre-fast"

    echo '--------------------------------------------------------------------------------'
    echo "$sfx: MITgcm_$dirsfx $odir"

    cpmodel MITgcm_$dirsfx

    cd MITgcm_$dirsfx/verification

    echo ./testreport $options -optfile "$mpioptfile" -command "$mpicommand" -odir "$odir" $mailfw
    ./testreport $options -optfile "$mpioptfile" -command "$mpicommand" -odir "$odir" $mailfw

    maildiff "$refdir/summary_$dirsfx.txt" "$odir"

    echo ../tools/do_tst_2+2 -mpi -exe "$mpicommand" -o "$odir" $mailrs
    ../tools/do_tst_2+2 -mpi -exe "$mpicommand" -o "$odir" $mailrs

    maildiff2p2 "$refdir/summary_2+2_$dirsfx.txt" "$odir"

    for d in */build; do make -C $d Clean; done

    cd ../..

    THEDATE=`date`
    echo '--------------------------------------------------------------------------------'
    echo "End $dirsfx $THEDATE"
    echo '--------------------------------------------------------------------------------'

fi

########################################################################
if test $dofast -ne 0; then

    sfx=fast

    options="$defopts -fast $cleanopts"
    dirsfx=$pre-$sfx
    odir="$dirpre-$sfx"

    echo '--------------------------------------------------------------------------------'
    echo "$sfx: MITgcm_$dirsfx $odir"

    cpmodel MITgcm_$dirsfx

    cd MITgcm_$dirsfx/verification

    echo ./testreport $options -optfile "$optfile" -odir "$odir" $mailfw
    ./testreport $options -optfile "$optfile" -odir "$odir" $mailfw

    maildiff "$refdir/summary_$dirsfx.txt" "$odir"

    cd ../..

    THEDATE=`date`
    echo '--------------------------------------------------------------------------------'
    echo "End $dirsfx $THEDATE"
    echo '--------------------------------------------------------------------------------'

fi

########################################################################
if test $dodarwinmpi -ne 0; then

    sfx=mpi

    options="$defopts $trmpiopts $cleanopts"
    dirsfx=$pre-$sfx
    odir="$dirpre-darwin-$sfx"

    echo '--------------------------------------------------------------------------------'
    echo "$sfx: MITgcm_$dirsfx $odir"

    gcmDIR="MITgcm_darwin_$dirsfx"
    cpmodel $gcmDIR

    cd $gcmDIR/verification

    exps="`ls -d darwin_*`"

    echo ./testreport $options -optfile "$mpioptfile" -command "$mpicommand" -odir "$odir" $mailfw -t "$exps"
    ./testreport $options -optfile "$mpioptfile" -command "$mpicommand" -odir "$odir" $mailfw -t "$exps"

    maildiff "$refdir/summary_darwin_$dirsfx.txt" "$odir"

    #echo ../tools/do_tst_2+2 -mpi -exe "$mpicommand" -o "$odir" $mailrs
    #../tools/do_tst_2+2 -mpi -exe "$mpicommand" -o "$odir" $mailrs
    #
    #maildiff2p2 "$refdir/summary_2+2_$dirsfx.txt" "$odir"

    cd ../..

    THEDATE=`date`
    echo '--------------------------------------------------------------------------------'
    echo "End $dirsfx $THEDATE"
    echo '--------------------------------------------------------------------------------'

fi

########################################################################
if test $dodarwin -ne 0; then

    sfx=

    options="$defopts"
    dirsfx=$pre
    odir="$dirpre-darwin"

    echo '--------------------------------------------------------------------------------'
    echo "$sfx: MITgcm_$dirsfx $odir"

    gcmDIR="MITgcm_darwin_$dirsfx"
    cpmodel1 $gcmDIR

    cd $gcmDIR/verification

    exps="`ls -d darwin_*`"

    echo ./testreport $options -optfile "$optfile" -odir "$odir" $mailfw -t "$exps"
    ./testreport $options -optfile "$optfile" -odir "$odir" $mailfw -t "$exps"

    maildiff "$refdir/summary_darwin_$dirsfx.txt" "$odir"

    echo ../tools/do_tst_2+2 -o "$odir" $mailrs
    ../tools/do_tst_2+2 -o "$odir" $mailrs

    maildiff2p2 "$refdir/summary_2+2_darwin_$dirsfx.txt" "$odir"

    for d in */build; do make -C $d Clean; done

    cd ../..

    THEDATE=`date`
    echo '--------------------------------------------------------------------------------'
    echo 'End job '$THEDATE
    echo '--------------------------------------------------------------------------------'

fi

########################################################################
if test $dodarwin2mpi -ne 0; then

    sfx=mpi

    options="$defopts $trmpiopts $cleanopts"
    dirsfx=$pre-$sfx
    odir="$dirpre-darwin2-$sfx"

    echo '--------------------------------------------------------------------------------'
    echo "$sfx: MITgcm_$dirsfx $odir"

    gcmDIR="MITgcm_darwin2_$dirsfx"
    cpmodel2 $gcmDIR

    cd $gcmDIR/verification

    exps="`ls -d monod_* quota_*`"

    echo ./testreport $options -optfile "$mpioptfile" -command "$mpicommand" -odir "$odir" $mailfw -t "$exps"
    ./testreport $options -optfile "$mpioptfile" -command "$mpicommand" -odir "$odir" $mailfw -t "$exps"

    #maildiff "$refdir/summary_darwin2_$dirsfx.txt" "$odir" "darwin2 "
    maildiff "$refdir/summary_darwin2_$dirsfx.txt" "$odir"

    #echo ../tools/do_tst_2+2 -mpi -exe "$mpicommand" -o "$odir" $mailrs
    #../tools/do_tst_2+2 -mpi -exe "$mpicommand" -o "$odir" $mailrs
    #
    #maildiff2p2 "$refdir/summary_2+2_$dirsfx.txt" "$odir"

    cd ../..

    THEDATE=`date`
    echo '--------------------------------------------------------------------------------'
    echo "End $dirsfx $THEDATE"
    echo '--------------------------------------------------------------------------------'

fi

########################################################################
if test $dodarwin2 -ne 0; then

    sfx=

    options="$defopts"
    dirsfx=$pre
    odir="$dirpre-darwin2"

    echo '--------------------------------------------------------------------------------'
    echo "$sfx: MITgcm_$dirsfx $odir"

    gcmDIR="MITgcm_darwin2_$dirsfx"
    cpmodel2 $gcmDIR

    cd $gcmDIR/verification

    exps="`ls -d monod_* quota_*`"

    echo ./testreport $options -optfile "$optfile" -odir "$odir" $mailfw -t "$exps"
    ./testreport $options -optfile "$optfile" -odir "$odir" $mailfw -t "$exps"

    maildiff "$refdir/summary_darwin2_$dirsfx.txt" "$odir"

    echo ../tools/do_tst_2+2 -o "$odir" $mailrs
    ../tools/do_tst_2+2 -o "$odir" $mailrs

    maildiff2p2 "$refdir/summary_2+2_darwin2_$dirsfx.txt" "$odir"

    for d in */build; do make -C $d Clean; done

    cd ../..

fi

THEDATE=`date`
echo '================================================================================'
echo 'End job '$THEDATE
echo '********************************************************************************'

