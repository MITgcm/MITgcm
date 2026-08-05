#! /usr/bin/env bash

#- script to run the sequence of multiple Divided-Adjoint runs:
#  no argment -> using non-MPI built AD-executable
#  single-arg -> using MPI built executable and "mpirun -np ${single-arg}" command

#- number of additional executions to perform is given by "add_DIVA_runs"
#  and corresponds to "nchklev_3" value in file "code_ad/tamc.h"
#- take it from file "run_ADM_DIVA" (as done in testreport):
adm_diva_nb=`sed -n '/^ *add_DIVA_runs *=/p' run_ADM_DIVA | sed 's/ //g'`
echo " Divided Adjoint Run: $adm_diva_nb"
eval "let $adm_diva_nb"
#- or set-it directly:
#add_DIVA_runs=4

extraRuns=`expr $add_DIVA_runs - 1`	
if test $# = 0 ; then
    rm -f costfunction*0000 costfinal divided.ctrl snapshot*
#- not MPI run:
    echo "Run $add_DIVA_runs times + final run:"
    for ii in `seq 0 $extraRuns` ; do
      echo " --> starts DIVA run # $ii :"
      ./mitgcmuv_ad > output_adm.txt.diva_${ii}
      echo -n " <-- DIVA run # $ii : done"
      if test -f divided.ctrl ; then
        echo -n ", divided.ctrl :" ; cat divided.ctrl
      else  echo ", no 'divided.ctrl' file" ; fi
#     if test $ii = 2 ; then exit $ii ; fi
    done
    echo " --> Final DIVA run :"
    ./mitgcmuv_ad > output_adm.txt
    echo -n " <-- Final run : done"
    if test -f divided.ctrl ; then echo -n ", divided.ctrl :" ; cat divided.ctrl
    else  echo "" ; fi
else
  if [ $1 -ge 1 ] ; then
    rm -f costfunction*0000 costfinal divided.ctrl snapshot*
#- MPI run on $1 procs (note: may need to edit mpirun command):
    echo "Run $add_DIVA_runs times + final run (use 'mpirun -np $1' ):"
    for ii in `seq 0 $extraRuns` ; do
      echo " --> starts DIVA run # $ii :"
      mpirun -np $1 ./mitgcmuv_ad
      echo -n " <-- DIVA run # $ii : done"
      mv -f STDOUT.0000 STDOUT.0000.diva_${ii}
      if test -f divided.ctrl ; then
        echo ", divided.ctrl :"
        cat divided.ctrl
      else  echo ", no 'divided.ctrl' file" ; fi
    done
    echo " --> Final DIVA run :"
    mpirun -np $1 ./mitgcmuv_ad
    echo -n " <-- Final run : done"
    if test -f divided.ctrl ; then echo ", divided.ctrl :" ; cat divided.ctrl
    else  echo "" ; fi
  fi
fi
