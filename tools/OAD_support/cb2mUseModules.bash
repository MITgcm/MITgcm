#!/bin/bash
file=$1
wantMPI=$2
needMpi=false
cp ${file} ${file}.tmp
for includeFile in `egrep "\# *include *\"" ${file} | sed 's/# *include *\"\(.*\)\"/\1/' | sort | uniq`
do 
  modFileName=${includeFile%.h}_mod.h
  if [ -f ${modFileName} ] 
  then 
    echo  "s/\# *include *\(\"${includeFile}\"\)/\#include \"${modFileName}\"/" > temp.sed
    # add in the mpi include here since we filtered it from the module
    if [ "$modFileName" == "EESUPPORT_mod.h" ] 
    then 
      needMpi=true
    fi
    cat ${file}.tmp | sed -f temp.sed | sed 's/IMPLICIT NONE//' | sed 's/implicit none//' > ${file}.tmp.1
    mv ${file}.tmp.1 ${file}.tmp
  fi
done
if [ "$needMpi" == "true" -a -n "$wantMPI" ] 
then 
   cat > cb2mUseMPI.awk <<EOF
BEGIN { useSeq = 0; inSR=0}
/^ *SUBROUTINE/ { inSR=1 } 
/^#include /  { if (inSR == 1) { useSeq = 1 ; print } }
/^#ifdef/ || /^#endif/ { if (useSeq == 1) print } 
!/^#include / && !/^#ifdef/ && !/^#endif/ {  if (useSeq == 1) { printf("#include \"mpif.h\"\n"); useSeq=0; inSR=0 } }
{if (useSeq == 0) print }
END {}
EOF
  awk -f cb2mUseMPI.awk ${file}.tmp > ${file}.tmp.1
  mv ${file}.tmp.1 ${file}.tmp
  rm -f cb2mUseMPI.awk
fi
mv ${file}.tmp ${file%.F}_cb2m.FF90
