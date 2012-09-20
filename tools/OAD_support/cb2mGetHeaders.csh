#!/bin/csh -f
set file=${1:r}
set awkScript=${2} 
awk -f ${awkScript} ${file}.h  | grep -v mpif.h > ${file}.h_temp
shift
shift   
foreach name ($*)
  cat ${file}.h_temp | grep -v ${name}.h > ${file}.h_t1
  mv ${file}.h_t1 ${file}.h_temp
end
echo '       USE '${file}'_mod' >> ${file}.h_temp
mv ${file}.h_temp ${file}_mod.h
