#!/bin/csh -f
set fileName=${1:r}
set awkScript=${2}    
echo '      MODULE '${fileName}_mod   > ${fileName}_temp
echo '#include "CPP_OPTIONS.h"'      >> ${fileName}_temp
echo '#include "CPP_EEOPTIONS.h"'    >> ${fileName}_temp
echo '#include "GMREDI_OPTIONS.h"'   >> ${fileName}_temp
if ( ${fileName} != 'SIZE' &&  ${fileName} != 'MNC_COMMON' &&  ${fileName} != 'EEPARAMS' ) then
  echo '      use SIZE_mod'          >> ${fileName}_temp
endif
if ( ${fileName} != 'EEPARAMS' &&  ${fileName} != 'SIZE' &&  ${fileName} != 'MNC_COMMON' ) then
  echo '      use EEPARAMS_mod'      >> ${fileName}_temp
endif
if ( ${fileName} != 'PARAMS' && ${fileName} != 'EEPARAMS' &&  ${fileName} != 'SIZE' &&  ${fileName} != 'MNC_COMMON' && ${fileName} != 'GAD' && ${fileName} != 'GRID' ) then
  echo '      use PARAMS_mod'	     >> ${fileName}_temp
endif
if ( ${fileName} == 'ctrl_dummy' || ${fileName} == 'CTRL_GENARR' ) then
  echo '      use CTRL_SIZE_mod'     >> ${fileName}_temp
endif
#echo awk -f ${awkScript} ${fileName}.h   
awk -f ${awkScript} ${fileName}.h | grep -v mpif.h   >> ${fileName}_temp
echo '      END MODULE' ${fileName}_mod   >> ${fileName}_temp
cp ${fileName}_temp ${fileName}_mod.FF90
\rm ${fileName}_temp
