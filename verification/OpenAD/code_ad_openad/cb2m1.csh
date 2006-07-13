#!/bin/csh -f
foreach f ( $* )
  echo 'converting '$f.h
  awk -f cb2m1.awk $f.h                > ${f}_mod.FF90_temp.001
  echo '      MODULE '${f}_mod       > ${f}_mod.FF90_temp.002
  echo '#include "CPP_OPTIONS.h"'    >> ${f}_mod.FF90_temp.002
  echo '#include "CPP_EEOPTIONS.h"'    >> ${f}_mod.FF90_temp.002
  if ( ${f} != 'SIZE' ) then
    echo '      use SIZE_mod'          >> ${f}_mod.FF90_temp.002
  endif
  if ( ${f} != 'EEPARAMS' &&  ${f} != 'SIZE') then
    echo '      use EEPARAMS_mod'      >> ${f}_mod.FF90_temp.002
  endif
  if ( ${f} != 'PARAMS' && ${f} != 'EEPARAMS' &&  ${f} != 'SIZE') then
    echo '      use PARAMS_mod'	       >> ${f}_mod.FF90_temp.002
  endif
  cat ${f}_mod.FF90_temp.001          >> ${f}_mod.FF90_temp.002
  echo '      END MODULE' ${f}_mod   >> ${f}_mod.FF90_temp.002
  cp ${f}_mod.FF90_temp.002 ${f}_mod.FF90
end
\rm *_mod.FF90_temp.*

foreach f ( *_mod.FF90 )
  grep '^#' $f > ${f:r}.h_temp.001
  echo '       USE '${f:r} >> ${f:r}.h_temp.001
  cp ${f:r}.h_temp.001 ${f:r}.h
end
\rm *_mod.h_temp.*
