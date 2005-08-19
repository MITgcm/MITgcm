#!/bin/csh -f
set FNAMES = "BAR2.h BARRIER.h CD_CODE_VARS.h CG2D.h CG3D.h DFILE.h DYNVARS.h EEIO.h EEPARAMS.h EESUPPORT.h EOS.h EXCH.h EXCH_JAM.h FC_NAMEMANGLE.h FFIELDS.h GAD.h GLOBAL_MAX.h GLOBAL_SUM.h GRID.h GW.h MONITOR.h MPI_INFO.h PARAMS.h SIZE.h SOLVE_FOR_PRESSURE3D.h SOLVE_FOR_PRESSURE.h SURFACE.h TIMEAVE_STATV.h"
foreach f ( $FNAMES )
  echo 'converting '$f
  awk -f cb2m1.awk $f                  > ${f:r}_mod.F90_temp.001
  echo '      MODULE '${f:r}_mod       > ${f:r}_mod.F90_temp.002
  echo '#include "CPP_OPTIONS.h"'    >> ${f:r}_mod.F90_temp.002
  echo '#include "CPP_EEOPTIONS.h"'    >> ${f:r}_mod.F90_temp.002
  if ( ${f:r} != 'SIZE' ) then
    echo '      use SIZE_mod'          >> ${f:r}_mod.F90_temp.002
  endif
  if ( ${f:r} != 'EEPARAMS' &&  ${f:r} != 'SIZE') then
    echo '      use EEPARAMS_mod'      >> ${f:r}_mod.F90_temp.002
  endif
  if ( ${f:r} != 'PARAMS' && ${f:r} != 'EEPARAMS' &&  ${f:r} != 'SIZE') then
    echo '      use PARAMS_mod'	       >> ${f:r}_mod.F90_temp.002
  endif
  cat ${f:r}_mod.F90_temp.001          >> ${f:r}_mod.F90_temp.002
  echo '      END MODULE' ${f:r}_mod   >> ${f:r}_mod.F90_temp.002
  cp ${f:r}_mod.F90_temp.002 ${f:r}_mod.F90
end
\rm *_mod.F90_temp.*

foreach f ( *_mod.F90 )
  grep '^#' $f > ${f:r}.h_temp.001
  echo '       USE '${f:r} >> ${f:r}.h_temp.001
  cp ${f:r}.h_temp.001 ${f:r}.h
end
\rm *_mod.h_temp.*

