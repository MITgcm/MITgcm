#include "OBSFIT_OPTIONS.h"
C--  File active_file_control_obs_tile_rl.F:
C--   Contents
C--   o active_read_obs_tile_rl
C--   o active_write_obs_tile_rl

CBOP
C     !ROUTINE: ACTIVE_READ_OBS_TILE_RL

C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_OBS_TILE_RL(
     I                                    fid,
     I                                    active_num_file,
     O                                    active_var,
     I                                    lAdInit,
     I                                    irec,
     I                                    irecglob,
     I                                    theSimulationMode,
     I                                    myOptimIter,
     I                                    bi,
     I                                    bj,
     I                                    myThid )

C     !DESCRIPTION: 
C     ==================================================================
C     | Read an active record from an ObsFit .equi. tiled file
C     | (can be netcdf or binary)  
C     ==================================================================

C     !USES: 
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_OBSFIT
# include "netcdf.inc"
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif

C     !INPUT PARAMETERS:
      INTEGER  fid
      INTEGER  active_num_file
      INTEGER  irec, irecglob
      INTEGER  theSimulationMode
      INTEGER  myOptimIter
      INTEGER  bi, bj, myThid
      _RL      active_var
      logical  lAdInit

CEOP

#ifdef ALLOW_OBSFIT
C     !LOCAL VARIABLES:
      INTEGER  err, varid1, varid2
      INTEGER  vec_start, vec_count
      INTEGER  ivar, i
      _RL      active_data_t
      _RL      vec_tmp(2)

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN

       _BEGIN_MASTER( myThid )

       IF (obsfitDoNcOutput) THEN
      
        vec_start=irec
        vec_count=1

        err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
        err = NF_GET_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     &   active_var)

        err = NF_INQ_VARID(fid, obsfit_namemask, varid2)
        err = NF_GET_VARA_DOUBLE(fid, varid2, vec_start, vec_count,
     &   sample_modmask(bi,bj))

       ELSE

        READ(fid,rec=(irec-1)*2+1)vec_tmp
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(2,vec_tmp)
#endif
        active_var=vec_tmp(1)
        READ(fid,rec=(irec-1)*2+2)vec_tmp
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(2,vec_tmp)
#endif
        sample_modmask(bi,bj)=vec_tmp(1)

       ENDIF

       _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. REVERSE_SIMULATION) THEN

       _BEGIN_MASTER( myThid )

       IF (obsfitDoNcOutput) THEN

        vec_start=irec
        vec_count=1

        err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
        err = NF_GET_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     &   active_data_t)

C Add active_var from appropriate location to data
        active_data_t = active_data_t + active_var
C Store the result on disk
        vec_start=irec
        vec_count=1

        err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
        err = NF_PUT_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     &   active_data_t)

C Set active_var to zero
        active_var = 0. _d 0

       ELSE

        READ(fid,rec=(irec-1)*2+1)vec_tmp
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(2,vec_tmp)
#endif
        active_data_t=vec_tmp(1)

C Add active_var from appropriate location to data
        active_data_t = active_data_t + active_var

C Store the result on disk
        vec_tmp(1)=active_data_t
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(2,vec_tmp)
#endif
        WRITE(fid,rec=(irec-1)*2+1)vec_tmp

C Set active_var to zero
        active_var = 0. _d 0

       ENDIF

       _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. TANGENT_SIMULATION) THEN

       _BEGIN_MASTER( myThid )

       IF (obsfitDoNcOutput) THEN

        vec_start=irec
        vec_count=1

      err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
      err = NF_GET_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     & active_var)

      ELSE

      READ(fid,rec=(irec-1)*2+1)vec_tmp
#ifdef _BYTESWAPIO
      CALL MDS_BYTESWAPR8(2,vec_tmp)
#endif
      active_var=vec_tmp(1)

      ENDIF

        _END_MASTER( myThid )

      ENDIF

#endif /* ALLOW_OBSFIT */

      RETURN
      END

C     =================================================================

CBOP
C     !ROUTINE: ACTIVE_WRITE_OBS_TILE_RL

C     !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_OBS_TILE_RL(
     I                                 fid,
     I                                 active_num_file,
     I                                 active_var,
     I                                 irec,
     I                                 irecglob,
     I                                 theSimulationMode,
     I                                 myOptimIter,
     I                                 bi,
     I                                 bj,
     I                                 myThid )

C     !DESCRIPTION: 
C     ==================================================================
C     | Write an active record to an ObsFit .equi. tiled file
C     | (can be netcdf or binary)  
C     ==================================================================

C     !USES: 
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_OBSFIT
# include "netcdf.inc"
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif

C     !INPUT PARAMETERS:
      INTEGER  fid
      INTEGER  active_num_file
      INTEGER  irec, irecglob
      INTEGER  theSimulationMode
      INTEGER  myOptimIter
      INTEGER  bi, bj, myThid
      _RL      active_var
CEOP

#ifdef ALLOW_OBSFIT
C     !LOCAL VARIABLES:
      INTEGER  err, varid1, varid2, varid3
      INTEGER  vec_start, vec_count
      INTEGER  ivar, i
      _RL      active_data_t
      _RL      vec_tmp(2)

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN

       _BEGIN_MASTER( myThid )

       IF (obsfitDoNcOutput) THEN

        vec_start=irec
        vec_count=1

        err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
        err = NF_PUT_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     &   active_var)

        err = NF_INQ_VARID(fid, obsfit_namemask, varid2)
        err = NF_PUT_VARA_DOUBLE(fid, varid2, vec_start, vec_count,
     &   sample_modmask(bi,bj))

        err = NF_INQ_VARID(fid,'sample_ind_glob', varid3 )
        err = NF_PUT_VAR1_INT(fid, varid3, vec_start, irecglob)

       ELSE

        vec_tmp(1)=active_var
        vec_tmp(2)=irecglob
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(2,vec_tmp)
#endif
        WRITE(fid,rec=(irec-1)*2+1)vec_tmp
        vec_tmp(1)=sample_modmask(bi,bj)
        vec_tmp(2)=irecglob
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(2,vec_tmp)
#endif
        WRITE(fid,rec=(irec-1)*2+2)vec_tmp

       ENDIF

       _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. REVERSE_SIMULATION) THEN

       _BEGIN_MASTER( myThid )

       IF (obsfitDoNcOutput) THEN

        vec_start=irec
        vec_count=1
        err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
        err = NF_GET_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     &   active_data_t)

C     Add active_var to data.
        active_var = active_var + active_data_t
        active_data_t = 0. _d 0

        vec_start=irec
        vec_count=1

        err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
        err = NF_PUT_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     &   active_data_t)

       ELSE

        READ(fid,rec=(irec-1)*2+1)vec_tmp
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(2,vec_tmp)
#endif
        active_data_t=vec_tmp(1)

C Add active_var from appropriate location to data.
        active_var = active_var + active_data_t
        active_data_t = 0. _d 0

C Store the result on disk.
        vec_tmp(1)=active_data_t
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(2,vec_tmp)
#endif
        WRITE(fid,rec=(irec-1)*2+1)vec_tmp

       ENDIF

       _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. TANGENT_SIMULATION) THEN

       _BEGIN_MASTER( myThid )

       IF (obsfitDoNcOutput) THEN

        vec_start=irec
        vec_count=1

        err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
        err = NF_PUT_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     &   active_var)

       ELSE

        vec_tmp(1)=active_var
        vec_tmp(2)=irecglob
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(2,vec_tmp)
#endif
        WRITE(fid,rec=(irec-1)*2+1)vec_tmp

       ENDIF

       _END_MASTER( myThid )

      ENDIF

#endif /* ALLOW_OBSFIT */

      RETURN
      END

C     ==================================================================

CBOP
C     !ROUTINE: ACTIVE_READ_OBS_GLOB_RL

C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_OBS_GLOB_RL(
     I                                  fid,
     I                                  active_num_file,
     O                                  active_var,
     I                                  lAdInit,
     I                                  irec,
     I                                  irecglob,
     I                                  theSimulationMode,
     I                                  myOptimIter,
     I                                  myThid )

C     !DESCRIPTION: 
C     ==================================================================
C     | Read an active record from an ObsFit .equi. global file
C     ==================================================================

C     !USES: 
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_OBSFIT
# include "netcdf.inc"
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif

C     !INPUT PARAMETERS:
      INTEGER  fid
      INTEGER  active_num_file
      INTEGER  irec, irecglob
      INTEGER  theSimulationMode
      INTEGER  myOptimIter
      INTEGER  myThid
      _RL      active_var
      logical  lAdInit

CEOP

#ifdef ALLOW_OBSFIT
C     !LOCAL VARIABLES:
      INTEGER  err, varid1, varid2
      INTEGER  vec_start, vec_count
      INTEGER  ivar, i
      _RL      active_data_t
      _RL      vec_tmp(2)

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN

       _BEGIN_MASTER( myThid )
      
        vec_start=irecglob
        vec_count=1

        err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
        err = NF_GET_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     &   active_var)

        err = NF_INQ_VARID(fid, obsfit_namemask, varid2)
        err = NF_GET_VARA_DOUBLE(fid, varid2, vec_start, vec_count,
     &   obs_modmask)

        _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. REVERSE_SIMULATION) THEN

       _BEGIN_MASTER( myThid )

        vec_start=irecglob
        vec_count=1

        err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
        err = NF_GET_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     &   active_data_t)

C Add active_var from appropriate location to data
        active_data_t = active_data_t + active_var
C Store the result on disk.
        vec_start=irec
        vec_count=1

        err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
        err = NF_PUT_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     &   active_data_t)

C Set active_var to zero
        active_var = 0. _d 0

       _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. TANGENT_SIMULATION) THEN

       _BEGIN_MASTER( myThid )

       vec_start=irecglob
       vec_count=1

       err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
       err = NF_GET_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     &  active_var)

       _END_MASTER( myThid )

      ENDIF

#endif /* ALLOW_OBSFIT */

      RETURN
      END

C     =================================================================

CBOP
C     !ROUTINE: ACTIVE_WRITE_OBS_GLOB_RL

C     !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_OBS_GLOB_RL(
     I                                     fid,
     I                                     active_num_file,
     I                                     active_var,
     I                                     irec,
     I                                     irecglob,
     I                                     theSimulationMode,
     I                                     myOptimIter,
     I                                     myThid )

C     !DESCRIPTION: 
C     ==================================================================
C     | Write an active record to an ObsFit .equi. global file
C     ==================================================================

C     !USES: 
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_OBSFIT
# include "netcdf.inc"
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif

C     !INPUT PARAMETERS:
      INTEGER  fid
      INTEGER  active_num_file
      INTEGER  irec, irecglob
      INTEGER  theSimulationMode
      INTEGER  myOptimIter
      INTEGER  myThid
      _RL      active_var
CEOP

#ifdef ALLOW_OBSFIT
C     !LOCAL VARIABLES:
      INTEGER  err, varid1, varid2
      INTEGER  vec_start, vec_count
      INTEGER  ivar, i
      _RL      active_data_t
      _RL      vec_tmp(2)

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN

       _BEGIN_MASTER( myThid )

       vec_start=irecglob
       vec_count=1

       err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
       err = NF_PUT_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     &  active_var)

       err = NF_INQ_VARID(fid, obsfit_namemask, varid2)
       err = NF_PUT_VARA_DOUBLE(fid, varid2, vec_start, vec_count,
     &  obs_modmask)

       _END_MASTER( myThid )

      ENDIF

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. REVERSE_SIMULATION) THEN

       _BEGIN_MASTER( myThid )

       vec_start=irec
       vec_count=1
       err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
       err = NF_GET_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     &  active_data_t)

C Add active_var to data.
       active_var = active_var + active_data_t
       active_data_t = 0. _d 0

       vec_start=irecglob
       vec_count=1

       err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
       err = NF_PUT_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     &  active_data_t)

       _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. TANGENT_SIMULATION) THEN

       _BEGIN_MASTER( myThid )

       vec_start=irecglob
       vec_count=1

       err = NF_INQ_VARID(fid, obsfit_nameequi, varid1)
       err = NF_PUT_VARA_DOUBLE(fid, varid1, vec_start, vec_count,
     &  active_var)

       _END_MASTER( myThid )

      ENDIF

#endif /* ALLOW_OBSFIT */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|