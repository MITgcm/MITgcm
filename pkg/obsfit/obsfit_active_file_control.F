#include "OBSFIT_OPTIONS.h"
C     obsfit_active_file_control.F:  Routines that handle the I/O of
C                                    active variables for adjoint
C                                    calculations
C
C     Routines
C     o  active_read_obs_tile_rl  - Read an active record from tiled file
C                                   fwd-mode only: including a mask
C     o  active_write_obs_tile_rl - Write an active record to tiled file
C                                   fwd-mode only: including a mask
C     o  active_read_obs_glob_rl  - Read an active record from global file
C                                   fwd-mode only: including a mask
C     o  active_write_obs_glob_rl - Write an active record to global file.
C                                   fwd-mode only: including a mask

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
      LOGICAL  lAdInit
      INTEGER  irec, irecglob, jrec
      INTEGER  theSimulationMode
      INTEGER  myOptimIter
      INTEGER  bi, bj, myThid
C     !IOUTPUT PARAMETERS:
      _RL      active_var
CEOP

#ifdef ALLOW_OBSFIT
C     !LOCAL VARIABLES:
      INTEGER  err, varID1, varID2
      INTEGER  vec_start, vec_count
      _RL      active_data_t
      _RL      vec_tmp(2)

       IF ( obsfitDoNcOutput ) THEN

        vec_start = irec
        vec_count = 1

        err = NF_INQ_VARID( fid, obsfit_nameequi, varID1 )
        CALL OBSFIT_NF_ERROR(
     &       'ACTIVE_READ: NF_INQ_VARID obsfit_nameequi',
     &       err,bi,bj,myThid )

       ELSE

        jrec = (irec-1)*2

       ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF ( theSimulationMode .EQ. FORWARD_SIMULATION ) THEN

       _BEGIN_MASTER( myThid )

       IF ( obsfitDoNcOutput ) THEN

        err = NF_GET_VARA_DOUBLE( fid, varID1, vec_start, vec_count,
     &        active_var )
        CALL OBSFIT_NF_ERROR(
     &         'ACTIVE_READ: NF_GET_VARA_DOUBLE active_var',
     &         err,bi,bj,myThid )

        err = NF_INQ_VARID( fid, obsfit_namemask, varID2 )
        CALL OBSFIT_NF_ERROR(
     &         'ACTIVE_READ: NF_INQ_VARID obsfit_namemask',
     &         err,bi,bj,myThid )

        err = NF_GET_VARA_DOUBLE( fid, varID2, vec_start, vec_count,
     &        sample_modmask(bi,bj) )
        CALL OBSFIT_NF_ERROR(
     &         'ACTIVE_READ: NF_GET_VARA_DOUBLE sample_modmask',
     &         err,bi,bj,myThid )

       ELSE

        READ( fid, rec=jrec+1 ) vec_tmp
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8( 2, vec_tmp )
#endif
        active_var = vec_tmp(1)
        READ( fid, rec=jrec+2 ) vec_tmp
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8( 2, vec_tmp )
#endif
        sample_modmask(bi,bj) = vec_tmp(1)

       ENDIF

       _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF ( theSimulationMode .EQ. REVERSE_SIMULATION ) THEN

       _BEGIN_MASTER( myThid )

       IF ( obsfitDoNcOutput) THEN

        err = NF_GET_VARA_DOUBLE( fid, varID1, vec_start, vec_count,
     &        active_data_t )
        CALL OBSFIT_NF_ERROR(
     &         'ACTIVE_READ: NF_GET_VARA_DOUBLE AD active_data_t',
     &         err,bi,bj,myThid )

C Add active_var from appropriate location to data
        active_data_t = active_data_t + active_var

C Store the result on disk
        err = NF_PUT_VARA_DOUBLE( fid, varID1, vec_start, vec_count,
     &        active_data_t )
        CALL OBSFIT_NF_ERROR(
     &         'ACTIVE_READ: NF_PUT_VARA_DOUBLE AD active_data_t',
     &         err,bi,bj,myThid )

C Set active_var to zero
        active_var = 0. _d 0

       ELSE

        READ( fid, rec=jrec+1 ) vec_tmp
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8( 2, vec_tmp )
#endif
        active_data_t = vec_tmp(1)

C Add active_var from appropriate location to data
        active_data_t = active_data_t + active_var

C Store the result on disk
        vec_tmp(1) = active_data_t
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8( 2, vec_tmp )
#endif
        WRITE( fid, rec=jrec+1 ) vec_tmp

C Set active_var to zero
        active_var = 0. _d 0

       ENDIF

       _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF ( theSimulationMode .EQ. TANGENT_SIMULATION ) THEN

       _BEGIN_MASTER( myThid )

       IF ( obsfitDoNcOutput ) THEN

        err = NF_GET_VARA_DOUBLE( fid, varID1, vec_start, vec_count,
     &        active_var )
        CALL OBSFIT_NF_ERROR(
     &         'ACTIVE_READ: NF_GET_VARA_DOUBLE TL active_var',
     &         err,bi,bj,myThid )
       ELSE

        READ( fid, rec=jrec+1 ) vec_tmp
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8( 2, vec_tmp )
#endif
        active_var = vec_tmp(1)

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
      INTEGER  irec, irecglob, jrec
      INTEGER  theSimulationMode
      INTEGER  myOptimIter
      INTEGER  bi, bj, myThid
      _RL      active_var

#ifdef ALLOW_OBSFIT
C     !LOCAL VARIABLES:
      INTEGER  err, varID1, varID2, varID3
      INTEGER  vec_start, vec_count
      _RL      active_data_t
      _RL      vec_tmp(2)
CEOP

      IF ( obsfitDoNcOutput ) THEN

        vec_start = irec
        vec_count = 1

        err = NF_INQ_VARID( fid, obsfit_nameequi, varID1 )
        CALL OBSFIT_NF_ERROR(
     &       'ACTIVE_WRITE: NF_INQ_VARID obsfit_nameequi',
     &       err,bi,bj,myThid )
      ELSE

        jrec = (irec-1)*2

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF ( theSimulationMode .EQ. FORWARD_SIMULATION ) THEN

       _BEGIN_MASTER( myThid )

       IF ( obsfitDoNcOutput ) THEN

        err = NF_PUT_VARA_DOUBLE( fid, varID1, vec_start, vec_count,
     &        active_var )
        CALL OBSFIT_NF_ERROR(
     &       'ACTIVE_WRITE: NF_PUT_VARA_DOUBLE active_var',
     &       err,bi,bj,myThid )

        err = NF_INQ_VARID( fid, obsfit_namemask, varID2 )
        CALL OBSFIT_NF_ERROR(
     &         'ACTIVE_WRITE: NF_INQ_VARID obsfit_namemask',
     &         err,bi,bj,myThid )
        err = NF_PUT_VARA_DOUBLE( fid, varID2, vec_start, vec_count,
     &        sample_modmask(bi,bj) )
        CALL OBSFIT_NF_ERROR(
     &         'ACTIVE_WRITE: NF_PUT_VARA_DOUBLE sample_modmask',
     &         err,bi,bj,myThid )

        err = NF_INQ_VARID( fid, 'sample_ind_glob', varID3 )
        CALL OBSFIT_NF_ERROR(
     &         'ACTIVE_WRITE: NF_INQ_VARID sample_ind_glob',
     &         err,bi,bj,myThid )
        err = NF_PUT_VAR1_INT( fid, varID3, vec_start, irecglob )
        CALL OBSFIT_NF_ERROR(
     &         'ACTIVE_WRITE: NF_PUT_VAR1_INT irecglob',
     &         err,bi,bj,myThid )

       ELSE

        vec_tmp(1) = active_var
        vec_tmp(2) = irecglob
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8( 2, vec_tmp )
#endif
        WRITE( fid, rec= jrec+1 ) vec_tmp
        vec_tmp(1) = sample_modmask(bi,bj)
        vec_tmp(2) = irecglob
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8( 2, vec_tmp )
#endif
        WRITE( fid, rec= jrec+2 ) vec_tmp

       ENDIF

       _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF ( theSimulationMode .EQ. REVERSE_SIMULATION ) THEN

       _BEGIN_MASTER( myThid )

       IF ( obsfitDoNcOutput ) THEN

        err = NF_GET_VARA_DOUBLE( fid, varID1, vec_start, vec_count,
     &        active_data_t )
        CALL OBSFIT_NF_ERROR(
     &       'ACTIVE_WRITE: NF_GET_VARA_DOUBLE active_data_t',
     &       err,bi,bj,myThid )

C     Add active_var to data.
        active_var = active_var + active_data_t
        active_data_t = 0. _d 0

        err = NF_PUT_VARA_DOUBLE(fid, varID1, vec_start, vec_count,
     &        active_data_t )
        CALL OBSFIT_NF_ERROR(
     &         'ACTIVE_WRITE: NF_PUT_VARA_DOUBLE active_data_t',
     &         err,bi,bj,myThid )

       ELSE

        READ( fid, rec=jrec+1 ) vec_tmp
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8( 2, vec_tmp )
#endif
        active_data_t = vec_tmp(1)

C Add active_var from appropriate location to data.
        active_var = active_var + active_data_t
        active_data_t = 0. _d 0

C Store the result on disk.
        vec_tmp(1) = active_data_t
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8( 2, vec_tmp )
#endif
        WRITE( fid, rec=jrec+1 ) vec_tmp

       ENDIF

       _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF ( theSimulationMode .EQ. TANGENT_SIMULATION ) THEN

       _BEGIN_MASTER( myThid )

       IF ( obsfitDoNcOutput ) THEN

        err = NF_PUT_VARA_DOUBLE(fid, varID1, vec_start, vec_count,
     &        active_var )
          CALL OBSFIT_NF_ERROR(
     &         'ACTIVE_WRITE: NF_PUT_VARA_DOUBLE TL active_var',
     &         err,bi,bj,myThid )
       ELSE

        vec_tmp(1) = active_var
        vec_tmp(2) = irecglob
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8( 2, vec_tmp )
#endif
        WRITE( fid, rec=jrec+1 ) vec_tmp

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
     O                                  active_mask,
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
      _RL      active_mask
      logical  lAdInit

#ifdef ALLOW_OBSFIT
C     !LOCAL VARIABLES:
      INTEGER  err, varID1, varID2
      INTEGER  vec_start, vec_count
      _RL      active_data_t
CEOP

      vec_start = irecglob
      vec_count = 1

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF ( theSimulationMode .EQ. FORWARD_SIMULATION ) THEN

       _BEGIN_MASTER( myThid )

        err = NF_INQ_VARID( fid, obsfit_nameequi, varID1 )
        err = NF_GET_VARA_DOUBLE( fid, varID1, vec_start, vec_count,
     &   active_var )

        err = NF_INQ_VARID( fid, obsfit_namemask, varID2 )
        err = NF_GET_VARA_DOUBLE( fid, varID2, vec_start, vec_count,
     &   active_mask )

        _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF ( theSimulationMode .EQ. REVERSE_SIMULATION ) THEN

       _BEGIN_MASTER( myThid )

       err = NF_INQ_VARID( fid, obsfit_nameequi, varID1 )
       err = NF_GET_VARA_DOUBLE( fid, varID1, vec_start, vec_count,
     &       active_data_t )

C Add active_var from appropriate location to data
       active_data_t = active_data_t + active_var
C Store the result on disk.
CAV !!!!!!
ccc       vec_start = irec
ccc       vec_count = 1

       err = NF_INQ_VARID( fid, obsfit_nameequi, varID1 )
       err = NF_PUT_VARA_DOUBLE( fid, varID1, vec_start, vec_count,
     &       active_data_t )

C Set active_var to zero
       active_var = 0. _d 0

       _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF ( theSimulationMode .EQ. TANGENT_SIMULATION ) THEN

       _BEGIN_MASTER( myThid )

       err = NF_INQ_VARID( fid, obsfit_nameequi, varID1 )
       err = NF_GET_VARA_DOUBLE( fid, varID1, vec_start, vec_count,
     &       active_var )

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
     I                                     active_mask,
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
      _RL      active_mask

#ifdef ALLOW_OBSFIT
C     !LOCAL VARIABLES:
      INTEGER  err, varID1, varID2
      INTEGER  vec_start, vec_count
      _RL      active_data_t
CEOP

      vec_start = irecglob
      vec_count = 1

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF ( theSimulationMode .EQ. FORWARD_SIMULATION ) THEN

       _BEGIN_MASTER( myThid )

       err = NF_INQ_VARID( fid, obsfit_nameequi, varID1 )
       err = NF_PUT_VARA_DOUBLE( fid, varID1, vec_start, vec_count,
     &       active_var )

       err = NF_INQ_VARID( fid, obsfit_namemask, varID2 )
       err = NF_PUT_VARA_DOUBLE( fid, varID2, vec_start, vec_count,
     &       active_mask )

       _END_MASTER( myThid )

      ENDIF

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF ( theSimulationMode .EQ. REVERSE_SIMULATION ) THEN

       _BEGIN_MASTER( myThid )

cav       vec_start = irec
cav       vec_count = 1
       err = NF_INQ_VARID( fid, obsfit_nameequi, varID1 )
       err = NF_GET_VARA_DOUBLE( fid, varID1, vec_start, vec_count,
     &       active_data_t )

C Add active_var to data.
       active_var = active_var + active_data_t
       active_data_t = 0. _d 0

cav       vec_start = irecglob
cav       vec_count = 1

       err = NF_INQ_VARID( fid, obsfit_nameequi, varID1 )
       err = NF_PUT_VARA_DOUBLE( fid, varID1, vec_start, vec_count,
     &       active_data_t )

       _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF ( theSimulationMode .EQ. TANGENT_SIMULATION ) THEN

       _BEGIN_MASTER( myThid )

       err = NF_INQ_VARID( fid, obsfit_nameequi, varID1 )
       err = NF_PUT_VARA_DOUBLE( fid, varID1, vec_start, vec_count,
     &       active_var )

       _END_MASTER( myThid )

      ENDIF

#endif /* ALLOW_OBSFIT */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
