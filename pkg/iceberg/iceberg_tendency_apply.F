#include "ICEBERG_OPTIONS.h"
C--  File iceberg_tendency_apply.F: Routines to apply ICEBERG tendencies
C--   Contents
C--   o ICEBERG_TENDENCY_APPLY_T
C--   o ICEBERG_TENDENCY_APPLY_S
C--   o ICEBERG_TENDENCY_APPLY_PTRACERS

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ICEBERG_TENDENCY_APPLY_T

C !INTERFACE:
      SUBROUTINE ICEBERG_TENDENCY_APPLY_T(
     U                    gT_arr,
     I                    k, bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION:
C     Add iceberg tendency terms to T (theta) tendency.
C     Routine works for one level at a time.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#endif /* ALLOW_PTRACERS */
#include "ICEBERG_SIZE.h"
#include "ICEBERG.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gT_arr    :: the tendency array
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gT_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j

       DO j=1,sNy
        DO i=1,sNx
         IF ( brg_OpenFraction(I,J,K,bi,bj) .GT. 0. _d 0 ) THEN
          gT_arr(i,j) = gT_arr(i,j) + brg_TendT3D(i,j,k,bi,bj)
         ENDIF
        ENDDO
       ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ICEBERG_TENDENCY_APPLY_S

C !INTERFACE:
      SUBROUTINE ICEBERG_TENDENCY_APPLY_S(
     U                    gS_arr,
     I                    k, bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION:
C     Add iceberg tendency terms to S tendency.
C     Routine works for one level at a time.

C     !INPUT PARAMETERS:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#endif /* ALLOW_PTRACERS */
#include "ICEBERG_SIZE.h"
#include "ICEBERG.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gS_arr    :: the tendency array
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gS_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid  
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j

       DO j=1,sNy
        DO i=1,sNx
         IF ( brg_OpenFraction(I,J,K,bi,bj) .GT. 0. _d 0 ) THEN
          gS_arr(i,j) = gS_arr(i,j) + brg_TendS3D(i,j,k,bi,bj)
         ENDIF
        ENDDO
       ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ICEBERG_TENDENCY_APPLY_PTRACERS
#ifdef ALLOW_PTRACERS
C !INTERFACE:
      SUBROUTINE ICEBERG_TENDENCY_APPLY_PTRACERS(
     U                    gPtracer,
     I                    k, bi, bj, iTracer, myTime, myIter, myThid )
C     !DESCRIPTION:
C     Add iceberg tendency terms to ptracer tendency.
C     Routine works for one level at a time.
C     !INPUT PARAMETERS:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS_SIZE.h"
#include "ICEBERG_SIZE.h"
#include "ICEBERG_OPTIONS.h"
#include "ICEBERG.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gPtracer  :: the tendency array
C     iTracer   :: tracer number
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gPtracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k, bi, bj, iTracer
      _RL     myTime
      INTEGER myIter
      INTEGER myThid  
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j
     
      IF(brg_useInputPtracers.EQ.1) THEN
       DO j=1,sNy
         DO i=1,sNx
             gPtracer(i,j) = gPtracer(i,j)
     &         + brg_ptr_addMass3D(I,J,k,bi,bj,iTracer)*mass2rUnit
     &           *recip_rA(i,j,bi,bj)
     &           *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
#endif /* ALLOW_PTRACERS */