C $Header: /u/gcmpack/MITgcm/pkg/iceberg/iceberg_tendency_apply.F,v 1.4 2014/07/09 17:00:49 jmc Exp $
C $Name: checkpoint65m $

#include "ICEBERG_OPTIONS.h"
C--  File iceberg_tendency_apply.F: Routines to apply ICEBERG tendencies
C--   Contents
C--   o ICEBERG_TENDENCY_APPLY_T
C--   o ICEBERG_TENDENCY_APPLY_S

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
c#include "GRID.h"
c#include "DYNVARS.h"
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
c#include "GRID.h"
c#include "DYNVARS.h"
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
