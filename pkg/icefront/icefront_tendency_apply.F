C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_tendency_apply.F,v 1.4 2014/07/09 17:00:49 jmc Exp $
C $Name:  $

#include "ICEFRONT_OPTIONS.h"
C--  File icefront_tendency_apply.F: Routines to apply ICEFRONT tendencies
C--   Contents
C--   o ICEFRONT_TENDENCY_APPLY_T
C--   o ICEFRONT_TENDENCY_APPLY_S

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ICEFRONT_TENDENCY_APPLY_T

C !INTERFACE:
      SUBROUTINE ICEFRONT_TENDENCY_APPLY_T(
     U                    gT_arr,
     I                    k, bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION:
C     Add icefront tendency terms to T (theta) tendency.
C     Routine works for one level at a time.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "GRID.h"
c#include "DYNVARS.h"
#include "ICEFRONT.h"

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

      IF ( applyIcefrontTendT ) THEN
       DO j=1,sNy
        DO i=1,sNx
         IF ( k .LE. k_icefront(i,j,bi,bj) ) THEN  
          gT_arr(i,j) = gT_arr(i,j)
     &         + ICEFRONT_TendT(i,j,k,bi,bj)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ICEFRONT_TENDENCY_APPLY_S

C !INTERFACE:
      SUBROUTINE ICEFRONT_TENDENCY_APPLY_S(
     U                    gS_arr,
     I                    k, bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION:
C     Add icefront tendency terms to S tendency.
C     Routine works for one level at a time.

C     !INPUT PARAMETERS:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "GRID.h"
c#include "DYNVARS.h"
#include "ICEFRONT.h"

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

      IF ( applyIcefrontTendS ) THEN
       DO j=1,sNy
        DO i=1,sNx
         IF ( k .LE. k_icefront(i,j,bi,bj) ) THEN  
          gS_arr(i,j) = gS_arr(i,j)
     &         + ICEFRONT_TendS(i,j,k,bi,bj)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
