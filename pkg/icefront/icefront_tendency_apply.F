C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_tendency_apply.F,v 1.1 2010/01/20 23:33:45 dimitri Exp $
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
     &                     iMin, iMax, jMin, jMax,
     &                     bi, bj, k, myTime, myThid )

C     !DESCRIPTION:
C     Add icefront tendency terms to T (theta) tendency.  Routine works
C     for one level at a time

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "ICEFRONT.h"

C     !INPUT PARAMETERS:
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj, myThid
      _RL myTime
CEOP

#ifdef ICEFRONT_TENDENCY
C     !LOCAL VARIABLES:
      INTEGER i, j

      IF ( myPa_applyTendT ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gT(i,j,k,bi,bj) = gT(i,j,k,bi,bj)
     &     + maskC(i,j,k,bi,bj) * myPa_TendScal1(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF

#endif /* ICEFRONT_TENDENCY */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ICEFRONT_TENDENCY_APPLY_S

C !INTERFACE:
      SUBROUTINE ICEFRONT_TENDENCY_APPLY_S(
     &                     iMin, iMax, jMin, jMax,
     &                     bi, bj, k, myTime, myThid )

C     !DESCRIPTION:
C     Add icefront tendency terms to S tendency.  Routine works for one
C     level at a time

C     !INPUT PARAMETERS:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "ICEFRONT.h"

C     !INPUT PARAMETERS:
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj, myThid
      _RL myTime
CEOP

#ifdef ICEFRONT_TENDENCY
C     !LOCAL VARIABLES:
      INTEGER i, j

      IF ( myPa_applyTendS ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gS(i,j,k,bi,bj) = gS(i,j,k,bi,bj)
     &     + maskC(i,j,k,bi,bj) * myPa_TendScal2(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF

#endif /* ICEFRONT_TENDENCY */

      RETURN
      END
