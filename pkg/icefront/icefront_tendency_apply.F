C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_tendency_apply.F,v 1.2 2010/01/29 01:05:27 dimitri Exp $
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

C     !LOCAL VARIABLES:
      INTEGER i, j

      IF ( applyIcefrontTendT ) THEN
       DO j=1,sNy
        DO i=1,sNx
         IF (k .LE. k_icefront(i,j,bi,bj)) THEN  
          gT(i,j,k,bi,bj) = gT(i,j,k,bi,bj)
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

C     !LOCAL VARIABLES:
      INTEGER i, j

      IF ( applyIcefrontTendS ) THEN
       DO j=1,sNy
        DO i=1,sNx
         IF (k .LE. k_icefront(i,j,bi,bj)) THEN  
          gS(i,j,k,bi,bj) = gS(i,j,k,bi,bj)
     &         + ICEFRONT_TendS(i,j,k,bi,bj)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
