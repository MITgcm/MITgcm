C $Header: /u/gcmpack/MITgcm/pkg/frazil/frazil_tendency_apply.F,v 1.1 2012/03/02 01:45:23 dimitri Exp $
C $Name:  $

#include "FRAZIL_OPTIONS.h"
C--  File frazil_tendency_apply.F: Routines to apply FRAZIL tendencies
C--   Contents
C--   o FRAZIL_TENDENCY_APPLY_T

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0

C !INTERFACE:
      SUBROUTINE FRAZIL_TENDENCY_APPLY_T(
     &                     iMin, iMax, jMin, jMax,
     &                     bi, bj, k, myTime, myThid )

C     !DESCRIPTION:
C     Add frazil tendency terms to T (theta) tendency.  Routine works
C     for one level at a time

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "FRAZIL.h"

C     !INPUT PARAMETERS:
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj, myThid
      _RL myTime
CEOP

#ifdef ALLOW_FRAZIL
C     !LOCAL VARIABLES:
      INTEGER i, j

      IF ( myPa_applyTendT ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gT(i,j,k,bi,bj) = gT(i,j,k,bi,bj)
     &     + maskC(i,j,k,bi,bj) * frazil_TendT(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_FRAZIL */

      RETURN
      END
