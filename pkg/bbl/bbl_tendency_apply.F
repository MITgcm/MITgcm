C $Header: /u/gcmpack/MITgcm/pkg/bbl/bbl_tendency_apply.F,v 1.2 2011/08/07 07:08:15 dimitri Exp $
C $Name:  $

#include "BBL_OPTIONS.h"
C--  File bbl_tendency_apply.F: Routines to apply BBL tendencies
C--   Contents
C--   o BBL_TENDENCY_APPLY_T
C--   o BBL_TENDENCY_APPLY_S

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: BBL_TENDENCY_APPLY_T

C !INTERFACE:
      SUBROUTINE BBL_TENDENCY_APPLY_T(
     &                     iMin, iMax, jMin, jMax,
     &                     bi, bj, k, myTime, myThid )

C     !DESCRIPTION:
C     Add bbl tendency terms to T (theta) tendency.  Routine works
C     for one level at a time

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "BBL.h"

C     !INPUT PARAMETERS:
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj, myThid
      _RL myTime
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j

      DO j=jMin,jMax
       DO i=iMin,iMax
         IF ( k .EQ. kLowC(i,j,bi,bj) )
     &       gT(i,j,k,bi,bj) = gT(i,j,k,bi,bj)
     &       + maskC(i,j,k,bi,bj) * bbl_TendTheta(i,j,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: BBL_TENDENCY_APPLY_S

C !INTERFACE:
      SUBROUTINE BBL_TENDENCY_APPLY_S(
     &                     iMin, iMax, jMin, jMax,
     &                     bi, bj, k, myTime, myThid )

C     !DESCRIPTION:
C     Add bbl tendency terms to S tendency.  Routine works for one
C     level at a time

C     !INPUT PARAMETERS:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "BBL.h"

C     !INPUT PARAMETERS:
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj, myThid
      _RL myTime
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j

      DO j=jMin,jMax
       DO i=iMin,iMax
        IF ( k .EQ. kLowC(i,j,bi,bj) )
     &       gS(i,j,k,bi,bj) = gS(i,j,k,bi,bj)
     &       + maskC(i,j,k,bi,bj) * bbl_TendSalt(i,j,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
