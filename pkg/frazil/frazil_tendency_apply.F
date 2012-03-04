C $Header: /u/gcmpack/MITgcm/pkg/frazil/frazil_tendency_apply.F,v 1.3 2012/03/04 23:11:20 dimitri Exp $
C $Name:  $

#include "FRAZIL_OPTIONS.h"
C--  File frazil_tendency_apply.F: Routines to apply FRAZIL tendencies
C--   Contents
C--   o FRAZIL_TENDENCY_APPLY_T

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0

C !INTERFACE:
      SUBROUTINE FRAZIL_TENDENCY_APPLY_T(
     I                     iMin, iMax, jMin, jMax,
     I                     bi, bj, k, myTime, myThid )

C     !DESCRIPTION:
C     Add frazil tendency terms to T (theta) tendency.
C     Routine works for one level at a time.

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_FRAZIL
# include "FRAZIL.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: current time in simulation
C     myThid :: my Thread Id number
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj
      _RL     myTime
      INTEGER myThid
CEOP

#ifdef ALLOW_FRAZIL

C     !LOCAL VARIABLES:
      INTEGER i, j

      DO j=jMin,jMax
       DO i=iMin,iMax
        gT(i,j,k,bi,bj) = gT(i,j,k,bi,bj)
     &       + FrazilForcingT(i,j,k,bi,bj)
     &       * recip_Cp * mass2rUnit
     &       * recip_drF(k) * _recip_hFacC(i,j,k,bi,bj)
       ENDDO
      ENDDO

#endif /* ALLOW_FRAZIL */

      RETURN
      END
