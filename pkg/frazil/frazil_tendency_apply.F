C $Header: /u/gcmpack/MITgcm/pkg/frazil/frazil_tendency_apply.F,v 1.5 2014/07/09 17:00:49 jmc Exp $
C $Name:  $

#include "FRAZIL_OPTIONS.h"
C--  File frazil_tendency_apply.F: Routines to apply FRAZIL tendencies
C--   Contents
C--   o FRAZIL_TENDENCY_APPLY_T

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0

C !INTERFACE:
      SUBROUTINE FRAZIL_TENDENCY_APPLY_T(
     U                     gT_arr,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     myTime, myIter, myThid )

C     !DESCRIPTION:
C     Add frazil tendency terms to T (theta) tendency.
C     Routine works for one level at a time.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c#include "DYNVARS.h"
#ifdef ALLOW_FRAZIL
# include "FRAZIL.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     gT_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gT_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_FRAZIL

C     !LOCAL VARIABLES:
      INTEGER i, j
      _RL recip_Cp

      recip_Cp = 1. _d 0 / HeatCapacity_Cp
      DO j=jMin,jMax
       DO i=iMin,iMax
        gT_arr(i,j) = gT_arr(i,j)
     &       + FrazilForcingT(i,j,k,bi,bj)
     &       * recip_Cp * mass2rUnit
     &       * recip_drF(k) * _recip_hFacC(i,j,k,bi,bj)
       ENDDO
      ENDDO

#endif /* ALLOW_FRAZIL */

      RETURN
      END
