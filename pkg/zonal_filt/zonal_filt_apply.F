C $Header: /u/gcmpack/MITgcm/pkg/zonal_filt/Attic/zonal_filt_apply.F,v 1.3 2001/02/04 14:38:51 cnh Exp $
C $Name:  $

#include "ZONAL_FILT_OPTIONS.h"

      SUBROUTINE ZONAL_FILT_APPLY( 
     U           uFld, vFld, tFld, sFld,
     I           myThid )
C     /==========================================================\
C     | S/R ZONAL_FILT_APPLY                                     |
C     | o Apply FFT filter to a latitude circle.                 |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_ZONAL_FILT

C     == Local data ==
      INTEGER K,bi,bj

      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

      DO k=1,Nr
        CALL ZONAL_FILTER(
     &    uFld, hFacW, 1-1, sNy+1, k, k, bi, bj, 1, myThid)
        CALL ZONAL_FILTER(
     &    vFld, hFacS, 1-1, sNy+1, k, k, bi, bj, 2, myThid)
        IF ( tempStepping ) CALL ZONAL_FILTER(
     &    tFld, hFacC, 1, sNy, k, k, bi, bj, 1, myThid)
        IF ( saltStepping ) CALL ZONAL_FILTER(
     &    sFld, hFacC, 1, sNy, k, k, bi, bj, 1, myThid)
      ENDDO

      ENDDO
      ENDDO

#endif /* ALLOW_ZONAL_FILT */

      RETURN
      END
