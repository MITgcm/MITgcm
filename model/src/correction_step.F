C $Header: /u/gcmpack/MITgcm/model/src/correction_step.F,v 1.17 2001/03/08 20:35:39 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | S/R CORRECTION_STEP                                      |
C     | o Corrects the horizontal flow fields with the surface   |
C     |   slope.                                                 |
C     \==========================================================/
      SUBROUTINE CORRECTION_STEP( bi, bj, iMin, iMax, jMin, jMax,
     I                       K, phiSurfX, phiSurfY,
     I                       myCurrentTime, myThid )
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "CG3D.h"
#endif
C     == Routine Arguments ==
C     phiSurfX, phiSurfY           - Surface Potential gradient
C     bi,bj,iMin,iMax,jMin,jMax, K - Loop counters
C     myThid                       - Instance number for 
C                                    this call to S/R CORRECTION_STEP
C     myCurrentTime                - Current simulation time for this instance.
      _RL  phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER K
      INTEGER myThid
      _RL myCurrentTime

C     == Local variables ==
      INTEGER i,j
      _RL hxFac,hyFac
      _RL hx3dFac,hy3dFac

C     On/off scaling paramters
      hxFac = pfFacMom
      hyFac = pfFacMom
      IF ( nonHydrostatic ) THEN
        hx3dFac = pfFacMom
        hy3dFac = pfFacMom
      ELSE
        hx3dFac = 0.
        hy3dFac = 0.
      ENDIF

C     Step forward zonal velocity
      DO j=jMin,jMax
       DO i=iMin,iMax
        uVel(i,j,k,bi,bj)=( gUNm1(i,j,k,bi,bj)
     &       -deltaTmom*hxFac*implicSurfPress*phiSurfX(i,j)
#ifdef ALLOW_NONHYDROSTATIC
     &       -deltaTmom*hx3dFac*_recip_dxC(i,j,bi,bj)*
     &          (cg3d_x(i,j,k,bi,bj)-cg3d_x(i-1,j,k,bi,bj))
#endif
     &                    )*_maskW(i,j,k,bi,bj)
        gUNm1(i,j,k,bi,bj)=gU(i,j,k,bi,bj)
       ENDDO
      ENDDO

C     Step forward meridional velocity
      DO j=jMin,jMax
       DO i=iMin,iMax
        vVel(i,j,k,bi,bj)=( gVNm1(i,j,k,bi,bj)
     &       -deltaTmom*hyFac*implicSurfPress*phiSurfY(i,j)
#ifdef ALLOW_NONHYDROSTATIC
     &       -deltaTmom*hy3dFac*_recip_dyC(i,j,bi,bj)*
     &          (cg3d_x(i,j,k,bi,bj)-cg3d_x(i,j-1,k,bi,bj))
#endif
     &                    )*_maskS(i,j,k,bi,bj)
        gVNm1(i,j,k,bi,bj)=gV(i,j,k,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
