C $Header: /u/gcmpack/MITgcm/model/src/correction_step.F,v 1.22 2005/11/08 02:14:10 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CORRECTION_STEP
C     !INTERFACE:
      SUBROUTINE CORRECTION_STEP( bi, bj, iMin, iMax, jMin, jMax,
     I                       K, phiSurfX, phiSurfY,
     I                       myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CORRECTION_STEP                                       
C     | o Corrects the horizontal flow fields with the surface    
C     |   slope.                                                  
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     phiSurfX, phiSurfY           - Surface Potential gradient
C     bi,bj,iMin,iMax,jMin,jMax, K - Loop counters
C     myThid                       - Instance number for 
C                                    this call to S/R CORRECTION_STEP
C     myTime                       - Current simulation time for this instance.
      _RL  phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER K
      INTEGER myThid
      _RL myTime

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j             :: Loop counters
C     hxFac, hyFac    :: Tracer parameters for supressing gradients
C     hx3dFac,hy3dFac
      INTEGER i,j
      _RL hxFac,hyFac
      _RL hx3dFac,hy3dFac
CEOP

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
        uVel(i,j,k,bi,bj)=( gU(i,j,k,bi,bj)
     &       -deltaTmom*hxFac*implicSurfPress*phiSurfX(i,j)
#ifdef ALLOW_NONHYDROSTATIC
     &       -deltaTmom*hx3dFac*_recip_dxC(i,j,bi,bj)*
     &          (phi_nh(i,j,k,bi,bj)-phi_nh(i-1,j,k,bi,bj))
#endif
     &                    )*_maskW(i,j,k,bi,bj)
       ENDDO
      ENDDO

C     Step forward meridional velocity
      DO j=jMin,jMax
       DO i=iMin,iMax
        vVel(i,j,k,bi,bj)=( gV(i,j,k,bi,bj)
     &       -deltaTmom*hyFac*implicSurfPress*phiSurfY(i,j)
#ifdef ALLOW_NONHYDROSTATIC
     &       -deltaTmom*hy3dFac*_recip_dyC(i,j,bi,bj)*
     &          (phi_nh(i,j,k,bi,bj)-phi_nh(i,j-1,k,bi,bj))
#endif
     &                    )*_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
