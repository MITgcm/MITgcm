C $Header: /u/gcmpack/MITgcm/model/src/timestep.F,v 1.23 2001/03/08 20:27:33 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | S/R TIMESTEP                                             |
C     | o Step model fields forward in time                      |
C     \==========================================================/
      SUBROUTINE TIMESTEP( bi, bj, iMin, iMax, jMin, jMax, K,
     I                     phiHyd, phiSurfX, phiSurfY,
     I                     myIter, myThid )
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine Arguments ==
C     phiHyd    - Hydrostatic Potential (ocean: pressure/rho) 
C                                       (atmos: geopotentiel)
C     phiSurfX, - gradient of Surface potentiel (Pressure/rho, ocean)
C     phiSurfY             or geopotentiel (atmos) in X and Y direction
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER K
      _RL     phiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr) 
      _RL     phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myIter, myThid

C     == Local variables ==
      INTEGER i,j
      _RL ab15,ab05
      _RL phxFac,phyFac, psFac

C     Adams-Bashforth timestepping weights
Caja  IF (myIter .EQ. 0) THEN
Caja   ab15=1.0
Caja   ab05=0.0
Caja  ELSE
       ab15=1.5+abeps
       ab05=-0.5-abeps
Caja  ENDIF

C     Step forward zonal velocity (store in Gu)
      psFac = pfFacMom*(1. _d 0 - implicSurfPress)
      DO j=jMin,jMax
        DO i=iMin,iMax
          gUNm1(i,j,k,bi,bj)=uVel(i,j,k,bi,bj) + deltaTmom * (
     &       ab15*gU(i,j,k,bi,bj)+ab05*gUNm1(i,j,k,bi,bj)
     &     - psFac*phiSurfX(i,j)
#ifdef INCLUDE_CD_CODE
     &     + guCD(i,j,k,bi,bj)
#endif
     &        )*_maskW(i,j,k,bi,bj)
        ENDDO
      ENDDO

      IF (staggerTimeStep) THEN
C--   -grad Phi_Hyd has not been incorporated to gU and is added here:
        phxFac = pfFacMom*deltaTmom
        DO j=jMin,jMax
          DO i=iMin,iMax
            gUNm1(i,j,k,bi,bj)=gUNm1(i,j,k,bi,bj)
     &       - _recip_dxC(i,j,bi,bj)
     &         *(phiHyd(i,j,k)-phiHyd(i-1,j,k))*phxFac
     &         *_maskW(i,j,k,bi,bj)
          ENDDO
        ENDDO
      ENDIF

C     Step forward meridional velocity (store in Gv)
      psFac = pfFacMom*(1. _d 0 - implicSurfPress)
      DO j=jMin,jMax
        DO i=iMin,iMax
          gVNm1(i,j,k,bi,bj)=vVel(i,j,k,bi,bj) + deltaTmom * (
     &       ab15*gV(i,j,k,bi,bj)+ab05*gVNm1(i,j,k,bi,bj)
     &     - psFac*phiSurfY(i,j)
#ifdef INCLUDE_CD_CODE
     &     + gvCD(i,j,k,bi,bj)
#endif
     &        )*_maskS(i,j,k,bi,bj)
        ENDDO
      ENDDO

      IF (staggerTimeStep) THEN
C--   -grad Phi_Hyd has not been incorporated to gV and is added here:
        phyFac = pfFacMom*deltaTmom
        DO j=jMin,jMax
          DO i=iMin,iMax
            gVNm1(i,j,k,bi,bj)=gVNm1(i,j,k,bi,bj)
     &       - _recip_dyC(i,j,bi,bj)
     &         *(phiHyd(i,j,k)-phiHyd(i,j-1,k))*phyFac                      
     &         *_maskS(i,j,k,bi,bj)
          ENDDO
        ENDDO
      ENDIF

      RETURN
      END
