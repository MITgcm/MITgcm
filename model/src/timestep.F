C $Header: /u/gcmpack/MITgcm/model/src/timestep.F,v 1.19 2001/02/02 21:04:48 adcroft Exp $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | S/R TIMESTEP                                             |
C     | o Step model fields forward in time                      |
C     \==========================================================/
      SUBROUTINE TIMESTEP( bi, bj, iMin, iMax, jMin, jMax,
     I                     K, phiHyd,
     I                     myIter, myThid )
      implicit none

C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine Arguments ==
C     phiHyd - Hydrostatic pressure (ocean) or geopotentiel (atmos)
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER K
      _RL     phiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr) 
      INTEGER myIter, myThid

C     == Local variables ==
      INTEGER i,j
      _RL ab15,ab05
      _RL phxFac,phyFac

C     Adams-Bashforth timestepping weights
Caja  IF (myIter .EQ. 0) THEN
Caja   ab15=1.0
Caja   ab05=0.0
Caja  ELSE
       ab15=1.5+abeps
       ab05=-0.5-abeps
Caja  ENDIF

C     Step forward zonal velocity (store in Gu)
      DO j=jMin,jMax
        DO i=iMin,iMax
          gUNm1(i,j,k,bi,bj)=uVel(i,j,k,bi,bj)
     &    +deltaTmom*(ab15*gU(i,j,k,bi,bj)+ab05*gUNm1(i,j,k,bi,bj)
#ifdef INCLUDE_CD_CODE
     &    +guCD(i,j,k,bi,bj)
#endif
     &        )*_maskW(i,j,k,bi,bj)
        ENDDO
      ENDDO

      IF (staggerTimeStep) THEN
C--   -grad Phi_Hyd has not been incorporated to gU and is added here:
        phxFac = pfFacMom*deltaTmom*recip_rhoConst
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
      DO j=jMin,jMax
        DO i=iMin,iMax
          gVNm1(i,j,k,bi,bj)=vVel(i,j,k,bi,bj)
     &    +deltaTmom*(ab15*gV(i,j,k,bi,bj)+ab05*gVNm1(i,j,k,bi,bj)
#ifdef INCLUDE_CD_CODE
     &    +gvCD(i,j,k,bi,bj)
#endif
     &        )*_maskS(i,j,k,bi,bj)
        ENDDO
      ENDDO

      IF (staggerTimeStep) THEN
C--   -grad Phi_Hyd has not been incorporated to gV and is added here:
        phyFac = pfFacMom*deltaTmom*recip_rhoConst
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
