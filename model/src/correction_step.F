C $Header: /u/gcmpack/MITgcm/model/src/correction_step.F,v 1.7 1998/08/20 18:43:11 cnh Exp $

#include "CPP_EEOPTIONS.h"

C     /==========================================================\
C     | S/R CORRECTION_STEP                                      |
C     | o Corrects the horizontal flow fields with the surface   |
C     |   slope.                                                 |
C     \==========================================================/
      SUBROUTINE CORRECTION_STEP( bi, bj, iMin, iMax, jMin, jMax,
     I                       K, etaX, etaY,
     I                       myCurrentTime, myThid )
      IMPLICIT NONE
! Common
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CG2D.h"
C     == Routine Arguments ==
C     etaX, etaY - Surface slope
C     bi,bj,iMin,iMax,jMin,jMax, K - Loop counters
C     myThid - Instance number for this call to S/R CORRECTION_STEP
C     myCurrentTime - Current simulation time for this instance.
      _RL  etaX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  etaY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER K
      INTEGER myThid
      _RL myCurrentTime

C     == Local variables ==
      INTEGER i,j
      _RL hxFac,hyFac,rRhoNil

C     On/off scaling paramters
      hxFac = pfFacMom
      hyFac = pfFacMom

C     Step forward zonal velocity
      DO j=jMin,jMax
       DO i=iMin,iMax
        uVel(i,j,k,bi,bj)=( gUNm1(i,j,k,bi,bj)
     &       -deltaTmom*hxFac*gBaro *pSurfX(i,j)
     &                    )*_maskW(i,j,k,bi,bj)
        gUNm1(i,j,k,bi,bj)=gU(i,j,k,bi,bj)
       ENDDO
      ENDDO

C     Step forward meridional velocity
      DO j=jMin,jMax
       DO i=iMin,iMax
        vVel(i,j,k,bi,bj)=( gVNm1(i,j,k,bi,bj)
     &       -deltaTmom*hyFac*rRhonil *pSurfY(i,j)
     &                    )*_maskS(i,j,k,bi,bj)
        gVNm1(i,j,k,bi,bj)=gV(i,j,k,bi,bj)
       ENDDO
      ENDDO

C     Rotate theta/gT/gTnm1
      DO j=jMin,jMax
       DO i=iMin,iMax
        theta(i,j,k,bi,bj)=gTNm1(i,j,k,bi,bj)
        gTNm1(i,j,k,bi,bj)=gT(i,j,k,bi,bj)
       ENDDO
      ENDDO

C     Rotate salt/gS/gSnm1
      DO j=jMin,jMax
       DO i=iMin,iMax
        salt(i,j,k,bi,bj)=gSNm1(i,j,k,bi,bj)
        gSNm1(i,j,k,bi,bj)=gS(i,j,k,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
