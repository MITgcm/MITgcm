C $Header: /u/gcmpack/MITgcm/model/src/correction_step.F,v 1.4 1998/06/16 15:20:08 adcroft Exp $

#include "CPP_EEOPTIONS.h"

C     /==========================================================\
C     | S/R CORRECTION_STEP                                      |
C     | o Corrects the horizontal flow fields with the surface   |
C     |   pressure gradient.                                     |
C     \==========================================================/
      SUBROUTINE CORRECTION_STEP( bi, bj, iMin, iMax, jMin, jMax,
     I                       K, pSurfX, pSurfY,
     I                       myThid )
      implicit none
! Common
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CG2D.h"
C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER K
      _RL  pSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  pSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid
C     == Local variables ==
      INTEGER i,j
      _RL ab15,ab05,hxFac,hyFac,rRhoNil

C     Adams-Bashforth timestepping weights
      ab15=1.5+abeps
      ab05=-0.5-abeps

C     On/off scaling paramters
      hxFac = pfFacMom
      hyFac = pfFacMom

      rRhoNil=1. / rhonil

C     Step forward zonal velocity
      DO j=jMin,jMax
       DO i=iMin,iMax
        uVel(i,j,k,bi,bj)=( gUNm1(i,j,k,bi,bj)
     &       -deltaTmom*hxFac*rRhonil *pSurfX(i,j)
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
