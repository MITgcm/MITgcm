C $Header: /u/gcmpack/MITgcm/model/src/timestep.F,v 1.5 1998/05/25 20:05:55 cnh Exp $

#include "CPP_EEOPTIONS.h"

C     /==========================================================\
C     | S/R TIMESTEP                                             |
C     | o Step model fields forward in time                      |
C     \==========================================================/
      SUBROUTINE TIMESTEP( bi, bj, iMin, iMax, jMin, jMax,
     I                     K, pSurfX, pSurfY,
     I                     myThid )
      implicit none
! Common
#include "SIZE.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "CG2D.h"
C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER K
      _RL  pSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  pSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid
C     == Local variables ==
      INTEGER i,j
      REAL ab15,ab05
      _RL  hxFac, hyFac

C     Adams-Bashforth timestepping weights
      ab15=1.5+abeps
      ab05=-0.5-abeps

C     On/off scaling paramters
      hxFac = pfFacMom
      hyFac = pfFacMom

C     Step forward zonal velocity
       DO j=jMin,jMax
        DO i=iMin,iMax
         uVel(i,j,k,bi,bj)=uVel(i,j,k,bi,bj)
     &    +deltaTmom*(ab15*gU(i,j,k,bi,bj)+ab05*gUNm1(i,j,k,bi,bj)
     &                -hxFac*pSurfX(i,j)/rhonil 
     &    )*maskW(i,j,k,bi,bj)
         gUNm1(i,j,k,bi,bj)=gU(i,j,k,bi,bj)
        ENDDO
       ENDDO
C     Step forward meridional velocity
       DO j=jMin,jMax
        DO i=iMin,iMax
         vVel(i,j,k,bi,bj)=vVel(i,j,k,bi,bj)
     &    +deltaTmom*(ab15*gV(i,j,k,bi,bj)+ab05*gVNm1(i,j,k,bi,bj)
     &                -hyFac*pSurfY(i,j)/rhonil 
     &               )*maskS(i,j,k,bi,bj)
         gVNm1(i,j,k,bi,bj)=gV(i,j,k,bi,bj)
        ENDDO
       ENDDO
C     Step forward temperature
       DO j=jMin,jMax
        DO i=iMin,iMax
         theta(i,j,k,bi,bj)=theta(i,j,k,bi,bj)
     &    +deltaTtracer*(ab15*gT(i,j,k,bi,bj)+ab05*gTNm1(i,j,k,bi,bj))
         gTNm1(i,j,k,bi,bj)=gT(i,j,k,bi,bj)
        ENDDO
       ENDDO

      _BARRIER
C     CALL PLOT_FIELD_XYZR8( uVel, 'TIEMSTEP.1 uVel',Nz,1,myThid)
C     CALL PLOT_FIELD_XYZR8( vVel, 'TIEMSTEP.1 vVel',Nz,1,myThid)
C     CALL PLOT_FIELD_XYZR8( theta, 'TIEMSTEP.1 theta',Nz,1,myThid)


      RETURN
      END
