C $Header: /u/gcmpack/MITgcm/model/src/timestep.F,v 1.2 1998/04/24 02:05:42 cnh Exp $

#include "CPP_EEOPTIONS.h"

C     /==========================================================\
C     | S/R TIMESTEP                                             |
C     | o Step model fields forward in time                      |
C     \==========================================================/
      SUBROUTINE TIMESTEP( bi, bj, iMin, iMax, jMin, jMax, myThid )
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
      INTEGER myThid
C     == Local variables ==
C     pg       - Pressure gradient terms. Note cg2d_x
C                holds term in units so that lateral
C                gradient is all that is needed.
      INTEGER i,j,k
      REAL ab15,ab05
      _RL  pg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     Adams-Bashforth timestepping weights
      ab15=1.5+abeps
      ab05=-0.5-abeps

C     Zonal pressure term
      DO j=jMin,jMax
       DO i=iMin,iMax
        pg(i,j)=rDxC(i,j,bi,bj)*
     &   (cg2d_x(i,j,bi,bj)-cg2d_x(i-1,j,bi,bj))
     &   *gravity*rhonil
       ENDDO
      ENDDO
C     Step forward zonal velocity
      DO k=1,Nz
       DO j=jMin,jMax
        DO i=iMin,iMax
         uVel(i,j,k,bi,bj)=uVel(i,j,k,bi,bj)
     &    +deltaTmom*(ab15*gU(i,j,k,bi,bj)+ab05*gUNm1(i,j,k,bi,bj)
     &                -pg(i,j)/rhonil 
CcnhDebugStarts
Cdbg &                -rdxC(i,j,bi,bj)*
Cdbg &                 (pHSave(i,j,k,bi,bj)-pHSave(i-1,j,k,bi,bj))/rhonil
CcnhDebugEnds
     &    )*maskW(i,j,k,bi,bj)
         gUNm1(i,j,k,bi,bj)=gU(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO
C     Meridional pressure term
      DO j=jMin,jMax
       DO i=iMin,iMax
        pg(i,j)=rDyC(i,j,bi,bj)*
     &   (cg2d_x(i,j,bi,bj)-cg2d_x(i,j-1,bi,bj))
     &   *gravity*rhonil
       ENDDO
      ENDDO
C     Step forward meridional velocity
      DO k=1,Nz
       DO j=jMin,jMax
        DO i=iMin,iMax
         vVel(i,j,k,bi,bj)=vVel(i,j,k,bi,bj)
     &    +deltaTmom*(ab15*gV(i,j,k,bi,bj)+ab05*gVNm1(i,j,k,bi,bj)
     &                -pg(i,j)/rhonil 
CcnhDebugStarts
Cdbg &                -rdyC(i,j,bi,bj)*
Cdbg &                 (pHSave(i,j,k,bi,bj)-pHSave(i,j-1,k,bi,bj))/rhonil
CcnhDebugEnds
     &               )*maskS(i,j,k,bi,bj)
         gVNm1(i,j,k,bi,bj)=gV(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO
C     Step forward temperature
      DO k=1,Nz
       DO j=jMin,jMax
        DO i=iMin,iMax
         theta(i,j,k,bi,bj)=theta(i,j,k,bi,bj)
     &    +deltaTtracer*(ab15*gT(i,j,k,bi,bj)+ab05*gTNm1(i,j,k,bi,bj))
         gTNm1(i,j,k,bi,bj)=gT(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO

      _BARRIER
C     CALL PLOT_FIELD_XYZR8( uVel, 'TIEMSTEP.1 uVel',Nz,1,myThid)
C     CALL PLOT_FIELD_XYZR8( vVel, 'TIEMSTEP.1 vVel',Nz,1,myThid)
C     CALL PLOT_FIELD_XYZR8( theta, 'TIEMSTEP.1 theta',Nz,1,myThid)


      RETURN
      END
