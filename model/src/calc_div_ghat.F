C $Header: /u/gcmpack/MITgcm/model/src/calc_div_ghat.F,v 1.13 2001/03/06 16:51:02 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | S/R CALC_DIV_GHAT                                        |
C     | o Form the right hand-side of the surface pressure eqn.  |
C     |==========================================================|
C     \==========================================================/
      SUBROUTINE CALC_DIV_GHAT( 
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     I        xA,yA,
     U        cg2d_b,
     I        myThid)

      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "CG3D.h"
#endif

C     == Routine arguments ==
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     k                              - Index of layer.
C     xA, yA                         - Cell face areas
C     cg2d_b - Conjugate Gradient 2-D solver : Right-hand side vector 
C     myThid - Instance number for this call of CALC_DIV_GHAT
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER K
      _RS xA (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL cg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j
      _RL pf (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C--   Pressure equation source term
C     Term is the vertical integral of the divergence of the 
C     time tendency terms along with a relaxation term that
C     pulls div(U) + dh/dt back toward zero.

      IF (implicDiv2Dflow.EQ.1.) then
C     Fully Implicit treatment of the Barotropic Flow Divergence
        DO j=1,sNy
         DO i=1,sNx+1
          pf(i,j) = xA(i,j)*gUNm1(i,j,k,bi,bj) / deltaTmom
         ENDDO
        ENDDO
      ELSE
C     Explicit+Implicit part of the Barotropic Flow Divergence
C      => Filtering of uVel,vVel is necessary
#ifdef ALLOW_ZONAL_FILT
        IF (zonal_filt_lat.LT.90.) THEN 
          CALL ZONAL_FILTER(
     &      uVel, hFacW, 1-1, sNy+1, k, k, bi, bj, 1, myThid)
          CALL ZONAL_FILTER(
     &      vVel, hFacS, 1-1, sNy+1, k, k, bi, bj, 2, myThid)
        ENDIF
#endif
        DO j=1,sNy
         DO i=1,sNx+1
          pf(i,j) = ( implicDiv2Dflow * gUNm1(i,j,k,bi,bj)
     &          + (1.-implicDiv2Dflow) * uVel(i,j,k,bi,bj)
     &               ) * xA(i,j) / deltaTmom
         ENDDO
        ENDDO
      ENDIF
      DO j=1,sNy
       DO i=1,sNx
        cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj) +
     &   pf(i+1,j)-pf(i,j)
       ENDDO
      ENDDO

#ifdef ALLOW_NONHYDROSTATIC
      IF (nonHydrostatic) THEN
       DO j=1,sNy
        DO i=1,sNx
         cg3d_b(i,j,k,bi,bj) = pf(i+1,j)-pf(i,j)
        ENDDO
       ENDDO
      ENDIF
#endif

      IF (implicDiv2Dflow.EQ.1.) then
C     Fully Implicit treatment of the Barotropic Flow Divergence
        DO j=1,sNy+1
         DO i=1,sNx
          pf(i,j) = yA(i,j)*gVNm1(i,j,k,bi,bj) / deltatmom
         ENDDO
        ENDDO
      ELSE
C     Explicit+Implicit part of the Barotropic Flow Divergence
        DO j=1,sNy+1
         DO i=1,sNx
          pf(i,j) = ( implicDiv2Dflow * gVNm1(i,j,k,bi,bj)
     &          + (1.-implicDiv2Dflow) * vVel(i,j,k,bi,bj)
     &               ) * yA(i,j) / deltaTmom
         ENDDO
        ENDDO
      ENDIF
      DO j=1,sNy
       DO i=1,sNx
        cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj) +
     &   pf(i,j+1)-pf(i,j)
       ENDDO
      ENDDO

#ifdef ALLOW_NONHYDROSTATIC
      IF (nonHydrostatic) THEN
       DO j=1,sNy
        DO i=1,sNx
         cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj) +
     &    pf(i,j+1)-pf(i,j)
        ENDDO
       ENDDO
      ENDIF
#endif

      RETURN
      END
